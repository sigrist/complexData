# Carrega localmente as medidas de temperatura
#
# Returns:
#  Data frame com medidas de temperatura obtidas do cepagri
carregaArquivos <- function() {
  con <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv") 
  cpa <- read.csv(con, header = FALSE, sep = ";",col.names = c("horario","temperatura","vento","umidade","sensacao"), as.is = TRUE)  
}


# Gera uma serie temporal para cada dia de medidas. Uma serie contem 24 medidas
#
# Args:
#  cpa: Data frame com medidas de temperatura obtidas do cepagri
# Returns:
#  matriz de 24 colunas e N linhas, onde as colunas sao as horas do dia e as linhas sao os dias
geraSeries <- function (cpa) {
  # Transforma horario em formato de data para facilitar manipulacao
  horarios = strptime(cpa[["horario"]], "%d/%m/%Y-%H:%M")
  
  # Transforma temperaturas em numerico
  temperaturas = as.numeric(cpa[["temperatura"]])
  
  # Obtem a media de cada hora, assim cada serie diaria tera 24 horas
  datasComHoraCheia = paste(as.Date(horarios), formatC(horarios[["hour"]], width = 2, flag = "0"))
  temperaturasPorHora = tapply(temperaturas, datasComHoraCheia, mean)
  
  
  # Separa medidas por dia
  temperaturasPorDia = split(temperaturasPorHora, as.Date(names(temperaturasPorHora)))
  temperaturasPorDia = lapply(temperaturasPorDia, signif, digits = 4)
  
  # Adiciona zeros para horas sem temperatura
  temperaturasPorDia = lapply(temperaturasPorDia, function(dia) {
    horas = strptime(names(dia), "%Y-%m-%d %H")[["hour"]]
    serieDiaAtual = rep(NA, 24)
    serieDiaAtual[horas+1] = dia
    return(serieDiaAtual)
  });
  
  # Cria uma matriz com cada linha sendo a medida de 24 horas de um dia
  dias = do.call(rbind, temperaturasPorDia)
}


# Pre-processa os dados, removendo valores de temperatura invalidos
# 
# Args:
#  dias: matriz de 24 colunas e N linhas, onde as colunas sao as horas do dia e as linhas sao os dias
#
# Returns:
#  Matriz do mesmo formato da entrada, por??m substituindo NA's pela media dos pontos anterior e posterior
#  Caso o primeiro dia comece com NA, sera removido do resultado
#  Caso o ultimo dia termine com NA, sera removido do resultado
preProcessa <- function(dias) {
  temperaturas = dias
  
  # Remove os primeiros dias caso esses comecem com NA's
  while (is.na(temperaturas[1,1])) {
    temperaturas = temperaturas[-1,]
  }
  
  # Remove os ultimos dias caso esses terminem NA's
  while (is.na(temperaturas[length(temperaturas[,1]), 24])) {
    temperaturas = temperaturas[-length(temperaturas[,1]),]
  }
  
  # Transforma temperaturas em um vetor para simplificar o processamento
  dimTemperaturas <- dim(temperaturas)
  nomeDatas <- rownames(temperaturas)
  
  dim(temperaturas) <- NULL
  
  # Interpola os valores NA com base nos valores anterior e posterior
  indicesNAs = which(is.na(temperaturas))
  ultimoIndiceNAProcessado = 0
  for (i in 1:length(indicesNAs)) {
    # Processa apenas se o valor NA em questao ainda nao foi processado
    if (i > ultimoIndiceNAProcessado) {
      
      # Encontra valores NA em sequencia (pode ser 1 ou mais)
      j = i
      while (j+1 <= length(indicesNAs) & indicesNAs[j+1] == indicesNAs[j] + 1) {
        j = j+1
      }
      ultimoIndiceNAProcessado = j
      
      # Interpola valores NA em sequencia utilizando o valor anterior e o posterior
      tempAntes = temperaturas[indicesNAs[i]-1]
      tempDepois = temperaturas[indicesNAs[j]+1]
      tamanhoIntervalo = j-i+1
      incrementoTemp=(tempDepois-tempAntes)/(tamanhoIntervalo+1)
      temperaturaInterpolada = tempAntes + incrementoTemp
      
      # cat("Temperatura antes:", tempAntes, "[ ")
      
      for (k in indicesNAs[i]:indicesNAs[j]) {
        temperaturas[k] = signif(temperaturaInterpolada, 4)
        # cat(signif(temperaturaInterpolada, 4), " ")
        temperaturaInterpolada = temperaturaInterpolada + incrementoTemp
      }
      
      # cat("] Temp depois:", tempDepois, "\n")
    }
  }
  
  # Transforma temperaturas novamente em uma matriz
  dim(temperaturas) <- dimTemperaturas
  rownames(temperaturas) <- nomeDatas
  
  return(temperaturas)
}


# Retorna uma lista de N series de temperatura dada uma serie de um dia como criterio, onde N 
# eh o parametro numeroRetornados, Os registros sao ordenados do mais similar ao menos similar
#
# Args:
#  criterio: serie de temperaturas de um dia utilizada como criterio de buscas
#  dias: matriz de 24 colunas e N linhas, onde as colunas sao as horas do dia e as linhas sao os dias
#  funcaoDistancia: funcao de distancia a ser utilizada. Recebe dois vetores como parametros
#  funcaoOrdenacao: funcao de ordenacao de resultados a ser utilizado
#  numeroRetornados: Quantidade de registros retornados
#
# Returns:
#  resultado contendo os N resultados mais proximos do criterio de busca, ordenados com o mais
#  similar no topo
buscaSeries <- function(criterio, dias, funcaoDistancia, funcaoOrdenacao, numeroRetornados) {
  distancias <- apply(dias, 1, funcaoDistancia, criterio)
  # names(distancias) <- rownames(dias)
  distancias <- funcaoOrdenacao(distancias)
  return(distancias[1:numeroRetornados])
}

# Dado um ano/mes e uma lista resultante de uma busca de temperaturas, calcula a precisao @ length(resultado)
#
# Args:
#  anoMes: ano e mes no formato YYYY-mm
#  resultado: lista resultante de uma busca de temperaturas
#
# Returns:name
#  precisao calculada para a busca
calculaPrecisao <- function(anoMes, resultado, dias) {
  anoMesResultado <- obtemAnoMes(names(resultado))
  anoMesColecao <- obtemAnoMes(rownames(temperaturas))
  total <- min(sum(anoMes == anoMesColecao), length(resultado))
  acertos <- sum(anoMes == anoMesResultado)
  precisao <-acertos/total
  return(precisao)
}

# Busca as series mais semelhantes a serie passada como criterio e retorna a precisao para a busca efetuada
# 
# Args:
#  criterio: serie de temperaturas de um dia utilizada como criterio de buscas
#  nomeCriterio: nome do criterio, no formato de data string YYYY-mm-dd
#  dias: matriz de 24 colunas e N linhas, onde as colunas sao as horas do dia e as linhas sao os dias
#  funcaoDistancia: funcao de distancia a ser utilizada. Recebe dois vetores como parametros
#  funcaoOrdenacao: funcao de ordenacao de resultados a ser utilizado
#  numeroRetornados: Quantidade de registros retornados
calculaPrecisaoSerie <- function(criterio, nomeCriterio, dias, funcaoDistancia, funcaoOrdenacao, numeroRetornados) {
  resultado <- buscaSeries(criterio, dias, funcaoDistancia, funcaoOrdenacao, numeroRetornados)
  anoMes <- obtemAnoMes(nomeCriterio)
  return(calculaPrecisao(anoMes, resultado, dias))
}

# Obtem apenas o mes e ano de uma data string
#
# Args:
#  data: data string no formato YYYY-mm-dd
#
# Returns:
#  ano-mes no formato YYYY-mm
obtemAnoMes <- function(data) {
  return(substr(data, 1, 7))
}

# Formata precisao para exibicao em percentual
#
# Args:
#  precisao: precisao em decimal de 0 a 1
#
# Returns:
#  precisao formatada como string percentual
formataPrecisao <- function(precisao) {
  return(paste(signif(precisao*100,5), "%"))
}

# Distancia L1 - metodo manhattan
distanciaL1 <- function(v1, v2) {
  dist(rbind(v1,v2), method = "manhattan")
}

# Distancia L2 - metodo euclidean
distanciaL2 <- function(v1, v2) {
  
}

# Distancia maxima entre 2 componentes de X e Y
distanciaMax <- function(v1, v2) {
  dist(rbind(v1,v2), method = "maximum")
}

distanciaCanberra <- function(v1, v2) {
  dist(rbind(v1,v2), method = "canberra")
}

ordenacaoAscendente <- function(resultado) {
  sort(resultado, decreasing = FALSE)
}

ordenacaoDescendente <- function(resultado) {
  sort(resultado, decreasing = TRUE)
}

# Dada uma funcao de distancia e outra de ordenacao, calcula precisao media P@30 da colecao toda. 
# Para realizar o calculo, eh calculada a precisao da busca de cada um dos itens da colecao, e feita a media 
# de todas as precisoes obtidas
#
# Args:
#  temperaturas: matriz de 24 colunas e N linhas, onde as colunas sao as horas do dia e as linhas sao os dias
#  funcaoDistancia: funcao de distancia a ser utilizada. Recebe dois vetores como parametros
#  funcaoOrdenacao: funcao de ordenacao de resultados a ser utilizado
#
# Returns: 
#  precisao media da colecao
calculaPrecisaoMediaParaTodos <- function(temperaturas, funcaoDistancia, funcaoOrdenacao = ordenacaoAscendente) {
  # Para cada  linha da matriz temperaturas, calular a distancia com as outras linhas,
  m <- apply(temperaturas, 1, buscaSeries, temperaturas, funcaoDistancia, funcaoOrdenacao, 30)
  mean(m)
}

# Esta funcao carrega e processa os dados de temperatura do cepagri, em seguida exibindo uma comparacao de 
# eficacia de buscas na colecao utilizando diferentes funcoes de similaridade ou distancia. O criterio de 
# comparacao eh a precisao media P@30 utilizando cada uma das funcoes de distancia
exibeResultados <- function() {
  # Obtem e faz pre-processamento das temperaturas
  cat("Obtendo temperaturas do servidor \n")
  cpa = carregaArquivos()
  
  
  cat("Gerando series temporariais (uma para cada dia) \n")
  temperaturas = geraSeries(cpa)
  
  cat("Pre-processando series para eliminar valores invalidos \n")
  temperaturas = preProcessa(temperaturas)
  
  # TODO
  cat("Calculando precisao media com diferentes funcoes de distancia \n")
  l1 = calculaPrecisaoMediaParaTodos(temperaturas, distanciaL1)
  l2 = calculaPrecisaoMediaParaTodos(temperaturas, distanciaL2) # Parou de funcionar (??)
  lMax = calculaPrecisaoMediaParaTodos(temperaturas, distanciaMax)
  lCam = calculaPrecisaoMediaParaTodos(temperaturas, distanciaCanberra)
  
  cat("L1:", l1, "\n")
  cat("L2:", l2, "\n")
  cat("Max:", lMax, "\n")
  cat("Camberra:", lCam, "\n")
  calculaPrecisaoSerie()
}

# TODO exibeResultados()

