# Carrega localmente as medidas de temperatura
carregaArquivos <- function() {
  con <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv") 
  cpa <- read.csv(con, header = FALSE, sep = ";",col.names = c("horario","temperatura","vento","umidade","sensacao"), as.is = TRUE)  
}


# Gera uma serie temporal para cada dia de medidas
# Uma serie contem 24 medidas
# O resultado da funcao eh uma matriz de 24 colunas e N linhas, onde linhas eh a quantidade de dias processados
# Caso estejam incompletos, o primeiro e o ultimo dia da serie sao eliminados
geraSeries <- function (cpa) {
  horarios = strptime(cpa[["horario"]], "%d/%m/%Y-%H:%M")
  temperaturas = as.numeric(cpa[["temperatura"]])
  
  ultimoAno = 0
  ultimoDiaDoAno = 0
  serieDiaAtual = 0
  dias = 0
  qtdeDias = 0
  nomeDias = ""
  
  for (i in 1:length(horarios)) {
    horario = horarios[i]
    
    # Verifica se continua processando o mesmo dia do ano ou foi para o proximo
    if (!(horario$year == ultimoAno & horario$yday == ultimoDiaDoAno)) {
      # Atualiza variaveis de controle
      ultimoAno = horario$year
      ultimoDiaDoAno = horario$yday
      
      # Prenche serie apenas se nao eh a primeira vez dentro do for
      qtdeDias = qtdeDias + 1
      if (length(serieDiaAtual) > 1) {
        # print(serieDiaAtual)
        if (length(dias) == 1) {
          # Inicializa a matriz das series temporais de cada dia
          dias = matrix(serieDiaAtual, ncol = 24)
          nomeDias = nomeDia
        } else {
          # Adiciona uma nova linha na matriz das series temporaris
          dias = rbind(dias, serieDiaAtual)
          nomeDias[length(nomeDias) + 1] = nomeDia
        }
      }
      
      # Cria uma serie para processar o dia
      serieDiaAtual = rep(NA, 24)
      nomeDia = format(horario, "%Y-%m-%d")
    }
    
    # Atualiza a temperatura para o dia que esta sendo processado
    # TODO tem mais de uma medida por hora, como iremos montar a serie?
    serieDiaAtual[horario$hour + 1] = temperaturas[i]
  }
  rownames(dias) <- nomeDias
  return(dias)
}

# Pre-processa os dados, removendo valores de temperatura invalidos
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
  
  return(temperaturas)
}

buscaSeries <- function(criterio, dias, funcaoDistancia, funcaoOrdenacao, numeroRetornados) {
  # TODO
}

calculaPrecisao <- function(anoMes, resultado) {
  # TODO
}

# Distancia L1 - metodo manhattan
distanciaL1 <- function(v1, v2) {
  x <- rbind(v1,v2)
  dist(x, method ="manhattan")
}

# Distancia L1 - metodo euclidean
distanciaL2 <- function(v1, v2) {
  # TODO  
  x <- rbind(v1,v2)
  dist(x, method ="euclidean")
}

distanciaLInf <- function(v1, v2) {
  # TODO  
}

distanciaCosseno <- function(v1, v2) {
  # TODO  
}

ordenacaoAscendente <- function(resultado) {
  sort(resultado, decreasing = FALSE)
}

ordenacaoDescendente <- function(resultado) {
  sort(resultado, decreasing = TRUE)
}

calculaPrecisaoMediaParaTodos <- function(temperaturas, funcaoDistancia, funcaoOrdenacao) {
  # TODO
}

exibeResultados <- function() {
  # Obtem e faz pre-processamento das temperaturas
  cpa = carregaArquivos()
  temperaturas = geraSeries(cpa)
  temperaturas = preProcessa(temperaturas)
  
  # TODO
  calculaPrecisaoMediaParaTodos(temperaturas, distanciaL1, ordenacaoAscendente)
  calculaPrecisaoMediaParaTodos(temperaturas, distanciaL2, ordenacaoAscendente)
  calculaPrecisaoMediaParaTodos(temperaturas, distanciaLInf, ordenacaoAscendente)
  calculaPrecisaoMediaParaTodos(temperaturas, distanciaLCosseno, ordenacaoDescendente)
}

# TODO exibeResultados()

