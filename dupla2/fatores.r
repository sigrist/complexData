# Nomes:
# Paulo Sigrist
# Rafael Sangalli
# Exercicio 1

fatores <- function(n) {
  primo <- 2
  ultimo <- 1
  tamanho <- 0
  fatores <- 0
  while(n > 1) {
    if(n %% primo == 0) {
      n <- n / primo
      if(primo != ultimo) {
        tamanho <- tamanho + 1
        ultimo <- primo
        fatores[tamanho] <- primo
      }
    } else {
      primo <- primo + 1
    }
    
  }
  return(fatores)
}

# Fatores2 - dado um numero inteiro, retorna uma matriz, formada por duas linhas.
#          - Para cada fator f da primeira linha, o elemento correspondente da segunda linha deve representar 
#          - o numero de vezes que o fator f aparece na fatoracao de n

fatores2 <- function(n) {
  # Comecar com o menor numero primo conhecido
  primo <- 2
  # Marcamos o ultimo como 1 para inicializar a marcacao
  ultimo <- 1
  # Tamanho do resultado, inicialmente vazio
  tamanho <- 0
  # Lista dos fatores, inicialmente vazia
  fatores <- 0
  # Lista das repeticoes, inicialmente com uma repeticao
  repeticoes <- 1
  
  # Enquanto n for maior que 1
  while(n > 1) {
    # Validar se o numero eh divizivel pelo primo
    if(n %% primo == 0) {
      # Caso positivo, atualize n
      n <- n / primo
      
      # Se o primo nao for o ultimo salvo
      if(primo != ultimo) {
        # aumente a posicao do vetor
        tamanho <- tamanho + 1
        # atualize o ultimo numero
        ultimo <- primo
        # atualize o vetor de fatores
        fatores[tamanho] <- primo
      } else {
        # como o primo esta repetindo, atualizar o vetor de repeticoes
        v <- repeticoes[tamanho]
        # incremente o valor de v e atualize o vetor de repeticoes
        repeticoes[tamanho] <- v+1
      }
    } else {
      # n nao eh divisivel pelo primo, passar para o proximo primo
      primo <- primo + 1
      # aumentar uma posicao no vetor de repeticoes e inicializar a nova posicao com uma repeticao
      repeticoes[tamanho+1] <- 1
      
    }
    
  }
  # Ao acabar, transformar os dois vetores em uma matriz
  m <- rbind(fatores, repeticoes)
  
  # Retorne a matrix
  return (m)
}

# Converte a matriz retornada pela funcao fatores2 no valor de entrada original
fatores3 <- function(m) {
  # Obtem o numero de colunas da matriz
  cols <- ncol(m)
  # Inicializa o vetor de valores com 1
  value <- 1
  # Para cada coluna...
  for (i in seq(cols)) {
    # Pega o numero primo (linha 1, coluna i)
    primo <- m[1, i]
    # Pega o numero de repeticoes (linha 2, coluna i)
    repeticoes <- m[2,i]
    # Atualiza o valor com o valor multiplicado pelo exponencial de primo / repeticoes
    value <- value * primo ^ repeticoes
  }
  # Retorna o valor
  value
}