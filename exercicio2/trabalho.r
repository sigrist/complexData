# INF-612 Handling Data
# Alunos - Paulo Sigrist / Rafael Sangalli

# Carrega localmente as medidas de temperatura
#
# Returns:
#  Data frame com medidas de temperatura obtidas do cepagri
loadAndFilterData <- function() {
  con <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv") 
  cpa <- read.csv(con, header = FALSE, sep = ";",col.names = c("horario","temperatura","vento","umidade","sensacao"), as.is = TRUE)  
  
  # Transforma horario em formato de data para facilitar manipulacao
  cpa[["horario"]] <- strptime(cpa[["horario"]], "%d/%m/%Y-%H:%M")

  # Filtrar o data frame pela data estipulda
  cpa <- cpa[cpa$horario >= as.POSIXlt("2014-07-01 00:00:00") & cpa$horario <= as.POSIXlt("2015-06-30 23:59:59"),]
  
  # Converter colunas strings para numerica
  cpa[["temperatura"]] <- as.double(cpa[["temperatura"]])
  cpa[["vento"]] <- as.double(cpa[["vento"]])
  cpa[["umidade"]] <- as.double(cpa[["umidade"]])
  cpa[["sensacao"]] <- as.double(cpa[["sensacao"]])
  
  return(cpa)
}

meanByMonth <- function(cpa) {
  # Media dos meses, ignorando as leituras erradas
  r <- tapply(cpa[["temperatura"]], cpa[["horario"]]$mon, mean, na.rm = TRUE)
  # Aplica os meses 
  rownames(r) <- month.name

  barplot(r,col = colors()[1:12],
          main = "Media de temperatura por mes",
          xlab = "Meses",
          ylab = "Temperatura") 
  
}

# Retorna todos os dados de um mes especifico
dataByMonth <- function(m, cpa) {
  cpa[cpa$horario$mon == m,]
}

# Retorna todos os dados de um periodo
dataByInterval <- function(from, to, cpa) {
  cpa[cpa$horario >= as.POSIXlt(from) & cpa$horario <= as.POSIXlt(to),]
}

# Retorna a media de temperatura por dia de um CPA
meanByDay <- function(month, cpa) {
  x <- dataByMonth(month, cpa)
  tapply(x[["temperatura"]], x[["horario"]]$mday, mean, na.rm = TRUE)
}

temperatureByDay <- function(cpa) {
  # Aplicar a funcao meanByDay para os meses de 0-11 no data frame cpa
  r <- sapply(c(0:11), meanByDay, cpa)
  
  # Vetor de cores para serem utilizadas no grafico
  cores <- c("red", "blue", "green", "yellow", "brown", "purple", "yellowgreen", "orange", "magenta", "pink", "navy", "black")

    # Plotar um grafico  
  plot(c(10:32),
       type = "n",
       main = "Alteracao de temperatura por dia",
       xlab = "Dias", ylab = "Graus (em Celsius)")
  
  # Para cada resultado, adicionar uma linha ao grafico
  sapply(c(1:12), function (l) { lines(unlist(r[l]), col=cores[l]) })
  
  legend("bottomleft", month.name,
         col = cores, pch = 15, cex = 0.75)

}

# Cria uma estrutura que divide os dados pela estacao do ano
# acessar como:
#   m$inverno 
#   m$outono 
#   m$verao 
#   m$primavera 

dataBySeason <- function(cpa) {
  # Estacoes do ano
  #   - 21 Junho 2014 - Inverno
  #   - 22 Setembro 2014 - Primavera
  #   - 21 Dezembro 2014 - Verao
  #   - 20 Marco 2015 - Outono
  #   - 21 Junho 2014 - Inverno
  
  
  inverno <- dataByInterval("2014-06-21 00:00:00", "2014-09-21 23:59:59", cpa)
  primavera <- dataByInterval("2014-09-22 00:00:00", "2014-12-20 23:59:59", cpa)
  verao <- dataByInterval("2014-12-22 00:00:00", "2015-03-19 23:59:59", cpa)
  outono <- dataByInterval("2015-03-20 00:00:00", "2015-06-20 23:59:59", cpa)
  inverno2 <- dataByInterval("2015-06-21 00:00:00", "2015-09-21 23:59:59", cpa)
  # Concatenar os dois periodos de inverno
  inverno <- rbind(inverno, inverno2)
  
  m <- 0
  m$inverno <- inverno
  m$primavera <- primavera
  m$verao <- verao
  m$outono <- outono
  
  return(m)
}

# Cria um histograma com os dados
histogramByValue <- function(data, title, xlabel = title, col1 = "rosybrown", col2 = "blue") {
  h <- hist(data, main = title,
            xlab = xlabel,
            ylab = "Ocorrencias", col = col1)
  xfit <- seq(min(data, na.rm = TRUE), max(data, na.rm = TRUE), length = 100)
  yfit <- dnorm(xfit, mean = mean(data, na.rm = TRUE), sd = sd(data, na.rm = TRUE))
  yfit <- yfit * diff(h$mids[1:2]) * length(data)
  lines(xfit, yfit, col = col2, lwd = 3)
}

stripChartAll <- function(data, title) {
  
  boxplot(data[,2:5], main = title,
          col = rgb(200, 0, 0, 100, maxColorValue = 255))
}