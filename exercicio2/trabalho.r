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

  cpa <- subset(cpa, cpa$horario >= as.POSIXlt("2014-07-01") & cpa$horario <= as.POSIXlt("2015-06-30 23:59:59"))
  
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

dataByMonth <- function(m, cpa) {
  subset(cpa, cpa$horario$mon == m)
}

# Retorna a media de temperatura por dia de um CPA
meanByDay <- function(cpa) {
  tapply(cpa[["temperatura"]], cpa[["horario"]]$mday, mean, na.rm = TRUE)
}

temperatureByDay <- function(cpa) {
  
  # TODO COmo fazer isso com um comando (apply, tapply??)
  jan <- meanByDay(dataByMonth(0, cpa))
  fev <- meanByDay(dataByMonth(1, cpa))
  mar <- meanByDay(dataByMonth(2, cpa))
  abr <- meanByDay(dataByMonth(3, cpa))
  mai <- meanByDay(dataByMonth(4, cpa))
  jun <- meanByDay(dataByMonth(5, cpa))
  jul <- meanByDay(dataByMonth(6, cpa))
  ago <- meanByDay(dataByMonth(7, cpa))
  set <- meanByDay(dataByMonth(8, cpa))
  out <- meanByDay(dataByMonth(9, cpa))
  nov <- meanByDay(dataByMonth(10, cpa))
  dez <- meanByDay(dataByMonth(11, cpa))
  
  plot(c(10:32),
       type = "n",
       main = "Alteracao de temperatura por dia",
       xlab = "Dias", ylab = "Graus (em Celsius)")
  lines(jan, col = "red")
  lines(fev, col = "blue")
  lines(mar, col = "green")
  lines(abr, col = "yellow")
  lines(mai, col = "brown")
  lines(jun, col = "purple")
  lines(jul, col = "yellowgreen")
  lines(ago, col = "orange")
  lines(set, col = "magenta")
  lines(out, col = "pink")
  lines(nov, col = "navy")
  lines(dez, col = "black")
  
  legend("bottomleft", month.name,
         col = c("red", "blue", "green", "yellow", "brown", "purple", "yellowgreen", "orange", "magenta", "pink", "navy", "black"), pch = 15, cex = 0.75)

}
