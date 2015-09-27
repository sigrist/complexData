idf <- function (termo, documents) {
  
  t = table(document)
  
  nt = t[termo]
  
  # log(nroDocuments/nt, base = exp(1))
  as.double(nroDocuments/nt)
}