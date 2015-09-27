library(jpeg)

getLibraryFiles <- function() {
  dir = "/Users/sigrist/Documents/workspaces/r_test/exerciciosPraticos/relevants"
  files = list.files(dir, recursive = TRUE, full.names = TRUE)
  files = files[grep("\\.[jJ][Pp][Gg]", files)]
}

calculateHistogramOld <- function(img, bins) {
  binRange <- seq(0, 1, length.out = bins + 1)
  binRange[1] = -1  
  
  data = img
  histogram = vector(mode = "integer", length = bins^3)
  posicao = 1
  for (i in 2:length(binRange)) {
    for (j in 2:length(binRange)) {
      for (k in 2:length(binRange)) {
        rFilter = data[,,1] > binRange[i-1] & data[,,1] <= binRange[i]
        gFilter = data[,,2] > binRange[j-1] & data[,,2] <= binRange[j]
        bFilter = data[,,3] > binRange[k-1] & data[,,3] <= binRange[k]
        
        histogram[posicao] = length(data[rFilter & gFilter & bFilter]) / 3
        posicao = posicao + 1
      }
    }
  }
  
  return(histogram)
}

calculateHistogram <- function(img, bins) {
  return(hist(img, plot = FALSE, breaks = seq(0, 1, l=(bins^3+1)))[["counts"]])
}

histogramsFromCollection <- function(files, bins) {
  histograms = matrix(ncol = bins ^ 3, nrow = length(files))
  cat("Processing ", length(files), ". It may take a while")
  count = 0
  for (i in 1:length(files)) {
    img <- readJPEG(files[i])
    histogram = calculateHistogram(img, bins)
    histograms[i,] <- calculateHistogram(img, bins)
    count = count + 1
    if (count %% 50 == 0) {
      cat(count, " files processed")
    }
  }
  cat(count, " files processed")
  return(histograms)
}

processLibrary <- function(bins) {
  files = getLibraryFiles()
  return(histogramsFromCollection(files, bins))
}

distance <- function(v1, v2) {
  return(sqrt(sum((v1 - v2)^2)))
}

getDistances <- function(query, database) {
  values = vector(mode = "integer", length = nrow(database))
  for (i in 1:nrow(database)) {
    values[i] = distance(query, database[i,])
  }
  return(values)
}

findSimilars <- function(queryHistogram, files, histograms) {
  distances = getDistances(queryHistogram, histograms)
  result = data.frame(files, distances)
  result = result[order(result[["distances"]]),]
  return(result)
}

printRanking <- function(file, files, histograms, bins) {
  splittedFilename = strsplit(file, "/")[[1]]
  splittedFilename = splittedFilename[1:length(splittedFilename)-1]
  relevantDir = paste(splittedFilename, collapse = "/")
  query = calculateHistogram(readJPEG(file), bins)
  result = findSimilars(query, files, histograms)
  print(result[1:100,])
}

calculatePrecisionAndRecall <- function(relevantDir, result, topFilesCount) {
  topFiles = result[["files"]][1:topFilesCount]
  relevantFilesFound = length(grep(relevantDir, topFiles))
  precision = relevantFilesFound/topFilesCount
  relevantFilesCount = length(list.files(relevantDir, recursive = TRUE, full.names = TRUE))
  recall = relevantFilesFound/relevantFilesCount  
  return(c(precision, recall))
}

calculateAveragePrecision <- function(files, histograms) {
  accuPrecisionAndRecall = c(0, 0)
  for (i in 1:length(files)) {
    splittedFilename = strsplit(files[i], "/")[[1]]
    splittedFilename = splittedFilename[1:length(splittedFilename)-1]
    relevantDir = paste(splittedFilename, collapse = "/")
    query = histograms[i,]
    
    result = findSimilars(query, files, histograms)
    accuPrecisionAndRecall = accuPrecisionAndRecall + calculatePrecisionAndRecall(relevantDir, result, 30)
  }
  accuPrecisionAndRecall = accuPrecisionAndRecall / length(files)
  print(accuPrecisionAndRecall)
}

bins = 4
# Examples:
#histograms = processLibrary(bins)
#calculateAveragePrecision(getLibraryFiles(), histograms)
#printRanking("/Users/vntrasa/Documents/Pessoal/pos-graduacao/juntando-dados/R/relevants/A8225/A8200.JPG", getLibraryFiles(), histograms)
