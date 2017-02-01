######## utility functions

synapse_downLoadTableFile <- function(table, row, column){
  tryCatch({
    return(synDownloadTableFile(table, row, column))
  }, error=function(err){
    print(paste('Error: ',err))
    print(row)
    return(NA)
  })
}

readJsonFile <- function(jsonFile){
  tryCatch({
    data <- jsonlite::fromJSON(jsonFile)
    list(data=data, error=FALSE)
  }, error = function(err){
    #print('Unable to read JSON file')
    list(data=NA, error=TRUE)
  })
}

calculateDrift <- function(x, y) {
  dx <- diff(x, lag = 1)
  dy <- diff(y, lag = 1)
  sqrt(dx^2 + dy^2)
}

Skewness <- function(x) {
  x <- x[!is.na(x)]
  mu <- mean(x)
  mean((x - mu)^3)/(mean((x - mu)^2)^(3/2))
}

Kurtosis <- function(x) {
  x <- x[!is.na(x)]
  mu <- mean(x)
  mean((x - mu)^4)/(mean((x - mu)^2)^2)
}

Cv <- function(x) {
  x <- x[!is.na(x)]
  (sd(x)/mean(x)) * 100
}

ZCR <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  aux.x <- rep(1, n)
  aux.x[x <= mean(x)] <- -1
  sum(aux.x[-n] * aux.x[-1] < 0)/(n - 1)
}

## Mean Teager-Kaiser energy operator of
## inter-taps intervals (from TKEO function
## in library(seewave) using f = 1, m = 1, M
## = 1)
MeanTkeo <- function(x) {
  x <- x[!is.na(x)]
  y <- x^2 - c(x[-1], NA) * c(NA, x[1:(length(x) -
                                         1)])
  mean(y, na.rm = TRUE)
}

CountDataPoints <- function(fnms) {
  dat <- fromJSON(fnms)
  length(dat)
}

RemoveNAs <- function(x) {
  nc <- ncol(x)
  nacounts <- apply(is.na(x), 1, sum)
  tokeep <- nacounts != nc
  x[tokeep, ]
}