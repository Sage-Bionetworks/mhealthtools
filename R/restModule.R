################## balance and turn functions

shapeRestData <- function(restData){
  timestamp <- restData$timestamp - restData$timestamp[1]
  accel <- restData$userAcceleration * 9.8
  data <- cbind(timestamp, accel)
  data
}

#dat <- restData
GetBalanceFeatures <- function(dat) {

  error = 'None'

  res <- tryCatch({
      tmp <- Turning(dat)
      list(dat=tmp$dat, turnTime = tmp$turningTime, error='None')
  },error=function(e){
      error="unable to process turning time"
      list(dat=dat, turnTime = NA, error=error)
  })

  dat <- res$dat
  turningTime <- res$turnTime
  error=res$error

  x <- dat[, "x"]
  y <- dat[, "y"]
  z <- dat[, "z"]
  aa <- sqrt(x^2 + y^2 + z^2)
  ###############################
  meanAA <- mean(aa, na.rm = TRUE)
  sdAA <- sd(aa, na.rm = TRUE)
  modeAA <- pracma::Mode(aa)
  skewAA <- Skewness(aa)
  kurAA <- Kurtosis(aa)
  auxAA <- quantile(aa, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  q1AA <- auxAA[[2]]
  medianAA <- auxAA[[3]]
  q3AA <- auxAA[[4]]
  iqrAA <- q3AA - q1AA
  rangeAA <- auxAA[[5]] - auxAA[[1]]
  acfAA <- acf(aa, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
  zcrAA <- ZCR(aa)

  dfaAA <- tryCatch({ fractal::DFA(aa, sum.order = 1)[[1]] },
                    error = function(err){ NA })

  out <- c(meanAA, sdAA, modeAA, skewAA, kurAA, q1AA, medianAA, q3AA, iqrAA, rangeAA,
           acfAA, zcrAA, dfaAA, turningTime)
  names(out) <- c("meanAA", "sdAA", "modeAA","skewAA", "kurAA", "q1AA", "medianAA",
                  "q3AA", "iqrAA", "rangeAA", "acfAA", "zcrAA", "dfaAA", "turningTime")
  bpa <- FeaturesBpa(dat)
  dis <- BoxVolumeFeature(dat)

  # features
  restFeatures <- c(out, bpa, dis, 'error'=error)
  return(restFeatures)
}

Turning <- function(dat, Q = 30, msl = 100) {
  aa <- sqrt(dat[, "x"]^2 + dat[, "y"]^2 + dat[, "z"]^2)
  aux <- changepoint::cpt.mean(aa, Q = Q, minseglen = msl,
                               penalty = "BIC", method = "BinSeg")
  cp <- changepoint::cpts(aux)
  if (length(cp) > 0) {
    cp <- cp[length(cp)]
    dat <- dat[-seq(cp), ]
    turningTime <- cp/100
  } else {
    turningTime <- 0
  }
  list(dat = dat, turningTime = turningTime)
}

FeaturesBpa <- function(post) {
  ft <- rep(NA, 3)
  time <- post[, 1] - post[1, 1]
  dTime <- time[length(time)] - time[1]
  post <- post[, -1]
  N <- nrow(post)
  # Orientation
  mg <- apply(post, 2, mean)
  # Orientation-corrected force signals
  postforce <- post - matrix(rep(mg, N), N, 3, byrow = TRUE)
  # Scaled velocity signals
  dt <- diff(time)
  dt <- c(dt, dt[length(dt)])
  postvel <- apply(postforce * matrix(rep(dt, 3), ncol = 3), 2, cumsum)
  # Average scaled power X, Y, Z
  postpower <- mean(apply(0.5 * 70 * postvel^2, 2, sum)/dTime)/10000
  # Force vector magnitude signal
  postmag <- sqrt(apply(postforce^2, 1, sum))
  # Maximum force
  postpeak <- as.numeric(quantile(postmag, 0.95))/10
  # Detrended fluctuation analysis scaling
  # exponent
  alpha <- tryCatch({ fractal::DFA(postmag, sum.order = 1)[[1]] },
                    error = function(err){ NA })
  # Output posture test feature vector
  ft <- c(postpeak, postpower, alpha)
  names(ft) <- c("postpeak", "postpower","alpha")
  ft
}

GetDisplacement <- function(time, accel) {
  deltaTime <- diff(time)
  n <- length(deltaTime)
  vel <- rep(NA, n)
  dis <- rep(NA, n)
  vel[1] <- 0
  dis[1] <- 0
  for (i in 2:n) {
    vel[i] <- vel[i - 1] + 0.5 * (accel[i] +
                                    accel[i - 1]) * deltaTime[i]
    dis[i] <- dis[i - 1] + 0.5 * (vel[i] +
                                    vel[i - 1]) * deltaTime[i]
  }
  list(vel = vel, dis = dis)
}


GetXYZDisplacement <- function(x) {
  disX <- GetDisplacement(x[, "timestamp"],
                          x[, "x"])$dis
  disY <- GetDisplacement(x[, "timestamp"],
                          x[, "y"])$dis
  disZ <- GetDisplacement(x[, "timestamp"],
                          x[, "z"])$dis
  list(disX = disX, disY = disY, disZ = disZ)
}


CenterAcceleration <- function(x) {
  x[, "x"] <- x[, "x"] - median(x[, "x"])
  x[, "y"] <- x[, "y"] - median(x[, "y"])
  x[, "z"] <- x[, "z"] - median(x[, "z"])
  x
}


BoxVolumeFeature <- function(x) {
  x <- CenterAcceleration(x)
  aux <- GetXYZDisplacement(x)
  rdx <- range(aux$disX, na.rm = TRUE)
  rdy <- range(aux$disY, na.rm = TRUE)
  rdz <- range(aux$disZ, na.rm = TRUE)
  dVol <- diff(rdx) * diff(rdy) * diff(rdz)
  rddx <- range(diff(aux$disX), na.rm = TRUE)
  rddy <- range(diff(aux$disY), na.rm = TRUE)
  rddz <- range(diff(aux$disZ), na.rm = TRUE)
  ddVol <- diff(rddx) * diff(rddy) * diff(rddz)
  vols <- c(dVol, ddVol)
  names(vols) <- c("dVol", "ddVol")
  vols
}

createRestFeaturesErrorResult <- function(error) {
  features <- c(rep(NA, 19), error)
  names(features) <- c("meanAA", "sdAA", "modeAA", "skewAA", "kurAA", "q1AA", "medianAA",
                       "q3AA", "iqrAA", "rangeAA", "acfAA", "zcrAA", "dfaAA", "turningTime", "postpeak",
                       "postpower", "alpha", "dVol", "ddVol", "error")
  features
}

####### MAIN
#' extracts features from accelerometer rest data file
#'
#' @param restAccel_json_file path to accelerometer json file
#' @return data frame of rest features
#' @export
#' @examples
#' library(synapseClient)
#' synapseLogin()
#' walkingTable <- synTableQuery("SELECT * FROM syn5713119")
#' walkingTable <- walkingTable@values
#' sampleRow <- rownames(walkingTable)[10]
#' sample_restAccel_jsonFile <- synDownloadTableFile('syn5713119',sampleRow, 'deviceMotion_walking_rest.json.items')
#' sample_restAccel_jsonFile <- as.character(sample_restAccel_jsonFile)
#' sample_restResults <- getRestFeatures(sample_restAccel_jsonFile)
getRestFeatures <- function(restAccel_json_file) {
  results <- readJsonFile(restAccel_json_file)
  restData <- results$data
  error <- results$error
  if (error == T) {
    restFeatures <- createRestFeaturesErrorResult("unable to read JSON file")
  } else if (is.data.frame(restData) == F) {
    restFeatures <- createRestFeaturesErrorResult("expected data frame after reading rest accelerator json file")
  } else if (nrow(restData) < 5) {
    restFeatures <- createRestFeaturesErrorResult("rest features data frame has less than 5 rows")
  } else {
    # shape data
    restData <- shapeRestData(restData)
    # compute the Rest features
    restFeatures <- GetBalanceFeatures(restData)
  }
}

#######
#######


