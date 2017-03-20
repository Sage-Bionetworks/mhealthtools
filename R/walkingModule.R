#### Functions to extract features from walking module ####
## Inputs: walking json file from the researchKit app - mPower
## SEE WORKING EXAMPLE at the END

process_medicationChoiceAnswers <- function(json_file) {
  tryCatch({
    d <- jsonlite::fromJSON(json_file)
    data.frame(medication = paste(unique(d$identifier),
                                  collapse = "+"), medicationTime = paste(unique(d$answer),
                                                                          collapse = "+"))
  }, error = function(err) {
    data.frame(medication = "NA", medicationTime = "NA")
  })
}


getMedianF0 <- function(tmp_time, y, nframe = 10){
  n = length(y)
  dt = round(n/(nframe + 1))

  if (length(unique(y)) > 1){
    F0 = as.numeric()
    for (i in 1:nframe) {
      nstart = (i - 1) * dt + 1
      nend = (i + 1) * dt
      F0[i] = lomb::lsp(y[nstart:nend], tmp_time[nstart:nend],
                        plot = FALSE, from = 0.2, to = 5)$peak.at[1]
    }
    medianF0 = median(F0, na.rm = T)
    sdF0 = sd(F0, na.rm = T)
  } else {
    medianF0 = 0
    sdF0 = 0
  }
  return(c(medianF0 = medianF0, sdF0 = sdF0))
}


SingleAxisFeatures <- function(x, tmp_time, varName) {
  meanX <- mean(x, na.rm = TRUE)
  sdX <- sd(x, na.rm = TRUE)
  modeX <- pracma::Mode(x)
  skewX <- e1071::skewness(x)
  kurX <- e1071::kurtosis(x)
  auxX <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  q1X <- auxX[[2]]
  medianX <- auxX[[3]]
  q3X <- auxX[[4]]
  iqrX <- q3X - q1X
  rangeX <- auxX[[5]] - auxX[[1]]
  acfX <- stats::acf(x, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
  zcrX <- ZCR(x)
  dfaX <- tryCatch({
    fractal::DFA(x, sum.order = 1)[[1]]
  }, error = function(err){ NA })
  cvX <- Cv(x)
  tkeoX <- MeanTkeo(x)
  lspX <- tryCatch({
    lomb::lsp(cbind(tmp_time, x), plot = FALSE)
  },error = function(err) { NA })
  F0X <- tryCatch({
    lspX$peak.at[1]
  }, error=function(err) { NA })
  P0X <- tryCatch({
    lspX$peak
  }, error=function(err) { NA })
  lspXF <- tryCatch({
    lomb::lsp(cbind(tmp_time, x), plot = FALSE, from = 0.2, to = 5)
  },error = function(err) { NA })
  F0XF <- tryCatch({
    lspXF$peak.at[1]
  }, error=function(err) { NA })
  P0XF <- tryCatch({
    lspXF$peak
  }, error=function(err) { NA })
  summaryF0X <- tryCatch({
    as.numeric(getMedianF0(tmp_time, x))
  }, error = function(err){ c(NA, NA) })
  tlagX <- tryCatch({
    tmp_time[fractal::timeLag(x, method = 'acfdecor')]
  }, error = function(err){ NA })

  out <- c(meanX, sdX, modeX, skewX, kurX,
           q1X, medianX, q3X, iqrX, rangeX, acfX,
           zcrX, dfaX, cvX, tkeoX, F0X, P0X,
           F0XF, P0XF, summaryF0X, tlagX)

  nms <- c("mean", "sd", "mode", "skew", "kur", "q1",
           "median", "q3", "iqr", "range",
           "acf", "zcr", "dfa", "cv", "tkeo", "F0","P0",
           "F0F", "P0F", "medianF0F", "sdF0F", "tlag")

  names(out) <- paste(nms, varName, sep = "")
  return(out)
}



#######
# Function to rotate the user acceleration
#######
get_quaternary_rotated_userAccel <- function(dat){
  attitude <- dat$attitude
  userAccel <- dat$userAcceleration
  colnames(userAccel) <- paste0('a_',colnames(userAccel))
  tmp_dat <- data.frame(attitude, userAccel)
  tmp_res <- apply(tmp_dat, 1, function(row){
    tmp_userAccel <- row[c('a_x', 'a_y', 'a_z')]
    tmp_userAccel <- as.matrix(tmp_userAccel, nrow=3)
    quaternion  <- row[c('x', 'y', 'z', 'w')]

    #create a rotation matrix from quaternion
    rotMatrix <- QuaternionRotationMatrix(row)

    #translate the user acceleration to uniform reference frame
    crossprod(rotMatrix, tmp_userAccel)
  })

  userAccelerationRotated <- t(tmp_res)
  colnames(userAccelerationRotated) <- c('accel_xrot', 'accel_yrot', 'accel_zrot')
  userAccelerationRotated
}




ShapeGaitData <- function(dat) {
  timestamp = dat$timestamp - dat$timestamp[1]
  userAccel = dat$userAcceleration
  colnames(userAccel) = gsub("userAcceleration\\.",
                             "", colnames(userAccel))
  userAccel_rotated <- dat %>% dplyr::select(accel_xrot, accel_yrot, accel_zrot)
  accel_dat <- cbind(timestamp, userAccel, userAccel_rotated)
}


####### MAIN
#' extracts walking features from walking accelerometer JSON data file
#'
#'
#' @param walking_json_file path to walking accelerometer json file
#' @return data frame of walking features
#' @export
#' @examples
#' library(synapseClient)
#' synapseLogin()
#' sample_walking_File <-'syn7077340'
#' walkingJsonFile <- synGet(sample_walking_File)@filePath
#' getWalkFeatures(walkingJsonFile)

getWalkFeatures <- function(walking_json_file) {
  if (is.na(walking_json_file) == T) {
    null_result = c(rep(NA, 113), error = "no json data file")
    names(null_result) = c("meanX", "sdX", "modeX", "skewX", "kurX", "q1X",
                           "medianX", "q3X", "iqrX", "rangeX", "acfX", "zcrX",
                           "dfaX", "cvX", "tkeoX", "F0X", "P0X","F0FX", "P0FX",
                           "medianF0FX", "sdF0FX", "tlagX", "meanY", "sdY", "modeY",
                           "skewY", "kurY", "q1Y", "medianY", "q3Y", "iqrY",
                           "rangeY", "acfY", "zcrY", "dfaY", "cvY", "tkeoY",
                           "F0Y", "P0Y", "F0FY", "P0FY", "medianF0FY", "sdF0FY",
                           "tlagY", "meanZ", "sdZ", "modeZ", "skewZ", "kurZ", "q1Z",
                           "medianZ", "q3Z", "iqrZ", "rangeZ", "acfZ", "zcrZ", "dfaZ",
                           "cvZ", "tkeoZ", "F0Z", "P0Z", "F0FZ", "P0FZ", "medianF0FZ",
                           "sdF0FZ", "tlagZ", "meanAA", "sdAA", "modeAA", "skewAA", "kurAA",
                           "q1AA", "medianAA", "q3AA", "iqrAA", "rangeAA", "acfAA", "zcrAA",
                           "dfaAA", "cvAA", "tkeoAA", "F0AA", "P0AA", "F0FAA", "P0FAA",
                           "medianF0FAA", "sdF0FAA", "tlagAA","meanAJ", "sdAJ", "modeAJ",
                           "skewAJ", "kurAJ", "q1AJ", "medianAJ", "q3AJ", "iqrAJ", "rangeAJ",
                           "acfAJ", "zcrAJ", "dfaAJ", "cvAJ", "tkeoAJ", "F0AJ", "P0AJ",
                           "F0FAJ", "P0FAJ", "medianF0FAJ", "sdF0FAJ", "tlagAJ",
                           "corXY", "corXZ", "corYZ", "error")
    return(null_result)
  }

  dat <- jsonlite::fromJSON(walking_json_file)
  userAccel_rotated <- get_quaternary_rotated_userAccel(dat)
  dat <- cbind(dat, userAccel_rotated)
  dat <- ShapeGaitData(dat)

  ###############################
  x <- dat$x
  y <- dat$y
  z <- dat$z
  aa <- sqrt(x^2 + y^2 + z^2)
  aj <- sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)
  ##################################
  x_rot <- dat$accel_xrot
  y_rot <- dat$accel_yrot
  z_rot <- dat$accel_zrot
  aa_rot <- sqrt(x_rot^2 + y_rot^2 + z_rot^2)
  aj_rot <- sqrt(diff(x_rot)^2 + diff(y_rot)^2 + diff(z_rot)^2)
  ###################################


  ###################################
  outX <- mpowertools:::SingleAxisFeatures(x, dat$timestamp, varName = "X")
  outY <- mpowertools:::SingleAxisFeatures(y, dat$timestamp, varName = "Y")
  outZ <- mpowertools:::SingleAxisFeatures(z, dat$timestamp, varName = "Z")
  outAA <- mpowertools:::SingleAxisFeatures(aa,  dat$timestamp, varName = "AA")
  outAJ <- mpowertools:::SingleAxisFeatures(aj,  dat$timestamp[-1], varName = "AJ")
  ###############################
  outX_rot <- mpowertools:::SingleAxisFeatures(x_rot, dat$timestamp, varName = "X_rot")
  outY_rot <- mpowertools:::SingleAxisFeatures(y_rot, dat$timestamp, varName = "Y_rot")
  outZ_rot <- mpowertools:::SingleAxisFeatures(z_rot, dat$timestamp, varName = "Z_rot")
  outAA_rot <- mpowertools:::SingleAxisFeatures(aa_rot,  dat$timestamp, varName = "AA_rot")
  outAJ_rot <- mpowertools:::SingleAxisFeatures(aj_rot,  dat$timestamp[-1], varName = "AJ_rot")
  ############################
  corXY <- cor(x, y, use = "p")
  corXZ <- cor(x, z, use = "p")
  corYZ <- cor(z, y, use = "p")
  cors <- c(corXY, corXZ, corYZ)
  names(cors) <- c("corXY", "corXZ", "corYZ")
  ############################
  corXY_rot <- cor(x_rot, y_rot, use = "p")
  corXZ_rot <- cor(x_rot, z_rot, use = "p")
  corYZ_rot <- cor(z_rot, y_rot, use = "p")
  cors_rot <- c(corXY_rot, corXZ_rot, corYZ_rot)
  names(cors_rot) <- c("corXY_rot", "corXZ_rot", "corYZ_rot")
  c(outX, outY, outZ, outAA, outAJ, cors,
    outX_rot, outY_rot, outZ_rot, outAA_rot, outAJ_rot, cors_rot,
    error = NA)
}



####### MAIN
#' extracts pedometer features from walking pedometer JSON data file
#'
#'
#' @param pedo_json_file path to pedometerjson file
#' @return data frame of pedometer features
#' @export
#' @examples
#' library(synapseClient)
#' synapseLogin()
#' sample_walking_pedometer_json_file <-'syn7315780'
#' pedometerJsonFile <- synGet(sample_walking_pedometer_json_file)@filePath
#' getPedometerFeatures(pedometerJsonFile)

getPedometerFeatures <- function(pedo_json_file) {
  if (is.na(pedo_json_file) == T) {
    null_result = c(rep(NA, 3), error = "no json data file")
    names(null_result) = c("steps_time_in_seconds","numberOfSteps",
                           "distance", "error")
    return(null_result)
  }

  pedoDat <- jsonlite::fromJSON(pedo_json_file)
  if (is.data.frame(pedoDat) == F) {
    return(c(rep(NA, 3), error = "expected data frame after reading pedometer json file"))
  }

  pedoDat <- pedoDat %>% mutate(steps_time_in_seconds = as.numeric(ymd_hms(endDate) -
                                                                     ymd_hms(startDate))) %>% arrange(steps_time_in_seconds) %>%
    dplyr::select(steps_time_in_seconds, numberOfSteps,distance)
  res <- pedoDat[nrow(pedoDat), , drop = T]
  res["error"] = NA
  unlist(res)  # so return type is a character vector in each case
}


##################### Working Example library(synapseclient)
##################### synapseLogin() sample_walking_accel_File
##################### <- 'syn7077340' walking_jsonFile <-
##################### synGet(sample_walking_accel_File)@filePath
##################### #get walk features
##################### getWalkFeatures(walking_jsonFile)
