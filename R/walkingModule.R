

########################## functions to extract tapping features from
########################## tapping module Computes tapping time
########################## series (tapping interval and tapping
########################## position) Inputs: tapping json file from
########################## the researchKit app - mPower SEE WORKING
########################## EXAMPLE at the END



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

SingleAxisFeatures <- function(x, t, varName) {
    meanX <- mean(x, na.rm = TRUE)
    sdX <- sd(x, na.rm = TRUE)
    modeX <- pracma::Mode(x)
    skewX <- Skewness(x)
    kurX <- Kurtosis(x)
    auxX <- quantile(x, probs = c(0, 0.25, 0.5,
        0.75, 1), na.rm = TRUE)
    q1X <- auxX[[2]]
    medianX <- auxX[[3]]
    q3X <- auxX[[4]]
    iqrX <- q3X - q1X
    rangeX <- auxX[[5]] - auxX[[1]]
    acfX <- acf(x, lag.max = 1, plot = FALSE)$acf[2,
        1, 1]
    zcrX <- ZCR(x)
    dfaX <- fractal::DFA(x, sum.order = 1)[[1]]
    cvX <- Cv(x)
    tkeoX <- MeanTkeo(x)
    lspX <- lomb::lsp(cbind(t, x), plot = FALSE)
    F0X <- lspX$peak.at[1]
    P0X <- lspX$peak
    out <- c(meanX, sdX, modeX, skewX, kurX,
        q1X, medianX, q3X, iqrX, rangeX, acfX,
        zcrX, dfaX, cvX, tkeoX, F0X, P0X)
    nms <- c("mean", "sd", "mode", "skew", "kur",
        "q1", "median", "q3", "iqr", "range",
        "acf", "zcr", "dfa", "cv", "tkeo", "F0",
        "P0")
    names(out) <- paste(nms, varName, sep = "")
    out
}


ShapeGaitData <- function(dat) {
    timestamp = dat$timestamp - dat$timestamp[1]
    userAccel = dat$userAcceleration
    colnames(userAccel) = gsub("userAcceleration\\.",
        "", colnames(userAccel))
    accel_dat <- cbind(timestamp, userAccel)
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
        null_result = c(rep(NA, 88), error = "no json data file")
        names(null_result) = c("meanX", "sdX",
            "modeX", "skewX", "kurX", "q1X",
            "medianX", "q3X", "iqrX", "rangeX",
            "acfX", "zcrX", "dfaX", "cvX", "tkeoX",
            "F0X", "P0X", "meanY", "sdY", "modeY",
            "skewY", "kurY", "q1Y", "medianY",
            "q3Y", "iqrY", "rangeY", "acfY",
            "zcrY", "dfaY", "cvY", "tkeoY",
            "F0Y", "P0Y", "meanZ", "sdZ", "modeZ",
            "skewZ", "kurZ", "q1Z", "medianZ",
            "q3Z", "iqrZ", "rangeZ", "acfZ",
            "zcrZ", "dfaZ", "cvZ", "tkeoZ",
            "F0Z", "P0Z", "meanAA", "sdAA",
            "modeAA", "skewAA", "kurAA", "q1AA",
            "medianAA", "q3AA", "iqrAA", "rangeAA",
            "acfAA", "zcrAA", "dfaAA", "cvAA",
            "tkeoAA", "F0AA", "P0AA", "meanAJ",
            "sdAJ", "modeAJ", "skewAJ", "kurAJ",
            "q1AJ", "medianAJ", "q3AJ", "iqrAJ",
            "rangeAJ", "acfAJ", "zcrAJ", "dfaAJ",
            "cvAJ", "tkeoAJ", "F0AJ", "P0AJ",
            "corXY", "corXZ", "corYZ", "error")
        return(null_result)
    }

    dat <- jsonlite::fromJSON(walking_json_file)
    dat <- ShapeGaitData(dat)
    x <- dat$x
    y <- dat$y
    z <- dat$z
    aa <- sqrt(x^2 + y^2 + z^2)
    aj <- sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)
    time <- dat$timestamp
    ###############################
    outX <- SingleAxisFeatures(x, time, varName = "X")
    outY <- SingleAxisFeatures(y, time, varName = "Y")
    outZ <- SingleAxisFeatures(z, time, varName = "Z")
    outAA <- SingleAxisFeatures(aa, time, varName = "AA")
    outAJ <- SingleAxisFeatures(aj, time[-1],
        varName = "AJ")
    ###############################
    corXY <- cor(x, y, use = "p")
    corXZ <- cor(x, z, use = "p")
    corYZ <- cor(z, y, use = "p")
    cors <- c(corXY, corXZ, corYZ)
    names(cors) <- c("corXY", "corXZ", "corYZ")
    c(outX, outY, outZ, outAA, outAJ, cors,
        error = NA)
}

getPedometerFeatures <- function(pedo_json_file) {
    if (is.na(pedo_json_file) == T) {
        null_result = c(rep(NA, 5), error = "no json data file")
        names(null_result) = c("floorsAscended",
            "floorsDescended", "steps_time_in_seconds",
            "numberOfSteps", "distance", "error")
        return(null_result)
    }

    pedoDat <- jsonlite::fromJSON(pedo_json_file)
    if (is.data.frame(pedoDat) == F) {
        return(c(rep(NA, 5), error = "expected data frame after reading pedometer json file"))
    }

    pedoDat <- pedoDat %>% mutate(steps_time_in_seconds = as.numeric(ymd_hms(endDate) -
        ymd_hms(startDate))) %>% arrange(steps_time_in_seconds) %>%
        dplyr::select(floorsAscended, floorsDescended,
            steps_time_in_seconds, numberOfSteps,
            distance)
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

