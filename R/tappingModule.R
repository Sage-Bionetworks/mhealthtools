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

shapeTappingData <- function(tapData) {
    tapData <- dplyr::mutate(tapData, TapCoordinate = stringr::str_replace_all(TapCoordinate,"[{}}]", "")) %>%
      tidyr::separate(TapCoordinate,into = c("X", "Y"), sep = ",") %>%
      dplyr::mutate(X = as.numeric(X),Y = as.numeric(Y)) %>%
      dplyr::mutate(time = TapTimeStamp,buttonid = TappedButtonId) %>%
      dplyr::select(time,X, Y, buttonid)
    return(tapData)
}


CleanTappedButtonNone <- function(x) {
  il <- x$buttonid == "TappedButtonLeft" ## get indexes of taps on left button
  ir <- x$buttonid == "TappedButtonRight" ## get indexes of taps on right button
  ino <- x$buttonid == "TappedButtonNone" ## get indexes of taps outside the button
  xx <- rbind(x[il,], x[ir,], x[ino,]) ## create new matrix where the data from taps outside the button is at the bottom
  dupli <- duplicated(cbind(xx$X, xx$Y)) ## determine which data is duplicated
  ## we only want to drop TappedButtonNone duplications
  ## so we force a FALSE for data corresponding to taps on the right and left buttons
  nlr <- sum(il) + sum(ir)
  dupli[seq(nlr)] <- FALSE
  ############################
  xx <- xx[which(!dupli),] ## now we remove on the duplicated data from taps outside the buttons
  xx[order(xx[, 1]),] ## order the data according to time
}



GetLeftRightEventsAndTapIntervals <- function(tapData, depressThr = 0) {
    tapTime <- tapData$time - tapData$time[1]
    ## calculate X offset
    tapX <- tapData$X - mean(tapData$X)
    ## find left/right finger 'depress' event
    dX <- diff(tapX)
    i <- c(1, which(abs(dX) > depressThr) + 1)
    ## filter data
    tapData <- tapData[i, ]
    tapTime <- tapTime[i]
    ## find depress event intervals
    tapInter <- diff(tapTime)

    ### ERROR CHECK -
    if (nrow(tapData) >= 5) {
        list(tapData = tapData, tapInter = tapInter,
            error = "None")
    } else {
        list(tapData = NA, tapInter = NA, error = TRUE)
    }
}

Fatigue <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    top10 <- round(0.1 * n)
    top25 <- round(0.25 * n)
    top50 <- floor(0.5 * n)
    list(fatigue10 = mean(x[1:top10]) - mean(x[(n -top10):n]),
         fatigue25 = mean(x[1:top25]) - mean(x[(n - top25):n]),
         fatigue50 = mean(x[1:top50]) - mean(x[(n - top50):n]))
}

ComputeTappingFeatures <- function(tapData,
    depressThr = 20) {
    results <- GetLeftRightEventsAndTapIntervals(tapData,
        depressThr)
    tapInter <- results$tapInter
    tapData <- results$tapData
    error <- results$error

    # check error - if after cleaning tapping
    # data less than 5 points remains
    if (error == TRUE) {
        tapFeatures <- createTappingFeaturesErrorResult("post cleaning less than 5 tap points remain")
        return(tapFeatures)
    }

    meanX <- mean(tapData$X)
    iL <- tapData$X < meanX
    iR <- tapData$X >= meanX
    driftLeft <- calculateDrift(tapData[iL,"X"], tapData[iL, "Y"])
    driftRight <- calculateDrift(tapData[iR,"X"], tapData[iR, "Y"])

    # determine Autocorrelation
    auxAcf <- try(acf(tapInter, lag.max = 2,
        plot = FALSE)$acf, silent = TRUE)
    if (inherits(auxAcf, "try-error")) {
        auxAcf <- list(NA, NA, NA)
    }
    auxFatigue <- Fatigue(tapInter)

    # other features
    data.frame(meanTapInter = mean(tapInter,na.rm = TRUE),
        medianTapInter = median(tapInter, na.rm = TRUE),
        iqrTapInter = IQR(tapInter, type = 7, na.rm = TRUE),
        minTapInter = min(tapInter,na.rm = TRUE),
        maxTapInter = max(tapInter, na.rm = TRUE),
        skewTapInter = Skewness(tapInter),
        kurTapInter = Kurtosis(tapInter),
        sdTapInter = sd(tapInter,na.rm = TRUE),
        madTapInter = mad(tapInter, na.rm = TRUE),
        cvTapInter = Cv(tapInter),
        rangeTapInter = diff(range(tapInter,na.rm = TRUE)),
        tkeoTapInter = MeanTkeo(tapInter),
        ar1TapInter = auxAcf[[2]], ar2TapInter = auxAcf[[3]],
        fatigue10TapInter = auxFatigue[[1]],
        fatigue25TapInter = auxFatigue[[2]],
        fatigue50TapInter = auxFatigue[[3]],
        meanDriftLeft = mean(driftLeft, na.rm = TRUE),
        medianDriftLeft = median(driftLeft,
            na.rm = TRUE), iqrDriftLeft = IQR(driftLeft,
            type = 7, na.rm = TRUE), minDriftLeft = min(driftLeft,
            na.rm = TRUE), maxDriftLeft = max(driftLeft,
            na.rm = TRUE), skewDriftLeft = Skewness(driftLeft),
        kurDriftLeft = Kurtosis(driftLeft),
        sdDriftLeft = sd(driftLeft, na.rm = TRUE),
        madDriftLeft = mad(driftLeft, na.rm = TRUE),
        cvDriftLeft = Cv(driftLeft), rangeDriftLeft = diff(range(driftLeft,
            na.rm = TRUE)), meanDriftRight = mean(driftRight,
            na.rm = TRUE), medianDriftRight = median(driftRight,
            na.rm = TRUE), iqrDriftRight = IQR(driftRight,
            type = 7, na.rm = TRUE), minDriftRight = min(driftRight,
            na.rm = TRUE), maxDriftRight = max(driftRight,
            na.rm = TRUE), skewDriftRight = Skewness(driftRight),
        kurDriftRight = Kurtosis(driftRight),
        sdDriftRight = sd(driftRight, na.rm = TRUE),
        madDriftRight = mad(driftRight, na.rm = TRUE),
        cvDriftRight = Cv(driftRight), rangeDriftRight = diff(range(driftRight,
            na.rm = TRUE)), numberTaps = nrow(tapData),
        buttonNoneFreq = sum(tapData$buttonid ==
            "TappedButtonNone")/nrow(tapData),
        corXY = cor(tapData$X, tapData$Y, use = "p"),
        error = "None")
}

readTappingFile <- function(tappingJsonFile) {
    tryCatch({
        tapData <- jsonlite::fromJSON(tappingJsonFile)
        list(data = tapData, error = FALSE)
    }, error = function(err) {
        list(data = NA, error = TRUE)
    })
}


createTappingFeaturesErrorResult <- function(error) {
    df <- data.frame(t(c(rep(NA, 42), error)))
    colnames(df) <- c("meanTapInter", "medianTapInter",
        "iqrTapInter", "minTapInter", "maxTapInter",
        "skewTapInter", "kurTapInter", "sdTapInter",
        "madTapInter", "cvTapInter", "rangeTapInter",
        "tkeoTapInter", "ar1TapInter", "ar2TapInter",
        "fatigue10TapInter", "fatigue25TapInter",
        "fatigue50TapInter", "meanDriftLeft",
        "medianDriftLeft", "iqrDriftLeft", "minDriftLeft",
        "maxDriftLeft", "skewDriftLeft", "kurDriftLeft",
        "sdDriftLeft", "madDriftLeft", "cvDriftLeft",
        "rangeDriftLeft", "meanDriftRight",
        "medianDriftRight", "iqrDriftRight",
        "minDriftRight", "maxDriftRight", "skewDriftRight",
        "kurDriftRight", "sdDriftRight", "madDriftRight",
        "cvDriftRight", "rangeDriftRight", "numberTaps",
        "buttonNoneFreq", "corXY", "error")
    df
}




####### MAIN
#' extracts tapping features from Tapping JSON data file
#'
#'
#' @param tappingJsonFile path to tapping json file
#' @return data frame of tapping features
#' @export
#' @examples
#' library(synapseClient)
#' synapseLogin()
#' sample_Tapping_File <-'syn7067514'
#' tappingJsonFile <- synGet(sample_Tapping_File)@filePath
#' getTappingFeatures(tappingJsonFile, depressThr = 0, removeDups=T)
getTappingFeatures <- function(tappingJsonFile, depressThr=20, removeDups=T) {
    tapData <- readTappingFile(tappingJsonFile)
    error <- tapData$error
    tapData <- tapData$data
    if (error == T) {
        tapFeatures <- createTappingFeaturesErrorResult("unable to read JSON file")
    } else if (is.data.frame(tapData) == F) {
        tapFeatures <- createTappingFeaturesErrorResult("expected data frame object after reading tapping json file")
    } else if (nrow(tapData) < 5) {
        tapFeatures <- createTappingFeaturesErrorResult("raw tapping data has less than 5 rows")
    } else {

        # shape data
        tapData <- shapeTappingData(tapData)

        #remove duplicate data points // if selected
        if (removeDups == TRUE){
          tapData <- CleanTappedButtonNone(tapData)
        }

        #check if cleaned data has < 5 rows
        if (nrow(tapData) < 5) {
          tapFeatures <- createTappingFeaturesErrorResult("post duplication removal tapping data has less than 5 rows")
        } else {
          # compute the tapping features
          tapFeatures <- ComputeTappingFeatures(tapData,depressThr)
        }
    }

    tapFeatures
}


