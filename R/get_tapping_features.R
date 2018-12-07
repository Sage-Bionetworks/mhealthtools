#' Wrapper functionality to preprocess and extract interpretable features from screen tapping data.
#'
#' @param tap_data A n x 4 data frame with column names as t, x, y, buttonid containing 
#' tapping measurements. Here n is the total number of tapping measurements, t is the time 
#' of measurement when the screen press was recorded, x and y are the coordinates of the screen 
#' touch in the smartphone, and buttonid can be one of the following values c('TappedButtonLeft',
#' 'TappedButtonRight','TappedButtonNone') indicating a tap that has been classified as to the left,
#' right or neither of those places on the screen
#' @param depress_threshold A numeric value indicating the threshold between two successive taps
#' @param remove_duplicates A logical value indicating if duplicates in taps considered as None 
#' (i.e buttonid is neither left nor right) need to be removed or not
#' @return A 1 x 43 data frame consisting of tapping features and an error message, if any
#' See https://github.com/Sage-Bionetworks/mhealthtools/blob/master/FeatureDefinitions.md for
#' feature definitions
#' @examples 
#' library(mhealthtools)
#' data("tap_data")
#' tap_features = get_tapping_features(tap_data)
#' 
#' tap_features = get_tapping_features(tap_data, depress_threshold = 120)
#' 
#' tap_features = get_tapping_features(tap_data, remove_duplicates = F)
#' @export
#' @author Elias Chaibub Neto, Meghasyam Tummalacherla, Phil Snyder, Thanneer M Perumal
get_tapping_features <- function(tap_data, 
                                 depress_threshold = 20,
                                 remove_duplicates = TRUE){
  if (!is.data.frame(tap_data)) {
    tap_features <- dplyr::tibble(error = "sensor data should be a dataframe")
  } else  if (nrow(tap_data) < 5) {
    tapFeatures <- dplyr::tibble(error = "tapping data has less than 5 rows")
  } else if (ncol(tap_data) != 4){
    tapFeatures <- dplyr::tibble(error = "tapping data has not equal to 4 columns")
  } else {
    # remove duplicate data points // if selected
    if (remove_duplicates) { 
      tap_data <- clean_tapped_button_none(tap_data = tap_data)
    }
    
    # check if cleaned data has < 5 rows
    if (nrow(tap_data) < 5) {
      tap_features <- dplyr::tibble(error = "sensor data has less than 5 unique rows")
    } else {
      tap_features <- tapping_features(tap_data = tap_data,
                                       depress_threshold = depress_threshold)
    }
  }
  return(tap_features %>% as.data.frame())
}


#' Remove duplicates in the given dataframe tap_data which have the buttonid parameter as 'TappedButtonNone'
#' 
#' @param tap_data A n x 4 data frame with column names as t, x, y, buttonid containing 
#' tapping measurements. Here n is the total number of tapping measurements, t is the time 
#' of measurement when the screen press was recorded, x and y are the coordinates of the screen 
#' touch in the smartphone, and buttonid can be one of the following values c('TappedButtonLeft',
#' 'TappedButtonRight','TappedButtonNone') indicating a tap that has been classified as to the left,
#' right or neither of those places on the screen
#' @return A dataframe with duplicates corresponding to buttonid == 'TappedButtonNone' removed

clean_tapped_button_none <- function(tap_data) {
  # Get separate dataframes for taps on left, right buttons, and None
  tap_left_right <- tap_data %>%
    dplyr::filter(buttonid %in% c("TappedButtonLeft", "TappedButtonRight"))
  tap_none <- tap_data %>%
    dplyr::filter(buttonid == "TappedButtonNone")
  
  # We only want to drop TappedButtonNone duplications
  duplicated_none <- duplicated(tap_none %>% dplyr::select(x, y))
  tap_none <- tap_none[which(!duplicated_none),]
  
  # Get cleaned data
  tap_data <- rbind(tap_left_right, tap_none)
  tap_data <- tap_data[order(tap_data$t),]
  return(tap_data)
}

#' Extract tapping (screen sensor) features
#' 
#' @param tap_data A n x 4 data frame with column names as t, x, y, buttonid containing 
#' tapping measurements. Here n is the total number of tapping measurements, t is the time 
#' of measurement when the screen press was recorded, x and y are the coordinates of the screen 
#' touch in the smartphone, and buttonid can be one of the following values c('TappedButtonLeft',
#' 'TappedButtonRight','TappedButtonNone') indicating a tap that has been classified as to the left,
#' right or neither of those places on the screen
#' @param depress_threshold A numeric value indicating the threshold between two successive taps
#' @return A 1 x 43 data frame consisting of tapping features and an error message, if any
#' See https://github.com/Sage-Bionetworks/mhealthtools/blob/master/FeatureDefinitions.md for
#' feature definitions

tapping_features <- function(tap_data, 
                             depress_threshold = 20) {
  results <- get_left_right_events_and_tap_intervals(tap_data = tap_data, 
                                                     depress_threshold = depress_threshold)
  tap_intervals <- results$tap_intervals
  tap_data <- results$tap_data
  error <- results$error
  
  # check error - if after cleaning tapping data less than 5 data points remain
  if (error) {
    tap_features <- dplyr::tibble(error = "post cleaning less than 5 tap points remain")
    return(tap_features)
  }
  
  mean_tap_data_x <- mean(tap_data$x)
  interval_left <- tap_data$x < mean_tap_data_x
  interval_right <- tap_data$x >= mean_tap_data_x
  drift_left <- calculate_drift(x = tap_data[interval_left, "x"],
                                y = tap_data[interval_left, "y"])
  drift_right <- calculate_drift(x = tap_data[interval_right, "x"],
                                 y = tap_data[interval_right, "y"])
  
  intertap_features <- intertap_summary_features(tap_intervals = tap_intervals)
  if (intertap_features$error == "None") {
    intertap_features <- intertap_features %>% dplyr::select(-error)
    colnames(intertap_features) <- paste0(colnames(intertap_features),
                                          "TapInter")
  } else {
    colnames(intertap_features) <- paste0(colnames(intertap_features),
                                          "TapInter")
  }
  
  tapdrift_left_features <- tapdrift_summary_features(tap_drift = drift_left)
  if (tapdrift_left_features$error == "None") {
    tapdrift_left_features <- tapdrift_left_features %>% dplyr::select(-error)
    colnames(tapdrift_left_features) <- paste0(colnames(tapdrift_left_features),
                                               "DriftLeft")
  } else {
    colnames(tapdrift_left_features) <- paste0(colnames(tapdrift_left_features),
                                               "DriftLeft")
  }
  
  tapdrift_right_features <- tapdrift_summary_features(tap_drift = drift_right)
  if (tapdrift_right_features$error == "None") {
    tapdrift_right_features <- tapdrift_right_features %>% dplyr::select(-error)
    colnames(tapdrift_right_features) <- paste0(
      colnames(tapdrift_right_features), "DriftRight")
  } else {
    colnames(tapdrift_right_features) <- paste0(
      colnames(tapdrift_right_features), "DriftRight")
  }
  
  tap_data_features <- tap_data_summary_features(tap_data = tap_data)
  if (tap_data_features$error == "None") {
    tap_data_features <- tap_data_features %>% dplyr::select(-error)
  }
  tap_features <- dplyr::bind_cols(intertap_features,
                                   tapdrift_left_features,
                                   tapdrift_right_features,
                                   tap_data_features)
  
  ftrs_error <- grep("error", colnames(tap_features))
  ftrs_error <- paste(tap_features[ftrs_error], collapse = " ; ")
  if (ftrs_error == "") {
    ftrs_error <- "None"
  }
  
  tap_features$error <- ftrs_error
  tap_features <- tap_features %>%
    as.data.frame()
  return(tap_features)
}