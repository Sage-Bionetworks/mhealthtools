#' Extract tapping features from raw tapping data.
#'
#' @param tap_data A data frame with columns t, x, y, buttonid containing 
#' tapping measurements. buttonid can be from c('TappedButtonLeft','TappedButtonRight','TappedButtonNone') 
#' indicating a tap that has been classified as to the left, right or neither of those places on the screen
#' @param depressThr A numeric value indicating the threshold between 
#' taps (better description needed)
#' @param removeDups A logical value indicating if duplicates in taps considered as None 
#' (i.e buttonid is neither left nor right) need to be removed or not
#' @return Tapping features 
#' @export
#' @author Elias Chaibub Neto, Meghasyam Tummalacherla
get_tapping_features <- function(tap_data, depressThr=20, removeDups=TRUE) {
  if (!is.data.frame(tap_data)) {
    tapFeatures <- dplyr::tibble(error = "expected data frame object")
  } else if (nrow(tap_data) < 5) {
    tapFeatures <- dplyr::tibble(error = "raw tapping data has less than 5 rows")
  } else {
    
    #remove duplicate data points // if selected
    if (removeDups){
      tap_data <- clean_tapped_button_none(tap_data = tap_data)
    }
    
    #check if cleaned data has < 5 rows
    if (nrow(tap_data) < 5) {
      tapFeatures <- dplyr::tibble(error="post duplication removal tapping data has less than 5 rows")
    } else {
      # compute the tapping features
      tapFeatures <- tapping_features(tap_data = tap_data,
                                      depressThr = depressThr)
    }
  }
  return(tapFeatures %>% as.data.frame())
}


#' Remove duplicates in the given dataframe tap_data which have the buttonid parameter as 'TappedButtonNone'
#' 
#' @param tap_data A dataframe with colums t,x,y and buttonid
#' @return A dataframe with duplicates corresponding to buttonid == 'TappedButtonNone' removed
clean_tapped_button_none <- function(tap_data) {
  # Get seperate dataframes for taps on left,right buttons, and None
  tapLeftRight <- tap_data %>% 
    dplyr::filter(buttonid %in% c('TappedButtonLeft','TappedButtonRight'))
  tapNone <- tap_data %>% 
    dplyr::filter(buttonid == 'TappedButtonNone')
  
  # we only want to drop TappedButtonNone duplications
  dupli <- duplicated(tapNone %>% 
                        dplyr::select(x,y))
  tapNone <- tapNone[which(!dupli),] # now we remove on the duplicated data from taps outside the buttons
  
  # Get cleaned data
  tap_data <- rbind(tapLeftRight,tapNone) # Combine left right and none tapzone data
  tap_data <- tap_data[order(tap_data$t),] # order the data according to time
  return(tap_data)
}

#' Extract tapping (screen sensor) features
#' 
#' @param tap_data A data frame with columns t, x, y, buttonid containing 
#' tapping measurements. buttonid can be from c('TappedButtonLeft','TappedButtonRight','TappedButtonNone') 
#' indicating a tap that has been classified as to the left, right or neither of those places on the screen
#' @param depressThr A numerical threshold for intertap distance in x axis
#' @return A dataframe of features.
tapping_features <- function(tap_data,
                             depressThr = 20) {
  
  results <- get_left_right_events_and_tap_intervals(tapData = tap_data,
                                                     depressThr = depressThr)
  tapInter <- results$tapInter
  tapData <- results$tapData
  error <- results$error
  
  # check error - if after cleaning tapping data less than 5 data points remain
  if (error) {
    tapFeatures <- dplyr::tibble(error = "post cleaning less than 5 tap points remain")
    return(tapFeatures)
  }
  
  meanX <- mean(tapData$x)
  iL <- tapData$x < meanX
  iR <- tapData$x >= meanX
  driftLeft <- calculate_drift(x = tapData[iL,"x"], y = tapData[iL, "y"])
  driftRight <- calculate_drift(x = tapData[iR,"x"], y = tapData[iR, "y"])
  
  intertap_features <- intertap_summary_features(tapInter = tapInter)
  if(intertap_features$error == 'None'){
    intertap_features <- intertap_features %>% dplyr::select(-error)
    colnames(intertap_features) <- paste0(colnames(intertap_features),'TapInter')
  }else{
    colnames(intertap_features) <- paste0(colnames(intertap_features),'TapInter')
  }
  
  tapdrift_left_features <- tapdrift_summary_features(tapDrift = driftLeft)
  if(tapdrift_left_features$error == 'None'){
    tapdrift_left_features <- tapdrift_left_features %>% dplyr::select(-error)
    colnames(tapdrift_left_features) <- paste0(colnames(tapdrift_left_features),'DriftLeft')
  }else{
    colnames(tapdrift_left_features) <- paste0(colnames(tapdrift_left_features),'DriftLeft')
  }
  
  tapdrift_right_features <- tapdrift_summary_features(tapDrift = driftRight)
  if(tapdrift_right_features$error == 'None'){
    tapdrift_right_features <- tapdrift_right_features %>% dplyr::select(-error)
    colnames(tapdrift_right_features) <- paste0(colnames(tapdrift_right_features),'DriftRight')
  }else{
    colnames(tapdrift_right_features) <- paste0(colnames(tapdrift_right_features),'DriftRight')
  }
  
  tapdata_features <- tap_data_summary_features(tapData = tap_data)
  if(tapdata_features$error == 'None'){
    tapdata_features <- tapdata_features %>% dplyr::select(-error)
  }
  
  tapFeatures <- dplyr::bind_cols(intertap_features,
                                  tapdrift_left_features,
                                  tapdrift_right_features,
                                  tapdata_features)
  
  ftrs_error <- grep('error', colnames(tapFeatures))
  ftrs_error <- paste(tapFeatures[ftrs_error], collapse = ' ; ')
  if(ftrs_error == ''){
    ftrs_error = 'None'
  }
  
  tapFeatures$error <- ftrs_error
  tapFeatures <- tapFeatures %>% 
    as.data.frame()
  return(tapFeatures)
}

