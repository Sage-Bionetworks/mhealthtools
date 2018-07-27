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


