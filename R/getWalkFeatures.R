#' Extract walk features from raw accelerometer and gyroscope data.
#'
#' @param accelerometer_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements. 
#' @param gyrometer_data A data frame with columns t, x, y, z containing 
#' gyrometer measurements. 
#' @param gravity_data A data frame with columns t, x, y, z containing 
#' gravity sensor measurements.
#' @param window_length Length of sliding windows.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param overlap Window overlap.
#' @return Walk features indexed by axis and window.
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
#' @importFrom magrittr "%>%"
get_walk_features <- function(accelerometer_data, gyrometer_data, gravity_data = NA,
                              window_length = 256, frequency_range = c(1, 25),
                              overlap = 0.5) {
  features = dplyr::tibble(Window = NA, error = NA)
  if(any(is.na(accelerometer_data)) || any(is.na(gyrometer_data))) {
    features$error = 'Malformed accelerometer or gyrometer data'
    return(features)
  }
  
  # Get accelerometer features
  features_accel = accelerometer_features(accelerometer_data, window_length, overlap,
                                          frequency_range[1], frequency_range[2])
  # Get gyrometer features
  features_gyro = gyrometer_features(gyrometer_data, window_length, overlap,
                                     frequency_range[1], frequency_range[2])
  
  # Return if processing is errored
  if(!is.na(features_accel$error) || !is.na(features_gyro$error)) {
    return(list(accelerometer = features_accel, gyroscope = features_gyro) %>%
             data.table::rbindlist(use.names = TRUE, fill = T, idcol = 'sensor'))
  }
  
  if(!is.na(gravity_data)) {
    gr_error <- tag_outlier_windows(gravity_data, window_length, overlap)
  } else {
    gr_error <- tibble(window = "NA", error = NA)
  }
  
  # Combine all features
  features = list(accelerometer = features_accel, gyroscope = features_gyro) %>%
    data.table::rbindlist(use.names = TRUE, fill = T, idcol = 'sensor') %>%
    dplyr::mutate(window = as.character(window)) %>%
    dplyr::select(-error) %>% 
    dplyr::left_join(gr_error, by = 'window')
  
  return(features)
}