####### MAIN
#' extracts features from tremor task handToNose accelerometer and gyroscope JSON data file
#'
#'
#' @param tremorJsonFileLoc path to tremor accelerometer json file
#' @return data frame of tremor features
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder 
get_kinetic_tremor_features <- function(
  accelerometer_data, gyroscope_data, gravity_data = NA,
  funs = NA, window_length = 256, time_range = c(1,9), 
  frequency_range = c(1, 25), overlap = 0.5, max_imf = 4) {
  
  features = dplyr::tibble(Window = NA, error = NA)
  
  # check input integrity
  if (any(is.na(accelerometer_data))) {
    features$error = 'Malformed accelerometer data'
    return(features)
  } else if (any(is.na(gyroscope_data))) {
    features$error = 'Malformed gyroscope data'
    return(features)
  }
  
  # Get accelerometer features
  features_accel <- accelerometer_features(
    sensor_data = accelerometer_data,
    transformation = transformation_imf_window(window_length = window_length,
                                               overlap = overlap,
                                               max_imf = max_imf),
    funs = funs,
    groups = c("axis", "IMF", "Window"),
    window_length = window_length,
    overlap = overlap,
    time_range = time_range,
    frequency_range = frequency_range)
  
  # Get gyroscope features
  features_gyro <- gyroscope_features(
    sensor_data = gyroscope_data,
    transformation = transformation_imf_window(window_length = window_length,
                                               overlap = overlap,
                                               max_imf = max_imf),
    funs = funs,
    groups = c("axis", "IMF", "Window"),
    window_length = window_length,
    overlap = overlap,
    time_range = time_range,
    frequency_range = frequency_range)
  
  # Return if processing is errored
  if(has_error(features_accel) || has_error(features_gyro)) {
    return(list(accelerometer = features_accel, gyroscope = features_gyro) %>%
             data.table::rbindlist(use.names = TRUE, fill = T, idcol = 'sensor'))
  }
  
  # Combine all features
  features <- list(accelerometer = features_accel, gyroscope = features_gyro) %>%
    data.table::rbindlist(use.names = TRUE, fill = T, idcol = 'sensor') %>% 
    dplyr::mutate(error = "None")
  
  # Tag outlier windows
  if(suppressWarnings(!is.na(gravity_data))) {
    gr_error <- tag_outlier_windows(gravity_data, window_length, overlap)
    features <- features %>%
      dplyr::select(-error) %>% 
      dplyr::left_join(gr_error, by = 'Window')
  }
  
  return(features)
  
}  