#' Extract tremor features from raw accelerometer and gyroscope data.
#'
#' @param accelerometer_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements. 
#' @param gyroscope_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements.
#' @param gravity_data A data frame with columns t, x, y, z containing 
#' gravity sensor measurements.
#' @param funs A list of feature extraction functions that accept a single
#' numeric vector as input.
#' @param models A function which accepts as input a dataframe with columns
#' axis, Window, jerk, acceleration, velocity, displacement and
#' outputs features. Useful for models which compute individual statistics
#' using multiple input variables.
#' @param window_length Length of sliding windows for bandpass filter
#' and windowing transformation.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param overlap Window overlap for the windowing transformation.
#' @return A list of feature dataframes. The outputs from \code{funs} will
#' be stored under \code{$extracted_features} and the outputs from \code{models}
#' will be stored under \code{$model_features}.'
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
get_tremor_features <- function(
  accelerometer_data, gyroscope_data, gravity_data = NULL,
  funs = NULL, models = NULL, window_length = 256, time_range = c(1,9),
  frequency_range = c(1, 25), overlap = 0.5) {
  
  features <- list()
  
  # check input integrity
  if (any(is.na(accelerometer_data))) {
    features$error <- dplyr::tibble(error = 'Malformed accelerometer data')
    return(features)
  } else if (any(is.na(gyroscope_data))) {
    features$error <- dplyr::tibble(error = 'Malformed gyroscope data')
    return(features)
  }
  
  # Get accelerometer features
  features_accel <- accelerometer_features(
    sensor_data = accelerometer_data, 
    transformation = transformation_window(window_length = window_length,
                                           overlap = overlap),
    funs = funs,
    models = models,
    window_length = window_length,
    time_range = time_range,
    frequency_range = frequency_range)
  
  # Get gyroscope features
  features_gyro <- gyroscope_features(
    sensor_data = gyroscope_data,
    transformation = transformation_window(window_length = window_length,
                                           overlap = overlap),
    funs = funs,
    models = models,
    window_length = window_length,
    time_range = time_range,
    frequency_range = frequency_range)
  
  # Combine features into a single list
  if (!is.null(funs)) {
    features$extracted_features <- dplyr::bind_rows(
      accelerometer = features_accel$extracted_features,
      gyroscope = features_gyro$extracted_features,
      .id = "sensor")
    # tag outlier windows if there are no other errors
    if (!has_error(features$extracted_features) && !is.null(gravity_data)) {
      gr_error <- tag_outlier_windows(gravity_data, window_length, overlap)
      features$extracted_features <- features$extracted_features %>%
        {if(rlang::has_name(., "error")) dplyr::select(-error) else .} %>% 
        dplyr::left_join(gr_error, by = 'Window')
    }
  }
  if (!is.null(models)) {
    features$model_features <- list(accelerometer = features_accel$model_features,
                                    gyroscope = features_gyro$model_features)
  }
  
  return(features)
}