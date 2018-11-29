#' Extract walk features from raw accelerometer and gyroscope data.
#'
#' @param accelerometer_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements. 
#' @param gyroscope_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements. 
#' @param gravity_data A data frame with columns t, x, y, z containing 
#' gravity sensor measurements.
#' @param funs Feature extraction functions that accept a single
#' time-series vector as input. 
#' @param models A function which accepts as input a dataframe with columns
#' axis, window, IMF, jerk, acceleration, velocity, displacement and
#' outputs features. Useful for models which compute individual statistics
#' using multiple input variables.
#' @param window_length Length of sliding windows for bandpass filter
#' and windowing transformation.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param overlap Window overlap for the windowing transformation.
#' @param max_imf Number of intrinsic mode functions to use for 
#' empirical mode decomposition.
#' @return A list of feature dataframes. The outputs from \code{funs} will
#' be stored under \code{$extracted_features} and the outputs from \code{models}
#' will be stored under \code{$model_features}.'
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
#' @importFrom magrittr "%>%"
get_walk_features <- function(
  accelerometer_data = NULL, gyroscope_data = NULL, gravity_data = NULL,
  time_filter = NULL, detrend = F, frequency_filter = NULL, IMF = 1,
  window_length = NULL, window_overlap = NULL, derived_kinematics = F,
  funs = NULL, models = NULL) {
  
  features <- list(extracted_features = NULL,
                   model_features = NULL,
                   error = NULL,
                   outlier_windows = NULL)
  
  # check input integrity
  if (!is.null(accelerometer_data) && any(is.na(accelerometer_data))) {
    features$error <- dplyr::tibble(error = "Malformed accelerometer data")
    return(features)
  } else if (!is.null(gyroscope_data) && any(is.na(gyroscope_data))) {
    features$error <- dplyr::tibble(error = "Malformed gyroscope data")
    return(features)
  }
  
  # Get accelerometer features
  if (!is.null(accelerometer_data)) {
    features_accel <- accelerometer_features(
      sensor_data = accelerometer_data,
      time_filter = time_filter,
      detrend = detrend,
      frequency_filter = frequency_filter,
      IMF = IMF,
      window_length = window_length,
      window_overlap = window_overlap,
      derived_kinematics = derived_kinematics,
      funs = funs,
      models = models)
  } else {
    features_accel <- list()
  }
  
  # Get gyroscope features
  if (!is.null(gyroscope_data)) {
    features_gyro <- gyroscope_features(
      sensor_data = gyroscope_data,
      time_filter = time_filter,
      detrend = detrend,
      frequency_filter = frequency_filter,
      IMF = IMF,
      window_length = window_length,
      window_overlap = window_overlap,
      derived_kinematics = derived_kinematics,
      funs = funs,
      models = models)
  } else {
    features_gyro <- list()
  }
  
  # Combine features into a single list
  if (!is.null(features_accel$extracted_features) ||
      !is.null(features_gyro$extracted_features)) {
    features$extracted_features <- dplyr::bind_rows(
      accelerometer = features_accel$extracted_features,
      gyroscope = features_gyro$extracted_features,
      .id = "sensor")
  } else if (!is.null(features_accel$error) ||
             !is.null(features_gyro$error)) {
    features$error <- dplyr::bind_rows(
      accelerometer = features_accel$error,
      gyroscope = features_gyro$error,
      .id = "sensor")
  }
  if (!is.null(features_accel$model_features) ||
      !is.null(features_gyro$model_features)) {
    features$model_features <- list(
      accelerometer = features_accel$model_features,
      gyroscope = features_gyro$model_features)
  }
  
  # tag outlier windows if there was a windowing transformation performed
  if (!is.null(features$extracted_features) && !is.null(gravity_data) &&
      !is.null(window_length) && !is.null(window_overlap)) {
    features$outlier_windows <- tag_outlier_windows(
      gravity = gravity_data,
      window_length = window_length,
      window_overlap = window_overlap)
  }
  
  return(features)
}