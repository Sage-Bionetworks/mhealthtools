#' Wrapper functionality to preprocess and extract interprettable features from 
#' walk assay measured using smartphone raw accelerometer and gyroscpe sensors.
#'
#' @param accelerometer_data A n x 4 data frame with column names as t, x, y, z 
#' containing accelerometer measurements. Here n is the total number of measurements, 
#' t is the time of measurement, x, y and z are the linear acceleration excluding 
#' gravity in their respective coordinates of the smartphone tri-axial accelerometers. 
#' x, y, and z co-ordinates are wrt phones axis as reference
#' @param gyroscope_data A n x 4 data frame with column names as t, x, y, z containing 
#' gyroscope measurements. Here n is the total number of measurements, t is the time 
#' of measurement, x, y and z are the angular velocity excluding gravity in their 
#' respective coordinates of the smartphone gyroscopes. x, y, and z co-ordinates are wrt 
#' phones axis as reference
#' @param gravity_data A n x 4 data frame with column names as t, x, y, z containing 
#' gravity estimations. Here n is the total number of measurements, t is the time of 
#' measurement, x, y and z are the linear acceleration measuring gravity components in
#' their respective coordinates of the smartphone gyroscopes. x, y, and z are wrt 
#' phones axis as reference
#' @param funs Multiple feature extraction functions as lists. By default we provide three 
#' functionalities including \code{time_domain_summary}, \code{frequency_domain_summary},
#' and \code{frequency_domain_energy}. Check extending Extending vignette for how to desing 
#' your own feature extraction functions.
#' @param models A function which accepts as input a dataframe with columns axis, window, 
#' IMF, jerk, acceleration, velocity, displacement and outputs features. Useful for models
#' which compute individual statistics using multiple input variables.
#' @param window_length Length of Hamming sliding windows for bandpass filter and windowing 
#' transformation. The default should only be changed by expert users. 
#' @param time_range Time range of the assay to use in s. We suggest using c(1, 9) for 
#' Sage Bionetworks specific assays. The default should only be changed by expert users.
#' @param frequency_range Frequency range for the bandpass filter in Hz. We suggest to use 
#' c(1, 25) for sage Bionetworks specific assays. The default should only be changed by expert 
#' users.
#' @param overlap Window overlap for the Hamming windowing transformation.
#' @param max_imf Number of intrinsic mode functions to use for empirical mode decomposition. 
#' Specifying 1 gives no emprical mode decomposition.
#' @return A list of feature dataframes for each \code{funs} feature extraction function will 
#' be stored under \code{$extracted_features} and the outputs from \code{models} will be stored under 
#' \code{$model_features}. See https://github.com/Sage-Bionetworks/mhealthtools/blob/master/FeatureDefinitions.md 
#' for feature definitions from the defualt functions
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
#' @examples 
#' library(mhealthtools)
#' data("accelerometer_data")
#' data(gyroscope_data)
#' walk_ftrs <- get_walk_features(accelerometer_data, gyroscope_data)
#' 
#' walk_ftrs <- get_walk_features(accelerometer_data, gyroscope_data, time_range = c(2,8))
#' 
#' walk_ftrs <- get_walk_features(accelerometer_data, gyroscope_data, time_range = c(2,8), frequency_range = c(1,25))
#' 
#' walk_ftrs <- get_walk_features(accelerometer_data, gyroscope_data, overlap = 0.3)
#' 
#' walk_ftrs <- get_walk_features(accelerometer_data, gyroscope_data, max_imf = 1)
#' @importFrom magrittr "%>%"
get_walk_features <- function(
  accelerometer_data, gyroscope_data, gravity_data = NULL,
  funs = list(frequency_domain_summary, time_domain_summary, frequency_domain_energy), 
  models = NULL, window_length = 256, time_range = c(1,9),
  frequency_range = c(1,25), overlap = 0.5, max_imf = 4) {
  
  features <- list()
  
  # check input integrity
  if (any(is.na(accelerometer_data))) {
    features$error <- dplyr::tibble(error = "Malformed accelerometer data")
    return(features)
  } else if (any(is.na(gyroscope_data))) {
    features$error <- dplyr::tibble(error = "Malformed gyroscope data")
    return(features)
  }
  
  # Get accelerometer features
  features_accel <- accelerometer_features(
    sensor_data = accelerometer_data,
    transformation = transformation_imf_window(window_length = window_length,
                                               overlap = overlap,
                                               max_imf = max_imf),
    funs = funs,
    models = models,
    window_length = window_length,
    time_range = time_range,
    frequency_range = frequency_range)
  
  # Get gyroscope features
  features_gyro <- gyroscope_features(
    sensor_data = gyroscope_data,
    transformation = transformation_imf_window(window_length = window_length,
                                               overlap = overlap,
                                               max_imf = max_imf),
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
    if (!has_error(features$extracted_features) && !is.null(gravity_data) &&
        rlang::has_name(features$extracted_features, "window")) {
      gr_error <- tag_outlier_windows(gravity_data, window_length, overlap)
      features$extracted_features <- features$extracted_features %>%
        {if (rlang::has_name(., "error")) dplyr::select(-error) else .} %>%
        dplyr::left_join(gr_error, by = "window")
    }
  }
  if (!is.null(models)) {
    features$model_features <- list(
      accelerometer = features_accel$model_features,
      gyroscope = features_gyro$model_features)
  }
  
  return(features)
}