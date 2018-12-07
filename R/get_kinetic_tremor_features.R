#' Preprocess and extract interpretable features from kinetic tremor assay.
#' 
#' @description \code{get_kinetic_tremor_features()} is a convinient wrapper to 
#' extract interpretable features from the kinetic tremor assay measured using 
#' smartphone raw accelerometer and gyroscope sensors.
#' 
#' @usage 
#' get_kinetic_tremor_features(accelerometer_data, gyroscope_data)
#' 
#' get_kinetic_tremor_features(accelerometer_data = NULL, gyroscope_data = NULL, 
#'   gravity_data = NULL, time_filter = NULL, detrend = F, frequency_filter = NULL, 
#'   IMF = 2, window_length = NULL, window_overlap = NULL, derived_kinematics = F,
#'   funs = NULL, models = NULL)
#' 
#' @param accelerometer_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements. 
#' @param gyroscope_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements. 
#' @param gravity_data A data frame with columns t, x, y, z containing 
#' gravity sensor measurements.
#' @param time_filter A length 2 numeric vector specifying the time range 
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not 
#' filter any measurements.
#' @param detrend A logical value indicating whether to detrend the signal. 
#' By default the value is FALSE.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition (EMD)
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length A numerical value representing the length of the sliding 
#' window used during the windowing transformation. Both \code{window_length} and
#'  \code{window_overlap} must be set for the windowing transformation to be applied.
#' @param window_overlap Fraction between (0, 1) specifying the window overlap used 
#' during the windowing transformation. Note, 1 represents no overlap.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics A logical value specifying whether to add derived 
#' kinematic features like \code{displacement}, \code{velocity}, \code{acceleration},
#' and \code{jerk} from raw \code{accelerometer_data} and 
#' \code{gyroscope_data}.
#' @param funs A function or list of feature extraction functions that each
#' accept a single numeric vector as input. Each function should return a 
#' dataframe of features (normally a single-row datafame). The input vectors
#' will be the axial measurements from \code{sensor_data} after the chosen
#' preprocessing and transformation steps have been applied. If no argument
#' is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A list of functions, each of which accept as input 
#' \code{sensor_data} after the chosen preprocessing and transformation
#' steps have been applied and return features. Useful for models which compute
#' individual statistics using multiple input variables.
#' @return A list. The outputs from \code{funs} will
#' be stored under \code{$extracted_features} and the outputs from \code{models}
#' will be stored under \code{$model_features}. If there is an error 
#' during the transform process, an error dataframe will be stored under
#' \code{$error}. If gravity_data is passed and window_length and 
#' window_overlap are set, phone rotation information will be stored
#' under \code{$outlier_windows}.
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
#' @examples 
#' library(mhealthtools)
#' data("kinetic_tremor_data")
#' 
#' accelerometer_data = cbind(t = kinetic_tremor_data$timestamp, kinetic_tremor_data$userAcceleration)
#' gyroscope_data = cbind(t = kinetic_tremor_data$timestamp, kinetic_tremor_data$rotationRate)
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data)
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, time_filter = c(2,8))
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, detrend = T)
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, frequency_filter = c(0.5, 25))
#' 
#' #' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, IMF = 4)
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, window_length = 512, window_overlap = 0.9)
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, derived_kinematics = F)
#' 
#' kinetic_tremor_ftrs <- get_kinetic_tremor_features(accelerometer_data, gyroscope_data, funs = list(time_domain_summary))
#' @importFrom magrittr "%>%"
get_kinetic_tremor_features <- function(
  accelerometer_data = NULL, gyroscope_data = NULL, gravity_data = NULL,
  time_filter = NULL, detrend = F, frequency_filter = NULL, IMF = 2,
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