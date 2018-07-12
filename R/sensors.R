#' Extract features from sensor data
#' 
#' A 'pure' implementation of the feature extraction process. This function
#' is not normally called directly.
#' 
#' The feature extraction paradigm is implemented as such:
#' 
#' Input: raw feature data
#' Transform: into a format suitable for calculating statistics upon
#' Extract: features by calculating statistics upon individual, grouped columns
#' Return: statistics/features for each group
#' 
#' @param sensor_data A dataframe.
#' @param transform A function which accepts \code{sensor_data} as input
#' and outputs a dataframe.
#' @param extract A list of functions to be applied to each of the columns
#' in \code{extract_on} from the output of \code{transform}.
#' @param extract_on A list of column names to calculate statistics from.
#' @param groups A list of column names to group upon when calculating statistics.
#' @return A dataframe of features
sensor_features <- function(sensor_data, transform, extract, extract_on, groups) {
  transformed_sensor_data <- transform(sensor_data)
  if (has_error(transformed_sensor_data)) return(transformed_sensor_data)
  features <- purrr::map_dfr(
    extract_on,
    ~ extract_features(transformed_sensor_data, ., groups, extract))
  return(features)
}

#' Extract kinematic sensor features
#' 
#' Extract kinematic (accelerometer/gyroscope) features. This function is not 
#' normally called directly. See \code{accelerometer_features} and \code{gyroscope_features}.
#' 
#' @param sensor_data A dataframe.
#' @param transform A function which accepts \code{sensor_data} as input
#' and outputs a dataframe.
#' @param extract A list of functions to be applied to each of the columns
#' in \code{extract_on} from the output of \code{transform}.
#' @param extract_on A list of column names to calculate statistics from.
#' @param groups A list of column names to group upon when calculating statistics.
#' @param acf_col Column name to calculate acf upon.
#' @return A dataframe of features.
kinematic_sensor_features <- function(sensor_data, transform, extract, 
                                      extract_on, groups, acf_col) {
  transformed_sensor_data <- transform(sensor_data)
  if (has_error(transformed_sensor_data)) return(sensor_data)
  movement_features <- sensor_features(
    sensor_data = transformed_sensor_data,
    transform = function(x) x,
    extract = extract,
    extract_on = extract_on,
    groups = groups)
  acf_features <- sensor_features(
    sensor_data = transformed_sensor_data,
    transform = purrr::partial(calculate_acf, col = acf_col, groups = groups),
    extract = extract,
    extract_on = "acf",
    groups = groups)
  all_features <- dplyr::bind_rows(movement_features, acf_features)
  return(all_features)
}

#' Extract accelerometer features
#' 
#' Extract accelerometer features. This function is not normally called
#' directly. See \code{accelerometer_features}.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements.
#' @param transform A function which accepts \code{sensor_data} as input
#' and outputs a dataframe.
#' @param extract A list of functions to be applied to each of the columns
#' in \code{extract_on} from the output of \code{transform}.
#' @param extract_on A list of column names to calculate statistics from.
#' @param groups A list of column names to group upon when calculating statistics.
#' @return Accelerometer features.
accelerometer_features_ <- function(sensor_data, 
                                    transform = transform_accelerometer_data,
                                    extract = c(time_domain_summary, 
                                                frequency_domain_summary, 
                                                frequency_domain_energy),
                                    extract_on = c("acceleration", "jerk", 
                                                   "velocity", "displacement"),
                                    groups = c("axis", "Window")) {
  kinematic_sensor_features(sensor_data, transform = transform,
                            extract = extract, extract_on = extract_on,
                            groups = groups, acf_col = "acceleration")
}

#' Extract gyroscope features
#' 
#' Extract gyroscope features. This function is not normally called
#' directly. See \code{gyroscope_features}.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements.
#' @param transform A function which accepts \code{sensor_data} as input
#' and outputs a dataframe.
#' @param extract A list of functions to be applied to each of the columns
#' in \code{extract_on} from the output of \code{transform}.
#' @param extract_on A list of column names to calculate statistics from.
#' @param groups A list of column names to group upon when calculating statistics.
#' @return gyroscope features.
gyroscope_features_ <- function(sensor_data,
                                transform = transform_gyroscope_data,
                                extract = c(time_domain_summary, 
                                            frequency_domain_summary, 
                                            frequency_domain_energy),
                                extract_on = c("acceleration", "velocity", "displacement"),
                                groups = c("axis", "Window")) {
  kinematic_sensor_features(
    sensor_data = sensor_data, transform = transform, extract = extract,
    extract_on = extract_on, groups = groups, acf_col = "velocity")
}

default_kinematic_features <- function(sampling_rate) {
  funs <- list(
    time_domain_summary = purrr::partial(time_domain_summary,
                                         sampling_rate = sampling_rate),
    frequency_domain_summary = purrr::partial(frequency_domain_summary,
                                              sampling_rate = sampling_rate),
    frequency_domain_energy = purrr::partial(frequency_domain_energy,
                                             sampling_rate = sampling_rate))
  return(funs)
}

#' Extract accelerometer features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' (by default) from accelerometer measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements.
#' @param funs A list of feature extraction functions that each accept
#' a single numeric vector as input.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return Accelerometer features.
accelerometer_features <- function(sensor_data, funs = NA, window_length = 256, overlap = 0.5,
                                   time_range = c(1,9), frequency_range=c(1, 25)) {
  sampling_rate <- get_sampling_rate(sensor_data)
  if(suppressWarnings(is.na(funs))) {
    funs <- default_kinematic_features(sampling_rate)
  }
  all_features <- accelerometer_features_(
    sensor_data = sensor_data,
    transform = purrr::partial(
      transform_accelerometer_data,
      transform = purrr::partial(window, window_length = window_length,
                                 overlap = overlap),
      window_length = window_length,
      overlap = overlap,
      time_range = time_range,
      frequency_range = frequency_range,
      sampling_rate = sampling_rate),
    extract = funs)
  return(all_features)
}

#' Extract gyroscope features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' (by default) from gyroscope measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements.
#' @param funs A list of feature extraction functions that each accept
#' a single numeric vector as input.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return Gyroscope features.
gyroscope_features <- function(sensor_data, funs = NA, window_length = 256, overlap = 0.5,
                               time_range = c(1,9), frequency_range=c(1, 25)) {
  sampling_rate <- get_sampling_rate(sensor_data)
  if(suppressWarnings(is.na(funs))) {
    funs <- default_kinematic_features(sampling_rate)
  }
  all_features <- gyroscope_features_(
    sensor_data = sensor_data,
    transform = purrr::partial(
      transform_gyroscope_data,
      transform = purrr::partial(window, window_length = window_length,
                                 overlap = overlap),
      window_length = window_length,
      overlap = overlap,
      time_range = time_range,
      frequency_range = frequency_range,
      sampling_rate = sampling_rate),
    extract = funs)
  return(all_features)
}

#' Apply standard transformations to kinematic sensor data
#' 
#' Apply standard transformations to kinematic (accelerometer/gyroscope)
#' sensor data. This function is not normally called directly. See
#' \code{transform_accelerometer_data} and \code{transform_gyroscope_data}.
#' 
#' @param sensor_data A dataframe with columns t, x, y, z.
#' @param transform A function which accepts a dataframe with columns t, axis, value
#' as input and outputs a dataframe.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @return A dataframe.
transform_kinematic_sensor_data <- function(sensor_data, transform, window_length, overlap,
                                            time_range, frequency_range, sampling_rate) {
  if (suppressWarnings(is.na(transform))) transform <- function(x) x
  preprocessed_sensor_data <- preprocess_sensor_data(sensor_data = sensor_data,
                                                    window_length = window_length,
                                                    sampling_rate = sampling_rate,
                                                    frequency_range = frequency_range, 
                                                    time_range = time_range)
  transformed_sensor_data <- transform(preprocessed_sensor_data)
  return(transformed_sensor_data)
}

#' Clean sensor data metrics
#' 
#' Puts sensor data in tidy format, detrends, applies bandpass filter,
#' and filters time.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' sensor measurements.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return A data frame in tidy format
preprocess_sensor_data <- function(sensor_data, window_length,
                                   sampling_rate, frequency_range, time_range) {
  preprocessed_sensor_data <- sensor_data %>%
    tidy_sensor_data() %>% 
    mutate_detrend() %>%
    mutate_bandpass(window_length, sampling_rate, frequency_range) %>%
    filter_time(time_range[1], time_range[2])
  return(preprocessed_sensor_data)
}

#' Prepare accelerometer sensor data for feature extraction
#' 
#' @param sensor_data A dataframe with columns t, x, y, z.
#' @param transform A function which accepts a dataframe with columns t, axis, value
#' as input and outputs a dataframe.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @param groups A list of column names to group upon when calculating statistics
#' @return A dataframe
transform_accelerometer_data <- function(sensor_data, transform = NA, window_length = 256, 
                                         overlap = 0.5, time_range = c(1,9), 
                                         frequency_range=c(1, 25), sampling_rate = 100,
                                         groups = c("axis", "Window")) {
  transform_kinematic_sensor_data(sensor_data, transform = transform, 
                                  window_length = window_length,
                                  overlap = overlap, time_range = time_range, 
                                  frequency_range = frequency_range,
                                  sampling_rate = sampling_rate) %>% 
    dplyr::rename(acceleration = value) %>% 
    mutate_derivative(sampling_rate = sampling_rate, groups = groups,
                      col = "acceleration", derived_col = "jerk") %>% 
    mutate_integral(sampling_rate = sampling_rate, groups = groups,
                    col = "acceleration", derived_col = "velocity") %>% 
    mutate_integral(sampling_rate = sampling_rate, groups = groups,
                    col = "velocity", derived_col = "displacement")
}

#' Prepare gyroscope sensor data for feature extraction
#' 
#' @param sensor_data A dataframe with columns t, x, y, z.
#' @param transform A function which accepts a dataframe with columns t, axis, value
#' as input and outputs a dataframe.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @param groups A list of column names to group upon when calculating statistics
#' @return A dataframe
transform_gyroscope_data <- function(sensor_data, transform = NA, window_length = 256,
                                     overlap = 0.5, time_range = c(1,9),
                                     frequency_range=c(1, 25), sampling_rate = 100,
                                     groups = c("axis", "Window")) {
  transform_kinematic_sensor_data(sensor_data, transform = transform, 
                                  window_length = window_length,
                                  overlap = overlap, time_range = time_range, 
                                  frequency_range = frequency_range,
                                  sampling_rate = sampling_rate) %>% 
    dplyr::rename(velocity = value) %>% 
    mutate_derivative(sampling_rate = sampling_rate, groups = groups,
                      col = "velocity", derived_col = "acceleration") %>% 
    mutate_integral(sampling_rate = sampling_rate, groups = groups,
                    col = "velocity", derived_col = "displacement")
}