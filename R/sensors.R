#' Extract accelerometer features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' from accelerometer measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements.
#' @param funs Feature extraction functions that accept a single
#' time-series vector as input.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return Accelerometer features.
accelerometer_features <- function(
  sensor_data,
  funs = c(time_domain_summary, frequency_domain_summary, frequency_domain_energy),
  window_length = 256, overlap = 0.5,
  time_range = c(1,9), frequency_range=c(1, 25)) {
  
  sampling_rate <- get_sampling_rate(sensor_data)
  # check input integrity
  if (is.na(sampling_rate)) {
    return(dplyr::tibble(window = "NA", error = "Could not calculate sampling rate."))
  }
  # preprocess and calculate jerk, velocity, displacement
  sensor_data <- sensor_data %>%
    tidy_sensor_data() %>% 
    mutate_detrend() %>%
    mutate_bandpass(window_length, sampling_rate, frequency_range) %>%
    filter_time(time_range[1], time_range[2]) %>%
    window(window_length, overlap) %>%
    mutate_jerk(sampling_rate) %>%
    mutate_velocity(sampling_rate) %>%
    mutate_displacement(sampling_rate)
  if (has_error(sensor_data)) return(sensor_data)
  # acf must be done seperately because it has different dimensions than sensor_data
  acf_data <- calculate_acf(sensor_data)
  
  # extract features
  movement_features <- map_dfr(
    list("acceleration", "jerk", "velocity", "displacement"),
    ~ extract_features(sensor_data, ., funs))
  
  if (has_error(acf_data)) {
    all_features <- acf_data %>% 
      dplyr::mutate(measurementType = "acf") %>% 
      dplyr::bind_rows(movement_features)
    return(all_features)
  }
  acf_features <- extract_features(acf_data, "acf", funs)
  all_features <- dplyr::bind_rows(movement_features, acf_features) %>%
    dplyr::mutate(error = NA)
  
  all_features <- all_features %>% 
    mutate(
      measurementType = ifelse(measurementType == "acceleration", "ua", measurementType),
      measurementType = ifelse(measurementType == "jerk", "uj", measurementType),
      measurementType = ifelse(measurementType == "velocity", "uv", measurementType),
      measurementType = ifelse(measurementType == "displacement", "ud", measurementType),
      measurementType = ifelse(measurementType == "acf", "uaacf", measurementType)
    )
  
  return(all_features)
}

#' Extract gyroscope features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' from gyroscope measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements.
#' @param funs Feature extraction functions that accept a single
#' time-series vector as input.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return Gyroscope features.
gyroscope_features <- function(
  sensor_data,
  funs = c(time_domain_summary, frequency_domain_summary, frequency_domain_energy),
  window_length = 256, overlap = 0.5,
  time_range = c(1,9), frequency_range=c(1, 25)) {
  sampling_rate <- get_sampling_rate(sensor_data)
  # check input integrity
  if (is.na(sampling_rate)) {
    return(dplyr::tibble(Window = "NA", error = "Could not calculate sampling rate."))
  }
  # preprocess and calculate jerk, velocity, displacement
  sensor_data <- sensor_data %>%
    tidy_sensor_data() %>% 
    mutate_detrend() %>%
    mutate_bandpass(window_length, sampling_rate, frequency_range) %>%
    filter_time(time_range[1], time_range[2]) %>%
    window(window_length, overlap) %>%
    mutate_jerk(sampling_rate) %>%
    mutate_velocity(sampling_rate) %>%
    mutate_displacement(sampling_rate)
  if (has_error(sensor_data)) return(sensor_data)
  # acf must be done seperately because it has different dimensions than sensor_data
  acf_data <- calculate_acf(sensor_data)
  
  # extract features
  movement_features <- map_dfr(
    list("acceleration", "jerk", "velocity"),
    ~ extract_features(sensor_data, ., funs))
  
  if (has_error(acf_data)) {
    all_features <- acf_data %>% 
      dplyr::mutate(measurementType = "acf") %>% 
      dplyr::bind_rows(movement_features)
    return(all_features)
  }
  acf_features <- extract_features(acf_data, "acf", funs)
  all_features <- dplyr::bind_rows(movement_features, acf_features) %>%
    dplyr::mutate(error = NA)
  
  all_features <- all_features %>% 
    mutate(
      measurementType = ifelse(measurementType == "acceleration", "uav", measurementType),
      measurementType = ifelse(measurementType == "jerk", "uaa", measurementType),
      measurementType = ifelse(measurementType == "velocity", "uad", measurementType),
      measurementType = ifelse(measurementType == "acf", "uavacf", measurementType)
    )
  
  return(all_features)
}

#' Extract features from a column
#' 
#' Apply each of the functions in \code{funs} to the column 
#' \code{col} in data frame \code{x}. Each of the functions in 
#' \code{funs} must accept a single vector as input and output 
#' a data frame with columns axis and window (and optionally others).
#' 
#' @param x A data frame with columns axis, window, and \code{col}.
#' @param col The name of the column in \code{x} to pass to each 
#' function in \code{funs}.
#' @param funs A list of functions that accept a single vector as input.
#' @return a data frame with columns axis, window, and other feature columns.
extract_features <- function(x, col, funs) {
  purrr::map(
    funs, ~ map_groups(
      x = x,
      groups = c("axis", "Window"),
      col = col,
      f = .)) %>%
    purrr::reduce(left_join, by=c("axis", "Window")) %>%
    dplyr::mutate(measurementType = col) %>%
    dplyr::select(measurementType, axis, Window, everything())
}