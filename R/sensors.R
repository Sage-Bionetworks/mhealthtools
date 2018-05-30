#' Extract accelerometer features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' from accelerometer measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' accelerometer measurements.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param bandpass_frequency_low Lower bound on frequency in Hz.
#' @param bandpass_frequency_high Upper bound on frequency in Hz.
#' @return Accelerometer features.
accelerometer_features <- function(
  sensor_data, window_length = 256, overlap = 0.5,
  bandpass_frequency_low = 1, bandpass_frequency_high = 25) {
  sampling_rate <- get_sampling_rate(sensor_data)
  sensor_data <- sensor_data %>%
    gather_axis() %>% 
    mutate_detrend() %>%
    mutate_bandpass(window_length, sampling_rate, bandpass_frequency_low,
                    bandpass_frequency_high) %>%
    filter_time(1, 9) %>%
    window(window_length, overlap) %>%
    mutate_jerk(sampling_rate) %>%
    mutate_velocity(sampling_rate) %>%
    mutate_displacement(sampling_rate)
  
  time_domain_features <- map_groups(
    sensor_data, c("axis", "window"), "acceleration",
    time_domain_summary, sampling_rate = sampling_rate)
  frequency_domain_features <- map_groups(
    sensor_data, c("axis", "window"), "acceleration",
    frequency_domain_summary, sampling_rate = sampling_rate)
  frequency_energy_features <- map_groups(
    sensor_data, c("axis", "window"), "acceleration",
    frequency_domain_energy, sampling_rate = sampling_rate)
  
  all_features <- plyr::join_all(
    list(time_domain_features, frequency_domain_features, frequency_energy_features),
    by = c("axis", "window")) %>% 
    dplyr::mutate(error = NA)
  return(all_features)
}

#' Extract gyrometer features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' from gyrometer measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' gyrometer measurements.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param bandpass_frequency_low Lower bound on frequency in Hz.
#' @param bandpass_frequency_high Upper bound on frequency in Hz.
#' @return Gyrometer features.
gyrometer_features <- function(
  sensor_data, window_length = 256, overlap = 0.5,
  bandpass_frequency_low = 1, bandpass_frequency_high = 25) {
  sampling_rate <- get_sampling_rate(sensor_data)
  sensor_data <- sensor_data %>%
    gather_axis() %>% 
    mutate_detrend() %>%
    mutate_bandpass(window_length, sampling_rate, bandpass_frequency_low,
                    bandpass_frequency_high) %>%
    filter_time(1, 9) %>%
    window(window_length, overlap) %>%
    mutate_jerk(sampling_rate) %>%
    mutate_velocity(sampling_rate) %>%
    mutate_displacement(sampling_rate)
  
  time_domain_features <- map_groups(
    sensor_data, c("axis", "window"), "acceleration",
    time_domain_summary, sampling_rate = sampling_rate)
  frequency_domain_features <- map_groups(
    sensor_data, c("axis", "window"), "acceleration",
    frequency_domain_summary, sampling_rate = sampling_rate)
  frequency_energy_features <- map_groups(
    sensor_data, c("axis", "window"), "acceleration",
    frequency_domain_energy, sampling_rate = sampling_rate)
  
  all_features <- plyr::join_all(
    list(time_domain_features, frequency_domain_features, frequency_energy_features),
    by = c("axis", "window")) %>%
    dplyr::mutate(error = NA)
  return(all_features)
}