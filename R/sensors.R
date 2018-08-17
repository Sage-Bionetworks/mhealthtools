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
  incidental_cols_to_preserve <- transformed_sensor_data %>%
    select(-dplyr::one_of(extract_on)) %>%
    distinct() # distinct of group (table index) cols and incidental cols
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
  all_features <- dplyr::bind_rows(movement_features, acf_features) %>%
    dplyr::left_join(incidental_cols_to_preserve, by = groups) %>%
    select(measurementType, dplyr::one_of(names(incidental_cols_to_preserve)),
           dplyr::everything())
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
accelerometer_features_ <- function(sensor_data, transform, extract, groups,
                                    extract_on = c("acceleration", "jerk", 
                                                   "velocity", "displacement")) {
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
                                              sampling_rate = sampling_rate,
                                              npeaks = 3),
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
#' @param transformation A function which accepts a tidy version of \code{sensor_data},
#' with columns t, axis, value, as input and outputs a dataframe suitable for 
#' feature extraction. By default, the tidy sensor data is windowed before feature
#' extraction. This is a more user-friendly version of the \code{transform} 
#' parameter required for more generic functions like \code{sensor_features}. Namely,
#' its input will already be "cleaned" by detrending, bandpassing, etc. See 
#' \code{preprocess_sensor_data} for all the cleaning steps performed.
#' @param funs A list of feature extraction functions that each accept
#' a single numeric vector as input.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return Accelerometer features.
#' @export
accelerometer_features <- function(sensor_data, transformation = NA, funs = NA, 
                                   groups = c("axis", "Window"), window_length = 256,
                                   overlap = 0.5, time_range = c(1,9),
                                   frequency_range=c(1, 25)) {
  sampling_rate <- get_sampling_rate(sensor_data)
  if(suppressWarnings(is.na(transformation))) {
    transformation <- transformation_window(window_length = window_length,
                                            overlap = overlap)
  }
  if(suppressWarnings(is.na(funs))) {
    funs <- default_kinematic_features(sampling_rate)
  }
  all_features <- accelerometer_features_(
    sensor_data = sensor_data,
    transform = purrr::partial(
      transform_accelerometer_data,
      transformation = transformation,
      window_length = window_length,
      overlap = overlap,
      time_range = time_range,
      frequency_range = frequency_range,
      sampling_rate = sampling_rate),
    extract = funs,
    groups = groups)
  return(all_features)
}

#' Extract gyroscope features
#' 
#' Extract features summarizing time domain, frequency domain, and frequency energy
#' (by default) from gyroscope measurements.
#' 
#' @param sensor_data A data frame with columns t, x, y, z containing 
#' gyroscope measurements.
#' @param transformation A function which accepts a tidy version of \code{sensor_data},
#' with columns t, axis, value, as input and outputs a dataframe suitable for 
#' feature extraction. By default, the tidy sensor data is windowed before feature
#' extraction. This is a more user-friendly version of the \code{transform} 
#' parameter required for more generic functions like \code{sensor_features}. Namely,
#' its input will already be "cleaned" by detrending, bandpassing, etc. See 
#' \code{preprocess_sensor_data} for all the cleaning steps performed.
#' @param funs A list of feature extraction functions that each accept
#' a single numeric vector as input.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @return Gyroscope features.
#' @export
gyroscope_features <- function(sensor_data, transformation = NA, funs = NA,
                               groups = c("axis", "Window"), window_length = 256,
                               overlap = 0.5, time_range = c(1,9),
                               frequency_range=c(1, 25)) {
  sampling_rate <- get_sampling_rate(sensor_data)
  if(suppressWarnings(is.na(transformation))) {
    transformation <- transformation_window(window_length = window_length,
                                            overlap = overlap)
  }
  if(suppressWarnings(is.na(funs))) {
    funs <- default_kinematic_features(sampling_rate)
  }
  all_features <- gyroscope_features_(
    sensor_data = sensor_data,
    transform = purrr::partial(
      transform_gyroscope_data,
      transformation = transformation,
      window_length = window_length,
      overlap = overlap,
      time_range = time_range,
      frequency_range = frequency_range,
      sampling_rate = sampling_rate),
    extract = funs,
    groups = groups)
  return(all_features)
}

#' Extract tapping (screen sensor) features
#' 
#' @param tap_data A data frame with columns t, x, y, buttonid containing 
#' tapping measurements. buttonid can be from c('TappedButtonLeft','TappedButtonRight','TappedButtonNone') 
#' indicating a tap that has been classified as to the left, right or neither of those places on the screen
#' @param depressThr A numerical threshold for intertap distance in x axis
#' @return A dataframe of features.
#' @export
tapping_features <- function(tap_data,
                             depressThr = 20) {
  
  results <- get_left_right_events_and_tap_intervals(tapData = tap_data,
                                                     depressThr = depressThr)
  tapInter <- results$tapInter
  tapData <- results$tapData
  error <- results$error
  
  # check error - if after cleaning tapping data less than 5 data points remain
  if (error) {
    tapFeatures <- dplyr::tibble(error = "post cleaning less than 5 tap points remain")
    return(tapFeatures)
  }
  
  meanX <- mean(tapData$x)
  iL <- tapData$x < meanX
  iR <- tapData$x >= meanX
  driftLeft <- calculate_drift(x = tapData[iL,"x"], y = tapData[iL, "y"])
  driftRight <- calculate_drift(x = tapData[iR,"x"], y = tapData[iR, "y"])
  
  intertap_features <- intertap_summary_features(tapInter = tapInter)
  if(intertap_features$error == 'None'){
    intertap_features <- intertap_features %>% dplyr::select(-error)
    colnames(intertap_features) <- paste0(colnames(intertap_features),'TapInter')
  }else{
    colnames(intertap_features) <- paste0(colnames(intertap_features),'TapInter')
  }
  
  tapdrift_left_features <- tapdrift_summary_features(tapDrift = driftLeft)
  if(tapdrift_left_features$error == 'None'){
    tapdrift_left_features <- tapdrift_left_features %>% dplyr::select(-error)
    colnames(tapdrift_left_features) <- paste0(colnames(tapdrift_left_features),'DriftLeft')
  }else{
    colnames(tapdrift_left_features) <- paste0(colnames(tapdrift_left_features),'DriftLeft')
  }
  
  tapdrift_right_features <- tapdrift_summary_features(tapDrift = driftRight)
  if(tapdrift_right_features$error == 'None'){
    tapdrift_right_features <- tapdrift_right_features %>% dplyr::select(-error)
    colnames(tapdrift_right_features) <- paste0(colnames(tapdrift_right_features),'DriftRight')
  }else{
    colnames(tapdrift_right_features) <- paste0(colnames(tapdrift_right_features),'DriftRight')
  }
  
  tapdata_features <- tap_data_summary_features(tapData = tap_data)
  if(tapdata_features$error == 'None'){
    tapdata_features <- tapdata_features %>% dplyr::select(-error)
  }
  
  tapFeatures <- dplyr::bind_cols(intertap_features,
                                  tapdrift_left_features,
                                  tapdrift_right_features,
                                  tapdata_features)
  
  ftrs_error <- grep('error', colnames(tapFeatures))
  ftrs_error <- paste(tapFeatures[ftrs_error], collapse = ' ; ')
  if(ftrs_error == ''){
    ftrs_error = 'None'
  }
  
  tapFeatures$error <- ftrs_error
  tapFeatures <- tapFeatures %>% 
    as.data.frame()
  return(tapFeatures)
}

#' Apply standard transformations to kinematic sensor data
#' 
#' Apply standard transformations to kinematic (accelerometer/gyroscope)
#' sensor data. This function is not normally called directly. See
#' \code{transform_accelerometer_data} and \code{transform_gyroscope_data}.
#' 
#' @param sensor_data A dataframe with columns t, x, y, z.
#' @param transformation A function which accepts a tidy version of \code{sensor_data},
#' with columns t, axis, value, as input and outputs a dataframe suitable for 
#' feature extraction. By default, the tidy sensor data is windowed before feature
#' extraction. This is a more user-friendly version of the \code{transform} 
#' parameter required for more generic functions like \code{sensor_features}. Namely,
#' its input will already be "cleaned" by detrending, bandpassing, etc. See 
#' \code{preprocess_sensor_data} for all the cleaning steps performed.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @return A dataframe.
transform_kinematic_sensor_data <- function(sensor_data, transformation, 
                                            window_length, overlap,
                                            time_range, frequency_range, sampling_rate) {
  if (suppressWarnings(is.na(transformation))) transformation <- function(x) x
  preprocessed_sensor_data <- preprocess_sensor_data(sensor_data = sensor_data,
                                                     window_length = window_length,
                                                     sampling_rate = sampling_rate,
                                                     frequency_range = frequency_range, 
                                                     time_range = time_range)
  transformed_sensor_data <- transformation(preprocessed_sensor_data)
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

transformation_window <- function(window_length, overlap) {
  purrr::partial(window, window_length = window_length, overlap = overlap)
}

transformation_imf_window <- function(window_length, overlap, max_imf) {
  purrr::partial(
    function(sensor_data, window_length, overlap, max_imf) {
      values <- sensor_data %>% 
        tidyr::spread(key = "axis", value = "value")
      imf <- purrr::map2(values %>% dplyr::select(t), values %>% dplyr::select(-t),
                         function(t, v) {
                           dplyr::as_tibble(
                             EMD::emd(v, t, max.imf = max_imf)$imf) %>% 
                             setNames(1:max_imf)
                         })
      names(imf) <- names(values %>% dplyr::select(-t))
      windowed_imf <- purrr::map_dfr(
        imf, function(df) {
          windowed_imf <- purrr::map_dfr(
            df, function(col) {
              windowSignal(col, window_length = window_length,
                           overlap = overlap) %>% 
                dplyr::as_tibble() %>% 
                tidyr::gather(key="Window", value="value") %>% 
                dplyr::mutate(Window = as.integer(Window))
            }, .id = "IMF") %>% dplyr::mutate(IMF = as.integer(IMF))
        }, .id = "axis")
      return(windowed_imf)
    },
    window_length = window_length,
    overlap = overlap,
    max_imf = max_imf)
}

#' Prepare accelerometer sensor data for feature extraction
#' 
#' @param sensor_data A dataframe with columns t, x, y, z.
#' @param transformation A function which accepts a tidy version of \code{sensor_data},
#' with columns t, axis, value, as input and outputs a dataframe suitable for 
#' feature extraction. By default, the tidy sensor data is windowed before feature
#' extraction. This is a more user-friendly version of the \code{transform} 
#' parameter required for more generic functions like \code{sensor_features}. Namely,
#' its input will already be "cleaned" by detrending, bandpassing, etc. See 
#' \code{preprocess_sensor_data} for all the cleaning steps performed.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @param groups A list of column names to group upon when calculating statistics
#' @return A dataframe
transform_accelerometer_data <- function(sensor_data, transformation = NA,
                                         window_length = 256, overlap = 0.5,
                                         time_range = c(1,9), frequency_range=c(1, 25),
                                         sampling_rate = 100, 
                                         groups = c("axis", "Window")) {
  transform_kinematic_sensor_data(sensor_data, transformation = transformation, 
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
#' @param transformation A function which accepts a tidy version of \code{sensor_data},
#' with columns t, axis, value, as input and outputs a dataframe suitable for 
#' feature extraction. By default, the tidy sensor data is windowed before feature
#' extraction. This is a more user-friendly version of the \code{transform} 
#' parameter required for more generic functions like \code{sensor_features}. Namely,
#' its input will already be "cleaned" by detrending, bandpassing, etc. See 
#' \code{preprocess_sensor_data} for all the cleaning steps performed.
#' @param window_length Length of sliding windows.
#' @param overlap Window overlap.
#' @param time_range Timestamp range to use.
#' @param frequency_range Frequency range for the bandpass filter.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @param groups A list of column names to group upon when calculating statistics
#' @return A dataframe
transform_gyroscope_data <- function(sensor_data, transformation = NA, window_length = 256,
                                     overlap = 0.5, time_range = c(1,9),
                                     frequency_range=c(1, 25), sampling_rate = 100,
                                     groups = c("axis", "Window")) {
  transform_kinematic_sensor_data(sensor_data, transformation = transformation, 
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