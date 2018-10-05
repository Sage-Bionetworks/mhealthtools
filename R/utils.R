#' Calculate the fatigue given a vector x
#' 
#' @param x A numeric vector containing inter tap intevals
#' @return A list containing fatigue10, fatigue25, fatigue50 where 
#' fatigueX is the difference in the mean values of the
#' first X percent of input x and last X percent of input x
fatigue <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  top10 <- round(0.1 * n)
  top25 <- round(0.25 * n)
  top50 <- floor(0.5 * n)
  return(list(fatigue10 = mean(x[1:top10]) - mean(x[(n -top10):n]),
              fatigue25 = mean(x[1:top25]) - mean(x[(n - top25):n]),
              fatigue50 = mean(x[1:top50]) - mean(x[(n - top50):n])))
}


#' Calculate the drift given x and y
#' 
#' @param x A vecor containing x co-ordinates (same length as that of y)
#' @param y A vecor containing y co-ordinates (same length as that of x)
#' @return Drift vector which is sqrt(dx^2 + dy^2)
calculate_drift <- function(x, y) {
  dx <- diff(x, lag = 1)
  dy <- diff(y, lag = 1)
  return(sqrt(dx^2 + dy^2))
}


#' Calculate the Mean Teager-Kaiser energy, adapted from TKEO function in library(seewave) using f = 1, m = 1, M = 1
#' 
#' @param x A vector x whose Mean Taiger-Kaiser Energy Operator value needs to be calculated
#' @return A numeric value that is representative of the MeanTKEO
mean_tkeo <- function(x) {
  x <- x[!is.na(x)] # Remove NAs
  y <- x^2 - c(x[-1], NA) * c(NA, x[1:(length(x) -
                                         1)])
  return(mean(y, na.rm = TRUE))
}


#' Calculate the Coefficient of Variation (coef_var) for a given sequence
#' 
#' @param x A numeric vector x whose Coefficient of Variation needs to be calculated
#' @return A numeric value that is representative of the Coefficient of Variation
coef_var <- function(x) {
  x <- x[!is.na(x)] # Remove NAs
  return((sd(x)/mean(x)) * 100)
}

#' Curate the raw tapping data to get Left and Right events, after applying the threshold
#' 
#' @param tapData A dataframe with t,x,y and buttonid columns
#' @param depressThr The threshold for intertap distance
#' @return A dataframe with feature values and the appropriate error message
get_left_right_events_and_tap_intervals <- function(tapData, depressThr = 20) {
  tapTime <- tapData$t - tapData$t[1]
  ## calculate X offset
  tapX <- tapData$x - mean(tapData$x)
  ## find left/right finger 'depress' event
  dX <- diff(tapX)
  i <- c(1, which(abs(dX) > depressThr) + 1)
  ## filter data
  tapData <- tapData[i, ]
  tapTime <- tapTime[i]
  ## find depress event intervals
  tapInter <- diff(tapTime)
  
  ### ERROR CHECK -
  if (nrow(tapData) >= 5) {
    return(list(tapData = tapData, tapInter = tapInter,
                error = FALSE))
  } else {
    return(list(tapData = NA, tapInter = NA, error = TRUE))
  }
}

#' Calculate the sampling rate.
#' 
#' @param sensor_data A data frame with columns t, x, y, z.
#' @return The sampling rate (number of samples taken per second on average).
get_sampling_rate <- function(sensor_data) {
  tryCatch({
    t_length = length(sensor_data$t)
    sampling_rate = t_length / (sensor_data$t[t_length] - sensor_data$t[1])
    return(sampling_rate)
  }, error = function(e) { NA })
}

has_error <- function(sensor_data) {
  tibble::has_name(sensor_data, "error") && any(!is.na(sensor_data$error))
}

#' Gather the axial columns
#' 
#' Gathers x, y, and z columns into a single \code{axis} column and
#' normalize the \code{t} column to begin at \code{t} = 0.
#' 
#' @param sensor_data A data frame with a time column, \code{t}.
#' @return Sensor data in tidy format.
tidy_sensor_data <- function(sensor_data) {
  if (has_error(sensor_data)) return(sensor_data)
  if (any(is.na(sensor_data$t))) stop("NA values present in column t.")
  tidy_sensor_data <- tryCatch({
    t0 <- sensor_data$t[1]
    normalized_sensor_data <-  sensor_data %>% dplyr::mutate(t = t - t0)
    index = order(sensor_data$t)
    tidy_sensor_data = normalized_sensor_data[index,] %>%
      tidyr::gather(axis, value, -t)
  }, error = function(e) {
    dplyr::tibble(
      Window = NA,
      error = "Could not put sensor data in tidy format by gathering the axes.")
  })
  return(tidy_sensor_data)
}

#' Detrend time series data
#' 
#' @param time Numeric vector containing the timestamp values.
#' @param values Numeric vector the same length as \code{time}.
#' @return Numeric vector with detrended values.
detrend <- function(time, values) {
  detrended_values <- loess(values ~ time)$residual
  return(detrended_values)
}

#' Detrend sensor data
#' 
#' @param sensor_data A data frame with columns t, axis, value.
#' @return Sensor data with detrended values.
mutate_detrend <- function(sensor_data) {
  if (has_error(sensor_data)) return(sensor_data)
  detrended_sensor_data <- tryCatch({
    detrended_sensor_data <- sensor_data %>%
      dplyr::group_by(axis) %>%
      dplyr::mutate(
        value = detrend(t, value)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Detrend error")
  })
  return(detrended_sensor_data)
}

#' Apply a pass-band filter to time series data
#' 
#' @param values Numeric vector.
#' @param window_length Length of the filter.
#' @param sampling_rate Sampling rate of the values.
#' @param frequency_low Lower bound on frequency in Hz
#' @param frequency_high Upper bound on frequency in Hz
#' @return Filtered time series data
bandpass <- function(values, window_length, sampling_rate,
                     frequency_range) {
  frequency_low <- frequency_range[1]
  frequency_high <- frequency_range[2]
  if(frequency_low*2/sampling_rate > 1 || frequency_high*2/sampling_rate > 1) {
    stop("Frequency parameters can be at most half the sampling rate.")
  } else if (any(is.na(values))) {
    stop("NA values present in input.")
  }
  bandpass_filter <- signal::fir1(
    window_length-1,
    c(frequency_low*2/sampling_rate,
      frequency_high*2/sampling_rate),
    type="pass",
    window=seewave::hamming.w(window_length))
  filtered_values <- signal::filtfilt(bandpass_filter, values)
  return(filtered_values)
}

#' Apply a pass-band filter to sensor data
#' 
#' @param sensor_date A data frame with columns t, axis, value.
#' @param window_length Length of the filter.
#' @param sampling_rate Sampling rate of the value column.
#' @param frequency_range Bounds on frequency in Hz
#' @return Filtered time series data
mutate_bandpass <- function(sensor_data, window_length, sampling_rate,
                            frequency_range) {
  if (has_error(sensor_data)) return(sensor_data)
  bandpass_filtered_sensor_data <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis) %>%
      dplyr::mutate(
        value = bandpass(value, window_length, sampling_rate,
                         frequency_range)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Bandpass filter error")
  })
  return(bandpass_filtered_sensor_data)
}

#' Select a specific time range from sensor data.
#' 
#' @param sensor_date A data frame with a \code{t} column.
#' @param t1 Start time.
#' @param t2 End time.
#' @return Sensor data between time t1 and t2 (inclusive)
filter_time <- function(sensor_data, t1, t2) {
  if (has_error(sensor_data)) return(sensor_data)
  if (!hasName(sensor_data, "t")) {
    stop("Input has no column t.")
  }
  filtered_time_sensor_data <- tryCatch({
    filtered_time_sensor_data <- sensor_data %>% dplyr::filter(t >= t1, t <= t2)
    return(filtered_time_sensor_data)
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "'Not enough time samples")
  })
}

#' Window the value vector of sensor data for each axis
#' 
#' @param sensor_data A data frame with columns t, axis, value.
#' @param window_length Length of the filter
#' @param overlap window overlap
#' @param include_timestamp Whether to include columns for starting and ending
#' timestamps for each row.
#' @return Windowed sensor data
window <- function(sensor_data, window_length, overlap) {
  if (has_error(sensor_data)) return(sensor_data)
  tryCatch({
    spread_sensor_data <- sensor_data %>%
      tidyr::spread(axis, value)
    windowed_sensor_data <- spread_sensor_data %>% 
      dplyr::select(x, y, z) %>%
      purrr::map(windowSignal, 
                 window_length = window_length, overlap = overlap)
    tidy_windowed_sensor_data <- lapply(
      windowed_sensor_data,
      function(windowed_matrix) {
        windowed_matrix <- cbind(window_index = 1:dim(windowed_matrix)[1],
                                 windowed_matrix)
        tidy_tibble <- windowed_matrix %>% 
          dplyr::as_tibble() %>%
          tidyr::gather(Window, value, -window_index, convert=T)
        return(tidy_tibble)
      }) %>% 
      dplyr::bind_rows(.id = "axis")
    start_end_times <- window_start_end_times(spread_sensor_data$t,
                                              window_length = window_length,
                                              overlap = overlap)
    tidy_windowed_sensor_data <- tidy_windowed_sensor_data %>% 
      dplyr::left_join(start_end_times, by="Window") %>%
      dplyr::select(axis, Window, window_index, window_start_time,
                    window_end_time, value)
    return(tidy_windowed_sensor_data)
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Windowing error")
  })
}

#' Compute start/end timestamps for each window
#' 
#' @param t A numeric time vector
#' @param window_length Length of the filter
#' @param overlap Window overlap
#' @return A dataframe with columns Window, window_start_time,
#' window_end_time, window_start_index, window_end_index
window_start_end_times <- function(t, window_length, overlap) {
  seq_length <- length(t)
  if (seq_length < window_length) {
    window_length <- seq_length
    overlap <- 1
  }  
  start_indices <- seq(1, seq_length, window_length*overlap)
  end_indices <- seq(window_length, seq_length, window_length*overlap)
  start_indices <- start_indices[1:length(end_indices)]
  start_times <- t[start_indices]
  end_times <- t[end_indices]
  window_start_end_times <- dplyr::tibble(
    Window = seq(1, length(start_indices)),
    window_start_time = start_times,
    window_end_time = end_times,
    window_start_index = start_indices,
    window_end_index = end_indices)
  return(window_start_end_times)
}

#' Window a signal
#'  
#' Given a numeric vector, this function will return a windowed 
#' signal with hamming window.
#'  
#' @param values Timeseries vector of length n.
#' @param window_length Length of the filter.
#' @param overlap Window overlap.
#' @return A matrix of window_length x nwindows
windowSignal <- function(values, window_length = 256, overlap = 0.5){
  start_end_times <- window_start_end_times(values, window_length = window_length,
                                            overlap = overlap)
  nstart = start_end_times$window_start_index
  nend = start_end_times$window_end_index
  wn = seewave::hamming.w(window_length)
  
  a = apply(cbind(nstart,nend), 1, function(x, a, wn){
    a[seq(x[1],x[2],1)]*wn
  }, values, wn)
  colnames(a) = 1:dim(a)[2]
  return(a)
}

#' Calculate jerk
#' 
#' @param acceleration An acceleration vector.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @return Jerk vector.
jerk <- function(acceleration, sampling_rate) {
  jerk <- (acceleration - dplyr::lag(acceleration)) * sampling_rate
  jerk[1] <- 0
  return(jerk)
}

#' Take the derivative of a vector v
#' 
#' Take the derivative of a vector v by calculating the difference
#' between component x_i and x_(i-1).
#' 
#' @param v A numeric vector
#' @return A numeric vector
derivative <- function(v) {
  derivative <- (v - dplyr::lag(v))
  derivative[1] <- 0
  return(derivative)
}

#' Take the integral of a vector v
#' 
#' Take the integral of a vector v by computing the inverse of
#' the lagged differences (\code{diff} function).
integral <- function(v, sampling_rate) {
  integral <- diffinv(v)[-1]
  return(integral)
}

#' Add jerk column to sensor data
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param sampling_rate Sampling rate of the acceleration data.
#' @return Sensor data with jerk column.
mutate_jerk <- function(sensor_data, sampling_rate) {
  if (has_error(sensor_data)) return(sensor_data)
  sensor_data_with_jerk <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis, Window) %>% 
      dplyr::mutate(jerk = jerk(acceleration, sampling_rate)) %>%
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Error calculating jerk")
  })
  return(sensor_data_with_jerk)
}

#' Calculate velocity
#' 
#' @param acceleration An acceleration vector.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @return Velocity vector.
velocity <- function(acceleration, sampling_rate) {
  velocity <- stats::diffinv(acceleration)[-1] * sampling_rate
  return(velocity)
}

#' Add velocity column to sensor data
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param sampling_rate Sampling rate of the acceleration data.
#' @return Sensor data with velocity column.
mutate_velocity <- function(sensor_data, sampling_rate) {
  if (has_error(sensor_data)) return(sensor_data)
  sensor_data_with_velocity <- tryCatch({
    sensor_data %>% 
      dplyr::group_by(axis, Window) %>%
      dplyr::mutate(velocity = velocity(acceleration, sampling_rate)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Error calculating velocity")
  })
  return(sensor_data_with_velocity)
}

#' Calculate displacement
#' 
#' @param acceleration An acceleration vector.
#' @param sampling_rate Sampling rate of the acceleration vector.
#' @return Displacement vector.
displacement <- function(acceleration, sampling_rate) {
  velocity <- velocity(acceleration, sampling_rate)
  displacement <- stats::diffinv(velocity)[-1] * sampling_rate
  return(displacement)
}

#' Add displacement column to sensor data
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param sampling_rate Sampling rate of the acceleration data.
#' @return Sensor data with displacement column.
mutate_displacement <- function(sensor_data, sampling_rate) {
  if (has_error(sensor_data)) return(sensor_data)
  sensor_data_with_displacement <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis, Window) %>%
      dplyr::mutate(displacement = displacement(acceleration, sampling_rate)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Error calculating displacement")
  })
  return(sensor_data_with_displacement)
}

#' Add a column which is the "derivative" of an existing column to a time-series dataframe.
#' 
#' See function \code{derivative}.
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param sampling_rate Sampling rate of \code{col}.
#' @param col Name of column to differentiate.
#' @param derived_col Name of new column which is the derivative of \code{col}.
#' @return A dataframe
mutate_derivative <- function(sensor_data, sampling_rate, col, derived_col) {
  if (has_error(sensor_data)) return(sensor_data)
  sensor_data_with_derivative <- tryCatch({
    sensor_data %>%
      dplyr::mutate(!!derived_col := derivative(!!dplyr::sym(col)) * sampling_rate)
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = paste("Error calculating", derived_col))
  })
  return(sensor_data_with_derivative)
}

#' Add a column which is the "integral" of an existing column to a time-series dataframe.
#' 
#' See function \code{integral}.
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param sampling_rate Sampling rate of \code{col}.
#' @param col Name of column to integrate.
#' @param derived_col Name of new column which is the integral of \code{col}.
#' @return A dataframe
mutate_integral <- function(sensor_data, sampling_rate, col, derived_col) {
  if (has_error(sensor_data)) return(sensor_data)
  sensor_data_with_integral <- tryCatch({
    sensor_data %>% 
      dplyr::mutate(!!derived_col := integral(!!dplyr::sym(col)) * sampling_rate)
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = paste("Error calculating", derived_col))
  })
  return(sensor_data_with_integral)
}

#' Construct a dataframe with ACF values
#' 
#' Estimate the ACF for windowed sensor data.
#' 
#' @param sensor_data A data frame with columns \code{axis}, \code{Window}, and \code{col}.
#' @param col Name of column to calculate acf of.
#' @return A tibble with columns axis, Window, window_index, acf
calculate_acf <- function(sensor_data, col) {
  if (has_error(sensor_data)) return(sensor_data)
  acf_data <- tryCatch({
    groups <- dplyr::group_vars(sensor_data)
    acf_data <- sensor_data %>%
      tidyr::nest(col) %>%
      dplyr::mutate(data = purrr::map(data, function(d) {
        acf_col <- acf(d[,col], plot=F)$acf
        index_col <- 1:length(acf_col)
        dplyr::bind_cols(acf = acf_col, window_index = index_col)
      })) %>%
      tidyr::unnest(data) %>% 
      dplyr::select(-acf, acf) # arrange other columns before acf col
    if (length(groups)) { # restore groups if originally grouped
      acf_data <- acf_data %>% dplyr::group_by_at(.vars = groups)
    }
    return(acf_data)
  }, error = function(e) {
    dplyr::tibble(Window = NA, error = "Error calculating ACF")
  })
  return(acf_data)
}

#' Get min and max gravity values for each window
#' 
#' @param gravity_vector A gravity vector
#' @param window_length Length of the filter.
#' @param overlap Window overlap.
#' @return Min and max values for each window.
tag_outlier_windows_ <- function(gravity_vector, window_length, overlap) {
  if (!is.vector(gravity_vector)) stop("Input must be a numeric vector")
  gravity_summary <- gravity_vector %>% 
    windowSignal(window_length = window_length, overlap = overlap) %>%
    dplyr::as_tibble() %>%
    tidyr::gather(Window, value) %>%
    dplyr::group_by(Window) %>%
    dplyr::summarise(max = max(value, na.rm = T),
                     min = min(value, na.rm = T))
  return(gravity_summary)
}

#' Identify abnormal device rotations
#' 
#' Identify windows in which the phone may have been rotated or flipped,
#' as indicated by a gravity vector
#' 
#' @param gravity A dataframe with gravity vectors for columns
#' @param window_length Length of the filter.
#' @param overlap Window overlap.
#' @return Rotations errors for each window.
tag_outlier_windows <- function(gravity, window_length, overlap) {
  gr_error <- tryCatch({
    gr_error <- gravity %>% 
      purrr::map(tag_outlier_windows_, window_length, overlap) %>%
      dplyr::bind_rows(.id = 'axis') %>%
      dplyr::mutate(error = sign(max) != sign(min)) %>% 
      dplyr::group_by(Window) %>% 
      dplyr::summarise(error = any(error, na.rm = T)) %>% 
      dplyr::mutate(Window = as.integer(Window))
    gr_error$error[gr_error$error == TRUE] = 'Phone rotated within window'
    gr_error$error[gr_error$error == FALSE] = 'None'
    return(gr_error)
  }, error = function(e) {
    dplyr::tibble(Window = "NA", error = "Error tagging outlier windows")
  })
  return(gr_error)
}

#' Get default tapping features for metrics that use the tapping dataframe as a whole
#' 
#' Calculates features characterising tapping data (interaction terms etc., from the tap data frame)
#' 
#' @param tap_data A data frame with columns t, x, y, buttonid containing 
#' tapping measurements. buttonid can be from c('TappedButtonLeft','TappedButtonRight','TappedButtonNone') 
#' indicating a tap that has been classified as to the left, right or neither of those places on the screen
#' @return A features data frame of dimension 1 x n_features
tap_data_summary_features <- function(tapData){
  ftrs <- tryCatch({
    dplyr::tibble(numberTaps = nrow(tapData),
                  buttonNoneFreq = sum(tapData$buttonid == "TappedButtonNone")/nrow(tapData),
                  corXY = cor(tapData$x, tapData$y, use = "p"),
                  error = 'None')
  },
  error = function(x){
    return(dplyr::tibble(error = 'Error calculating tap data(frame) summary features'))
  })
}

#' Get default tapping features for intertap distance
#' 
#' Calculates features characterising a timeseries data 
#' 
#' @param tapInter A numeric vector containing intertap intervals
#' @return A features data frame of dimension 1 x n_features
intertap_summary_features <- function(tapInter){
  
  # Remove NAs
  tapInter <- tapInter %>% na.omit()
  
  # determine Autocorrelation
  auxAcf <- tryCatch({acf(tapInter, lag.max = 2,
                          plot = FALSE)$acf},
                     error = function(x){
                       return(list(NA,NA,NA))
                     })
  
  # calculate fatigue
  auxFatigue <- fatigue(tapInter)
  
  ftrs <- tryCatch({
    dplyr::tibble(mean = mean(tapInter,na.rm = TRUE),
                  median = median(tapInter, na.rm = TRUE),
                  iqr = IQR(tapInter, type = 7, na.rm = TRUE),
                  min = min(tapInter,na.rm = TRUE),
                  max = max(tapInter, na.rm = TRUE),
                  skew = e1071::skewness(tapInter),
                  kur = e1071::kurtosis(tapInter),
                  sd = sd(tapInter,na.rm = TRUE),
                  mad = mad(tapInter, na.rm = TRUE),
                  cv = coef_var(tapInter),
                  range = diff(range(tapInter,na.rm = TRUE)),
                  tkeo = mean_tkeo(tapInter),
                  ar1 = auxAcf[[2]],
                  ar2 = auxAcf[[3]],
                  fatigue10 = auxFatigue[[1]],
                  fatigue25 = auxFatigue[[2]],
                  fatigue50 = auxFatigue[[3]],
                  error = 'None')
  },
  error = function(x){
    return(dplyr::tibble(error = 'Error Calculating intertap summary features'))
  })
  return(ftrs)
}

#' Get default tapping features for tap drift
#' 
#' Calculates features characterising a timeseries data 
#' 
#' @param tapDrift A numeric vector 
#' @return A features data frame of dimension 1 x n_features
tapdrift_summary_features <- function(tapDrift){
  
  # Remove NAs
  tapDrift <- tapDrift %>% na.omit()
  
  ftrs <- tryCatch({
    dplyr::tibble(mean = mean(tapDrift, na.rm = TRUE),
                  median = median(tapDrift, na.rm = TRUE),
                  iqr = IQR(tapDrift, type = 7, na.rm = TRUE),
                  min = min(tapDrift, na.rm = TRUE),
                  max = max(tapDrift, na.rm = TRUE),
                  skew = e1071::skewness(tapDrift),
                  kur = e1071::kurtosis(tapDrift),
                  sd = sd(tapDrift, na.rm = TRUE),
                  mad = mad(tapDrift, na.rm = TRUE),
                  cv = coef_var(tapDrift), 
                  range = diff(range(tapDrift, na.rm = TRUE)),
                  error = 'None')
  },
  error = function(x){
    return(dplyr::tibble(error = 'Error Calculating tapdrift summary features'))
  })
  return(ftrs)
}



#' Get time domain features
#' 
#' Calculates features characterising a time series in the time domain.
#' 
#' @param values A numeric vector.
#' @param sampling_rate Sampling_rate of \code{values}.
#' @return A features data frame of dimension 1 x n_features
time_domain_summary <- function(values, sampling_rate=NA) {
  if(is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for time_domain_summary")
    sampling_rate <- 100
  }
  ftrs <- dplyr::tibble(
    mean = mean(values, na.rm = TRUE),
    median = quantile(values, probs = c(0.5), na.rm = TRUE),
    mode = pracma::Mode(values),
    mx =  max(values, na.rm = T),
    mn = min(values, na.rm = T),
    sd = sd(values, na.rm = TRUE),
    skewness = e1071::skewness(values),
    kurtosis = e1071::kurtosis(values),
    Q25 = quantile(values, probs = c(0.25), na.rm = TRUE),
    Q75 = quantile(values, probs = c(0.75), na.rm = TRUE),
    range = max(values, na.rm = T) - min(values, na.rm = T),
    rough = seewave::roughness(values),
    rugo = seewave::rugo(values),
    energy = sum(values^2),
    mobility = sqrt(var(diff(values)*sampling_rate)/var(values)),
    mtkeo = mean(seewave::TKEO(
      values, f = sampling_rate, plot = F)[,2], na.rm = T),
    dfa = fractal::DFA(values, sum.order = 1)[[1]],
    rmsmag = sqrt(sum(values^2)/length(values))) %>%  # Root Mean Square magnitude
    dplyr::mutate(IQR = Q25 - Q75,
                  complexity = sqrt(
                    var(diff(diff(values)*sampling_rate)*sampling_rate) /
                      var(diff(values)*sampling_rate)))
  names(ftrs) = paste0(names(ftrs), '.tm')
  return(ftrs)
}

#' Get frequency domain features
#' 
#' Calculates features characterising a time series in the frequency domain.
#' 
#' @param values A numeric vector.
#' @param sampling_rate Sampling_rate of \code{values}.
#' @param npeaks Number of peaks to be computed in EWT
#' @return A features data frame of dimension 1 x num_features
frequency_domain_summary <- function(values, sampling_rate=NA, npeaks = NA) {
  if(is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for time_domain_summary")
    sampling_rate = 100
  }
  
  if(is.na(npeaks)) {
    warning("Using default npeaks of 3 for frequency_domain_summary")
    npeaks = 3
  }
  spect <- getSpectrum(values, sampling_rate)
  freq <- spect$freq
  pdf <- spect$pdf/sum(spect$pdf, na.rm = T)
  pdf_adjusted <- pdf - mean(pdf)
  cdf <- cumsum(pdf)
  w = sd(pdf)
  
  # Get STFT spectrum based features
  ftrs <- dplyr::tibble(
    mn = sum(freq * pdf),
    mx = max(pdf),
    sd = sqrt(sum(pdf * ((freq - mn)^2))),
    sem = sd/sqrt(dim(spect)[1]),
    md = freq[length(cdf[cdf <= 0.5]) + 1],
    mod = freq[which.max(pdf)],
    Q25 = freq[length(cdf[cdf <= 0.25]) + 1],
    Q75 = freq[length(cdf[cdf <= 0.75]) + 1],
    IQR = Q75 - Q25,
    cent = sum(freq * pdf),
    skew = (sum((pdf_adjusted)^3)/(dim(spect)[1] - 1))/w^3,
    kurt = (sum((pdf_adjusted)^4)/(dim(spect)[1] - 1))/w^4,
    sfm = seewave::sfm(pdf),
    sh = seewave::sh(pdf))
  
  # Get EWT spectrum
  ewSpect <- data.frame(freq = freq, pdf = pdf) %>%
    getEWTspectrum(sampling_rate = sampling_rate, npeaks = npeaks)
  
  # Compute normalised point energies of each EW spctrum
  ewEnergy <- colSums(ewSpect^2, na.rm = T)
  ewEnergy <- ewEnergy/sum(ewEnergy, na.rm = T)
  
  # Compute entropy with EWT approximated energies
  ftrs <- ftrs %>% 
    dplyr::mutate(
      ewt.permEnt = statcomp::permutation_entropy(ewEnergy),
      ewt.shannonEnt = seewave::sh(ewEnergy, alpha = 'shannon'),
      ewt.simpsonEnt = seewave::sh(ewEnergy, alpha = 'simpson'),
      ewt.renyiEnt = seewave::sh(ewEnergy, alpha = 2), # alpha is hardcoded to be 2
      ewt.tsallisEnt = (1-sum(ewEnergy^0.1))/(0.1-1)) # q is hardcoded to be 0.1
  
  names(ftrs) <- paste0(names(ftrs), '.fr')
  
  return(data.frame(ftrs))
}


#' Get AR spectrum
#' 
#' Given a numeric vector, this function will return a spectrum
#' with all pole AR model.
#' 
#' @param values A timeseries vector.
#' @param sampling_rate Sampling rate of the signal (by default it is 100 Hz).
#' @param nfreq Number of frequecy points to be interpolated.
#' @return An AR spectrum.
getSpectrum <- function(values, sampling_rate = 100, nfreq = 500){
  tmp = stats::spec.ar(values, n.freq = nfreq, plot = F)
  spect = data.frame(freq = tmp$freq * sampling_rate, pdf = tmp$spec)
  return(spect)
}


#' Get EWT spectrum 
#' 
#' Given the spectrum of a time series vector this function will return its 
#' Empirical Wavelet Transformed spectrum.
#' 
#' @param spect FFT spectrum as a two dimensional data frame with columns
#' names as freq and pdf respectively n.freq x 2.
#' @param npeaks Number of peaks to be captured.
#' @param fractionMinPeakHeight Minimum height (relative to maximum peak height)
#' a peak has to have. Specified as fraction between 0 and 1.
#' @param minPeakDistance The minimum distance (in indices) peaks.
#' have to have to be counted. 
#' @param sampling_rate Sampling rate of the signal (by default it is 100 Hz).
#' @return Emprical wavelet transformed spectrum of dimension n.freq x (npeaks + 1).
getEWTspectrum <- function(spect, npeaks = 3, fractionMinPeakHeight = 0.1,
                           minPeakDistance = 1, sampling_rate = 100) {
  
  # Find top peaks for EWT calculation
  peakFreqs = pracma::findpeaks(spect$pdf, 
                                minpeakheight = fractionMinPeakHeight *
                                  max(spect$pdf, na.rm = T),
                                minpeakdistance = minPeakDistance, 
                                npeaks = npeaks,
                                sortstr = TRUE)
  
  # Convert peak frequency to radians and find mid points
  peakFreqs = spect$freq[sort(peakFreqs[,2])] * pi * 2 / sampling_rate
  peakFreqs = unique(c(0,peakFreqs,pi))
  midPeakFreqs = c(0, peakFreqs[-length(peakFreqs)] + diff(peakFreqs)/2, pi)
  
  # Choose optimal scaling operator for the transition widths
  numeratorvec = midPeakFreqs[2:(length(midPeakFreqs)+2)] - 
    midPeakFreqs[1:(length(midPeakFreqs)+1)]
  denominatorvec = midPeakFreqs[2:(length(midPeakFreqs)+2)] + 
    midPeakFreqs[1:(length(midPeakFreqs)+1)]
  optimalGamma = min(numeratorvec/denominatorvec, na.rm = TRUE)
  
  # Compute emprical scaling and wavelets
  empricalWavelets = purrr::map2(
    midPeakFreqs[1:(length(midPeakFreqs)-1)], 
    midPeakFreqs[2:length(midPeakFreqs)],
    .f = function(wn1, wn2, n.freq, optimalGamma) {
      # Compute emprical scaling function for the first peak
      phi.sy = rep(0, n.freq)
      w = seq(0, pi, len = n.freq)
      
      # Compute beta (an arbitary coefficient)
      x = (1/(2*optimalGamma*wn1)) * (abs(w) - (1-optimalGamma) * wn1)
      beta1 = x^4*(35-84*x+70*x^2-20*x^3)
      
      x = (1/(2*optimalGamma*wn2)) * (abs(w) - (1-optimalGamma) * wn2)
      beta2 = x^4*(35-84*x+70*x^2-20*x^3)
      
      if(wn2 != pi) {
        # Compute scaling/wavelets for different conditions
        ind = ((1+optimalGamma)*wn1 <= abs(w)) & (abs(w) <= (1-optimalGamma) * wn2)
        phi.sy[ind] = 1
        ind = ((1 - optimalGamma) * wn2 <= abs(w)) &
          (abs(w) <= (1 + optimalGamma) * wn2)
        phi.sy[ind] = cos(pi*beta2[ind]/2)
        ind = ((1 - optimalGamma) * wn1 <= abs(w)) &
          (abs(w) <= (1 + optimalGamma) * wn1)
        phi.sy[ind] = sin(pi*beta1[ind]/2)
      } else {
        # Compute scaling/wavelets for different conditions
        ind = abs(w) <= (1-optimalGamma) * wn1
        phi.sy[ind] = 1
        ind = ((1 - optimalGamma) * wn1 <= abs(w)) &
          (abs(w) <= (1 + optimalGamma) * wn1)
        phi.sy[ind] = cos(pi*beta1[ind]/2)
        phi.sy = 1 - phi.sy
      }
      
      return(phi.sy)
    }, 
    dim(spect)[1], optimalGamma)
  
  # Compute EW modified spectrum
  ewSpect = sapply(empricalWavelets, function(x, spect){spect$pdf*x}, spect)
  
  return(ewSpect)
}


#' Get frequency domain energy features
#' 
#' Given a numeric vector, this function will return features
#' characterising the time series in frequency domain.
#' 
#' @param values A timeseries vector.
#' @param sampling_rate Sampling rate of the signal (by default it is 100 Hz).
#' @return A features data frame of dimension 1 x 48.
frequency_domain_energy <- function(values, sampling_rate=NA) {
  if(is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for frequency_domain_energy")
    sampling_rate = 100
  }  
  spect = getSpectrum(values, sampling_rate)
  freq = spect$freq
  pdf = spect$pdf/sum(spect$pdf, na.rm = T)
  cdf = cumsum(pdf)
  
  st = seq(1,24.5,0.5)
  en = seq(1.5,25,0.5)
  
  ftrs = mapply(function(indStr, indEn){
    ind = which(freq >= indStr & freq <= indEn)
    pracma::trapz(freq[ind], pdf[ind])
  }, st, en) %>% t %>% data.frame()
  colnames(ftrs) = paste0('EnergyInBand',gsub('\\.','_',st))
  
  return(ftrs)
}

#' Map a function to a single column within tibble groups
#' 
#' A convenience function for mapping a function -- which accepts a vector as input
#' and outputs an atomic value -- to a single column of each group in a grouped tibble.
#' 
#' @param .x A tibble
#' @param col Column to pass as a vector to \code{.f}.
#' @param .f Function to be mapped to \code{col} for each group.
#' @param ... Additional arguments to \code{.f}.
#' @return A tibble indexed by groups with an additional column containing
#' the output of the mapped function.
map_groups <- function(x, col, f, ...) {
  dots <- rlang::enquos(...) # can also use enexprs()
  x %>%
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, ~ f(.[[col]], !!!dots))) %>%
    tidyr::unnest(data)
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
  groups <- dplyr::group_vars(x)
  funs_output <- purrr::map(
    funs, ~ map_groups(
      x = x,
      col = col,
      f = .))
  if (length(groups)) { # concatenate elements of funs_output
    funs_output <- funs_output %>% 
      purrr::reduce(dplyr::left_join, by = groups) %>%
      dplyr::mutate(measurementType = col) %>%
      dplyr::select(measurementType, dplyr::one_of(groups), dplyr::everything())
  }
  return(funs_output)
}