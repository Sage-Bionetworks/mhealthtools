#' Preprocess and extract heart rate from smartphone video recordings.
#' 
#' A convenience wrapper for extracting heart rate features for each color
#' band from average pixel value per frame of video (processed hr) captured
#' using smartphone cameras.
#' 
#' @param heartrate_data A data frame with columns t, red, green and blue
#' @param window_length Length of the time window in seconds, to be considered
#' while calculating the heart rate for each channel
#' @param frequency_range Frequency range in Hz for the bandpass filter parameters
#' @param bandpass_order Order (length) of the bandpass filter to be used for filtering
#' 
#' @return list containing heart rate and confidence of the estimate for
#' each color (red, green, blue)
#' @export
#' @author Meghasyam Tummalacherla, Phil Snyder 
#' @examples 
#' heartrate_data = heartrate_data[,c('timestamp', 'red', 'green', 'blue')]
#' heartrate_ftrs = get_heartrate(heartrate_data)
#'  
get_heartrate <- function(heartrate_data, window_length = 10, window_overlap = 0.5) {
  heartrate_error_frame <- data.frame(red = NA, green = NA, blue = NA,
                            error = NA, sampling_rate = NA)
  sampling_rate <- get_sampling_rate(heartrate_data)
  if (is.infinite(sampling_rate) || is.na(sampling_rate)) {
    heartrate_error_frame$error <- paste("Sampling Rate calculated from timestamp is Inf",
                               "or NaN / timestamp not found in json")
    return(heartrate_error_frame)
  }

  # Convert window length from seconds to samples
  window_length <- round(sampling_rate * window_length)
  
  # Apply pre processing filter signal between frequency_range
  # order for the running mean based filter
  mforder <- 65 # (Based on 60 and 30 fps)
  
  # Split each color into segments based on window_length
  heartrate_data <- tryCatch({
    heartrate_data %>%
      dplyr::select(red, green, blue) %>%
      na.omit() %>%
      lapply(window_signal, window_length, 0.5)
    }, error = function(e) { NA })
  if (all(is.na(heartrate_data))) {
    heartrate_data1$error <- "red, green, blue cannot be read from JSON"
    return(heartrate_data1)
  }
  
  # Apply filter to each segment of each color
  heartrate_data <- heartrate_data %>%
    lapply(function(dfl) {
      dfl[is.na(dfl)] <- 0
      dfl <- tryCatch({
        apply(dfl, 2, get_filtered_signal, mforder,
              bandpass_order, frequency_range, sampling_rate)
        }, error = function(e) { NA })
    })
  if (all(is.na(heartrate_data))) {
    heartrate_data1$error <- "filtering error"
    return(heartrate_data1)
  }
  
  # Get HR for each filtered segment of each color
  heartrate_data <- heartrate_data %>%
    lapply(function(dfl) {
      dfl <- tryCatch({
        apply(dfl, 2, get_hr_from_time_series, sampling_rate)
      }, error = function(e) { NA })
      dfl <- as.data.frame(t(dfl))
      colnames(dfl) <- c("hr", "confidence")
      return(dfl)
  })
  if (all(is.na(heartrate_data))) {
    heartrate_data1$error <- "HR calculation error"
    return(heartrate_data1)
  }
  heartrate_data$error <- "none"
  if (sampling_rate < 55) {
    heartrate_data$error <- "Low sampling rate, at least 55FPS needed"
  }
  heartrate_data$sampling_rate <- sampling_rate
  return(heartrate_data)
}

#' Bandpass and sorted mean filter the given signal
#'
#' @param x A time series numeric data
#' @param mean_filter_order Length of the sorted mean filter window
#' @param frequency_range Frequency range in Hz for the bandpass filter parameters
#' @param bandpass_order Order (length) of the bandpass filter to be used for filtering
#' @param sampling_rate The sampling rate (fs) of the time series data
#' @return The filtered time series data
get_filtered_signal <- function(x, sampling_rate, mean_filter_order = 33,
                                bandpass_order = 128, frequency_range=c(2, 25)) {
  # Defaults are set for 60Hz sampling rate
  x[is.na(x)] <- 0
  x <- x - mean(x)
  
  # Bandpass filter the given time series data
  if (sampling_rate > 55) {
    bandpass_edges <- c(frequency_range[1] * 2 / sampling_rate,
                        frequency_range[2] * 2 / sampling_rate)
    bandpass_filter <- signal::fir1(
      bandpass_order - 1,
      bandpass_edges,
      type = "pass",
      window = seewave::hamming.w(bandpass_order))
  } else {
    bandpass_edges <- frequency_range[1] * 2 / sampling_rate
    bandpass_filter <- signal::fir1(
      bandpass_order,
      bandpass_edges,
      type = "high",
      window = seewave::hamming.w(bandpass_order + 1))
  }
  
  x <- signal::filtfilt(bandpass_filter, x)
  x <- x[seq(bandpass_order / 2 + 1, length(x) - (bandpass_order / 2) + 1)]
  
  # Sorted Mean filter the given signal
  y <- 0 * x
  sequence_limits <- seq((mean_filter_order + 1) / 2,
                         length(x) - (mean_filter_order - 1) / 2, 1)
  for (i in sequence_limits) {
    temp_sequence <- x[seq(i - (mean_filter_order - 1) / 2,
                           (i + (mean_filter_order - 1) / 2))]
    
    temp_sequence <- temp_sequence - mean(temp_sequence)
    y[i] <- (((x[i] - max(temp_sequence) - min(temp_sequence)) -
              (sum(temp_sequence) - max(temp_sequence)) / (mean_filter_order - 1)) /
               (max(temp_sequence) - min(temp_sequence) + 0.0000001))
  }
  return(y)
}

#' Given a processed time series find its period using autocorrelation
#' and then convert it to heart rate (bpm)
#'
#' @param x A time series numeric data
#' @param sampling_rate The sampling rate (fs) of the time series data
#' @param min_hr Minimum expected heart rate
#' @param max_hr Maximum expected heart rate
#' @return A named vector containing heart rate and the confidence of the result 
get_hr_from_time_series <- function(x, sampling_rate, min_hr = 40, max_hr=200) {
  x[is.na(x)] <- 0
  x <- stats::acf(x, lag.max = 1000, plot = F)$acf
  y <- 0 * x
  y[seq(round(60 * sampling_rate / max_hr), round(60 * sampling_rate / min_hr))] <-
    x[seq(round(60 * sampling_rate / max_hr), round(60 * sampling_rate / min_hr))]
  confidence <- max(y) / max(x)
  hr <- 60 * sampling_rate / (which.max(y) - 1)
  
  # If hr or condidence is NaN, then return hr = 0 and confidence = 0
  if (is.na(confidence) || is.na(hr)) {
    confidence <- NA
    hr <- NA
  }
  
  return(c(hr, confidence))
}