#' Preprocess and extract heart rate from smartphone video recordings.
#' 
#' A convenience wrapper for extracting heart rate features for each color
#' band from average pixel value per frame of video (processed hr) captured
#' using smartphone cameras.
#' 
#' @param heartrate_data A data frame with columns t, red, green and blue
#' @param window_length Length of the time window in seconds, to be considered
#' while calculating the heart rate for each channel
#' @param window_overlap The overlap between consecutive windows, i.e how fine the
#' resolution of the heartrate output needs to be
#' @param method The algorithm used to calculate the heartrate, current methods
#' include ('acf','psd') which stand for autocorrelation function, and power
#' spectral density respectively. We will be adding support for peak picking 
#' algorithms, and wavelet methods later. 
#' 
#' @return list containing heart rate and confidence of the estimate for
#' each color (red, green, blue)
#' @export
#' @author Meghasyam Tummalacherla, Phil Snyder 
#' @examples 
#' heartrate_data = heartrate_data[,c('timestamp', 'red', 'green', 'blue')]
#' heartrate_ftrs = get_heartrate(heartrate_data)
#'  
get_heartrate <- function(heartrate_data, window_length = 10, window_overlap = 0.5,
                          method = 'acf') {
  ## We will throw away ~3s worth of data(180 samples) after filtering,
  ## keep this in mind
  
  heartrate_error_frame <- data.frame(red = NA, green = NA, blue = NA,
                                      error = NA, sampling_rate = NA)
  sampling_rate <- mhealthtools:::get_sampling_rate(heartrate_data)
  if (is.infinite(sampling_rate) || is.na(sampling_rate)) {
    heartrate_error_frame$error <- paste("Sampling Rate calculated from timestamp is Inf",
                                         "or NaN / timestamp not found in json")
    return(heartrate_error_frame)
  }
  
  # Convert window length from seconds to samples
  window_length <- round(sampling_rate * window_length)
  mean_filter_order <- 65
  
  ##  Apply pre-processing filter to all heartrate data
  
  # We do so as we are using an IIR (running filter), so we do not need
  # to filter each window, as for a running filter the effects are local
  # ARMA filter's output depends only on the y_i's and x_i's used
  # It is also inline with our mean centering filter, since that is also
  # a running filter
  
  heartrate_data <- tryCatch({
    heartrate_data %>% 
      dplyr::select(red, green, blue) %>% 
      na.omit() %>% 
      lapply(get_filtered_signal, sampling_rate, mean_filter_order, method) %>% 
      as.data.frame()
  }, error = function(e){NA})
  if (all(is.na(heartrate_data))) {
    heartrate_error_frame$error <- "Error in filtering the signal"
    return(heartrate_error_frame)
  }
  
  
  # Split each color into segments based on window_length
  heartrate_data <- tryCatch({
    heartrate_data %>%
      dplyr::select(red, green, blue) %>%
      na.omit() %>%
      lapply(mhealthtools:::window_signal, window_length, window_overlap, 'rectangle')
  }, error = function(e) { NA })
  if (all(is.na(heartrate_data))) {
    heartrate_error_frame$error <- "red, green, blue cannot be read from JSON"
    return(heartrate_error_frame)
  }
  
  # Get HR for each filtered segment of each color
  heartrate_data <- heartrate_data %>%
    lapply(function(dfl) {
      dfl <- tryCatch({
        apply(dfl, 2, get_hr_from_time_series, sampling_rate, method)
      }, error = function(e) {c(hr= NA, confidence = NA) })
      dfl <- as.data.frame(t(dfl))
      colnames(dfl) <- c("hr", "confidence")
      return(dfl)
    })
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
#' @return The filtered time series data
get_filtered_signal <- function(x, sampling_rate, mean_filter_order = 65, method = 'acf') {
  
  # Defaults are set for 60Hz sampling rate
  x[is.na(x)] <- 0
  x <- x - mean(x)
  
  ## Our designed signal processing filter for HR data
  #################
  ## Elliptic IIR filter design (For 60Hz Sampling Rate)
  ## We chose an Elliptic IIR, since it is an equi-ripple filter
  #################
  if(sampling_rate > 20){
    
    bandpass_params <- signal::ellipord(Wp = c(0.5/30,10/30), 
                                        Ws = c(0.3/30, 12/30),
                                        Rp = 0.001,
                                        Rs = 0.001)
  }else{
    bandpass_params <- signal::ellipord(Wp = c(0.5/15,10/15), 
                                        Ws = c(0.3/15, 12/15),
                                        Rp = 0.001,
                                        Rs = 0.001)
  }
  # If this doesn't work, use the one below
  # bandpass_params <- signal::ellipord(Wp = c(0.5/30,4/30),
  #                                     Ws = c(0.3/30, 6/30),
  #                                     Rp = 0.001,
  #                                     Rs = 0.001)
  # The reason is if we can't find a good signal in 0.7 to 10Hz,
  # we will try to find one in 0.7 to 4Hz, because of the noise/heartrate
  # (higher HR => more higher freq components/noise)
  options(warn=-1) # Turn off warnings
  # The reason we get a lot of warnings here is for the given bandpass params abovem
  # We will run into NA/Inf values for the maximum positive value while calculating the
  # filter co-efficients, so they will be replaced by value calculated using
  # machine double eps
  bandpass_filter <- signal::ellip(bandpass_params)
  options(warn=0) # Turn on warnings
  
  x <- signal::filter(bandpass_filter, x)
  x <- x[180:length(x)] # 180 samples is 3s @ 60Hz
  y <- x
  
  #################
  ## Mean centering filter design (For 60Hz Sampling Rate)
  ## The purpose of this is to make sure the waveform is uniform in range
  #################
  if(method == 'acf' || method == 'peak'){
    y <- 0 * x
    sequence_limits <- seq((mean_filter_order + 1) / 2,
                           length(x) - (mean_filter_order - 1) / 2, 1)
    for (i in sequence_limits) {
      temp_sequence <- x[seq(i - (mean_filter_order - 1) / 2,
                             (i + (mean_filter_order - 1) / 2),1)]
      
      y[i] <- (((x[i] - max(temp_sequence) - min(temp_sequence)) -
                  (sum(temp_sequence) - max(temp_sequence)) / (mean_filter_order - 1)) /
                 (max(temp_sequence) - min(temp_sequence) + 0.0000001))
      if(method == 'peak'){
        y[i] = (y[i]*(sign(y[i])+1)/2)
        y[i] = (y[i])^0.15
        y[i] = exp(y[i])^0.75
      }
    }
    y <- y[sequence_limits]
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
get_hr_from_time_series <- function(x, sampling_rate, method = 'acf', min_hr = 40, max_hr=200) {
  x[is.na(x)] <- 0
  
  if(method == 'acf'){
    x <- stats::acf(x, lag.max = 500, plot = F)$acf
    y <- 0 * x
    y[seq(round(60 * sampling_rate / max_hr), round(60 * sampling_rate / min_hr))] <-
      x[seq(round(60 * sampling_rate / max_hr), round(60 * sampling_rate / min_hr))]
    hr <- 60 * sampling_rate / (which.max(y) - 1)
    confidence <- max(y) / max(x)
  }
  
  if(method == 'psd'){
    x_spec <- mhealthtools:::get_spectrum(
      x, sampling_rate,nfreq = 2^round(log(length(x))/log(2))
    ) %>% dplyr::filter(freq>0.6, freq< 3.3)
    # 0.6Hz = 36BPM, 3.3HZ = 198BPM
    hr <- 60*x_spec$freq[which.max(x_spec$pdf)]
    confidence <- 'NAN-PSD'
  }
  
  if(method == 'peak'){
    
    x_max <- max(x)
    x_peaks <- pracma::findpeaks(x, minpeakdistance = 60 * sampling_rate / max_hr,
                                 minpeakheight = 0.85*x_max)
    
    # peak cleaning to be done
    
    # sort peaks by time when they occur not by amplitude
    x_peaks <- x_peaks[order(x_peaks[,2]),]
    peak_dist <- diff(x_peaks[,2])
    hr <- 60 * sampling_rate / (mean(peak_dist))
    confidence <- 'NAN-PEAK'
  }
  
  # If hr or condidence is NaN, then return hr = 0 and confidence = 0
  if (is.na(confidence) || is.na(hr)) {
    confidence <- NA
    hr <- NA
  }
  
  return(c(hr, confidence))
}
