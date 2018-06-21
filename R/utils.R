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
      tidyr::gather(axis, acceleration, -t)
  }, error = function(e) {
    dplyr::tibble(
      window = NA,
      error = "Could not put sensor data in tidy format by gathering the axes.")
  })
  return(tidy_sensor_data)
}

#' Detrend time series data
#' 
#' @param time Numeric vector containing the timestamp values.
#' @param acceleration Numeric vector containing the acceleration values.
#' @return Numeric vector with detrended acceleration values.
detrend <- function(time, acceleration) {
  detrended_acceleration <- loess(acceleration ~ time)$residual
  return(detrended_acceleration)
}

#' Detrend acceleration within sensor data
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @return Sensor data with detrended acceleration values.
mutate_detrend <- function(sensor_data) {
  if (has_error(sensor_data)) return(sensor_data)
  detrended_sensor_data <- tryCatch({
    detrended_sensor_data <- sensor_data %>%
      dplyr::group_by(axis) %>%
      dplyr::mutate(
        acceleration = detrend(t, acceleration)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Detrend error")
  })
  return(detrended_sensor_data)
}

#' Apply a pass-band filter to time series data
#' 
#' @param acceleration Numeric vector containing acceleration values.
#' @param window_length Length of the filter.
#' @param sampling_rate Sampling rate of the acceleration data.
#' @param frequency_low Lower bound on frequency in Hz
#' @param frequency_high Upper bound on frequency in Hz
#' @return Filtered time series data
bandpass <- function(acceleration, window_length, sampling_rate,
                     frequency_range) {
  frequency_low <- frequency_range[1]
  frequency_high <- frequency_range[2]
  if(frequency_low*2/sampling_rate > 1 || frequency_high*2/sampling_rate > 1) {
    stop("Frequency parameters can be at most half the sampling rate.")
  } else if (any(is.na(acceleration))) {
    stop("NA values present in input.")
  }
  bandpass_filter <- signal::fir1(
    window_length-1,
    c(frequency_low*2/sampling_rate,
      frequency_high*2/sampling_rate),
    type="pass",
    window=seewave::hamming.w(window_length))
  filtered_acceleration <- signal::filtfilt(bandpass_filter, acceleration)
  return(filtered_acceleration)
}

#' Apply a pass-band filter to sensor data
#' 
#' @param sensor_date A data frame with columns t, axis, acceleration.
#' @param window_length Length of the filter.
#' @param sampling_rate Sampling rate of the acceleration data.
#' @param frequency_range Bounds on frequency in Hz
#' @return Filtered time series data
mutate_bandpass <- function(sensor_data, window_length, sampling_rate,
                            frequency_range) {
  if (has_error(sensor_data)) return(sensor_data)
  bandpass_filtered_sensor_data <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis) %>%
      dplyr::mutate(
        acceleration = bandpass(acceleration, window_length, sampling_rate,
                                frequency_range)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Bandpass filter error")
  })
  return(bandpass_filtered_sensor_data)
}

#' Select a specific time range from sensor data.
#' 
#' @param sensor_date A data frame with columns t, axis, acceleration.
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
    dplyr::tibble(window = NA, error = "Not enough time samples")
  })
}

#' Window the acceleration vector of sensor data by axis
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param window_length Length of the filter
#' @param overlap window overlap
#' @return Windowed sensor data
window <- function(sensor_data, window_length, overlap) {
  if (has_error(sensor_data)) return(sensor_data)
  tryCatch({
    windowed_sensor_data <- sensor_data %>%
      tidyr::spread(axis, acceleration) %>% 
      dplyr::select(x, y, z) %>%
      purrr::map(windowSignal, 
                 window_length = window_length, overlap = overlap)
    tidy_windowed_sensor_data <- lapply(
      windowed_sensor_data,
      function(windowed_matrix) {
        windowed_matrix <- cbind(index = 1:dim(windowed_matrix)[1],
                                 windowed_matrix)
        tidy_tibble <- windowed_matrix %>% 
          dplyr::as_tibble() %>%
          tidyr::gather(window, acceleration, -index, convert=T)
        return(tidy_tibble)
      }) %>% 
      dplyr::bind_rows(.id = "axis")
    return(tidy_windowed_sensor_data)
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Windowing error")
  })
}

#' Window a signal
#'  
#' Given a acceleration vector this function will return a windowed 
#' signal with hamming window.
#'  
#' @param accel Timeseries vector of length n.
#' @param window_length Length of the filter.
#' @param overlap Window overlap.
#' @return A matrix of window_length x nwindows windowed acceleration matrix.
windowSignal <- function(accel, window_length = 256, overlap = 0.5){
  nlen = length(accel)
  
  # If length of signal is less than window length
  if (nlen < window_length){
    window_length = nlen; overlap = 1;
  }
  
  nstart = seq(1, nlen, window_length*overlap)
  nend = seq(window_length, nlen, window_length*overlap)
  nstart = nstart[1:length(nend)]
  wn = seewave::hamming.w(window_length)
  
  a = apply(cbind(nstart,nend), 1, function(x, a, wn){
    a[seq(x[1],x[2],1)]*wn
  }, accel, wn)
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

#' Add jerk column to sensor data
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @param sampling_rate Sampling rate of the acceleration data.
#' @return Sensor data with jerk column.
mutate_jerk <- function(sensor_data, sampling_rate) {
  if (has_error(sensor_data)) return(sensor_data)
  sensor_data_with_jerk <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis, window) %>% 
      dplyr::mutate(jerk = jerk(acceleration, sampling_rate)) %>%
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Error calculating jerk")
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
      dplyr::group_by(axis, window) %>%
      dplyr::mutate(velocity = velocity(acceleration, sampling_rate)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Error calculating velocity")
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
      dplyr::group_by(axis, window) %>%
      dplyr::mutate(displacement = displacement(acceleration, sampling_rate)) %>% 
      dplyr::ungroup()
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Error calculating displacement")
  })
  return(sensor_data_with_displacement)
}

#' Construct a dataframe with ACF values
#' 
#' Estimate the ACF for windowed sensor data.
#' 
#' @param sensor_data A data frame with columns t, axis, acceleration.
#' @return A tibble with columns axis, window, index, acf
calculate_acf <- function(sensor_data) {
  if (has_error(sensor_data)) return(sensor_data)
  acf_data <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis, window) %>%
      tidyr::nest(acceleration) %>%
      dplyr::mutate(data = map(data, function(d) {
        acf_col <- stats::acf(d$acceleration, plot=F)$acf
        index_col <- 1:length(acf_col)
        dplyr::bind_cols(acf = acf_col, index = index_col)
      })) %>%
      tidyr::unnest(data) %>% 
      select(axis, window, index, acf)
  }, error = function(e) {
    dplyr::tibble(window = NA, error = "Error calculating ACF")
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
  gravity_summary <-
    windowSignal(gravity_vector, window_length, overlap) %>%
    dplyr::as_tibble() %>%
    tidyr::gather(window, value) %>%
    dplyr::group_by(window) %>%
    dplyr::summarise(max = max(value, na.rm = T),
                     min = min(value, na.rm = T))
  return(gravity_summary)
}

#' Identify abnormal device rotations
#' 
#' Identify windows in which the phone may have been rotated or flipped,
#' as indicated by a gravity vector
#' 
#' @param gravity A gravity vector
#' @param window_length Length of the filter.
#' @param overlap Window overlap.
#' @return Rotations errors for each window.
tag_outlier_windows <- function(gravity, window_length, overlap) {
  gr_error <- tryCatch({
    gr_error <- gravity %>% 
      purrr::map(tag_outlier_windows_, window_length, overlap) %>%
      dplyr::bind_rows(.id = 'axis') %>%
      dplyr::mutate(error = sign(max) != sign(min)) %>% 
      dplyr::group_by(window) %>% 
      dplyr::summarise(error = any(error, na.rm = T))
    gr_error$error[gr_error$error == TRUE] = 'Phone rotated within window'
    gr_error$error[gr_error$error == FALSE] = 'None'
    return(gr_error)
  }, error = function(e) {
    dplyr::tibble(window = "NA", error = "Error tagging outlier windows")
  })
  return(gr_error)
}

#' Get time domain features
#' 
#' Calculates features characterising a time series in the time domain.
#' 
#' @param accel An acceleration vector.
#' @param sampling_rate Sampling_rate of the acceleration vector.
#' @return A features data frame of dimension 1 x n_features
time_domain_summary <- function(accel, sampling_rate=NA) {
  if(is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for time_domain_summary")
    sampling_rate = 100
  }
  ftrs <- dplyr::tibble(
    mean = mean(accel, na.rm = TRUE),
    median = quantile(accel, probs = c(0.5), na.rm = TRUE),
    mode = pracma::Mode(accel),
    mx =  max(accel, na.rm = T),
    mn = min(accel, na.rm = T),
    sd = sd(accel, na.rm = TRUE),
    skewness = e1071::skewness(accel),
    kurtosis = e1071::kurtosis(accel),
    Q25 = quantile(accel, probs = c(0.25), na.rm = TRUE),
    Q75 = quantile(accel, probs = c(0.75), na.rm = TRUE),
    range = max(accel, na.rm = T) - min(accel, na.rm = T),
    rough = seewave::roughness(accel),
    rugo = seewave::rugo(accel),
    energy = sum(accel^2),
    mobility = sqrt(var(diff(accel)*sampling_rate)/var(accel)),
    mtkeo = mean(seewave::TKEO(
      accel, f = sampling_rate, plot = F)[,2], na.rm = T),
    dfa = fractal::DFA(accel, sum.order = 1)[[1]],
    rmsmag = sqrt(sum(accel^2)/length(accel))) %>%  # Root Mean Square magnitude
    dplyr::mutate(IQR = Q25 - Q75,
                  complexity = sqrt(
                    var(diff(diff(accel)*sampling_rate)*sampling_rate) /
                      var(diff(accel)*sampling_rate)))
  names(ftrs) = stringr::str_c(names(ftrs), '.tm')
  return(ftrs)
}

#' Get frequency domain features
#' 
#' Calculates features characterising a time series in the frequency domain.
#' 
#' @param accel An acceleration vector.
#' @param sampling_rate Sampling_rate of the acceleration vector.
#' @param npeaks Number of peaks to be computed in EWT
#' @return A features data frame of dimension 1 x num_features
#####
frequency_domain_summary <- function(accel, sampling_rate=NA, npeaks = NA) {
  if(is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for time_domain_summary")
    sampling_rate = 100
  }

  if(is.na(npeaks)) {
    warning("Using default npeaks of 3 for frequency_domain_summary")
    npeaks = 3
  }
  spect <- getSpectrum(accel, sampling_rate)
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
  
  names(ftrs) <- stringr::str_c(names(ftrs), '.fr')
  
  return(data.frame(ftrs))
}


#' Get AR spectrum
#' 
#' Given an acceleration vector, this function will return a spectrum
#' with all pole AR model.
#' 
#' @param accel A timeseries vector.
#' @param sampling_rate Sampling rate of the signal (by default it is 100 Hz).
#' @param nfreq Number of frequecy points to be interpolated.
#' @return An AR spectrum.
getSpectrum <- function(accel, sampling_rate = 100, nfreq = 500){
  tmp = stats::spec.ar(accel, nfreq = nfreq, plot = F)
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
#' Given an acceleration vector, this function will return features
#' characterising the time series in frequency domain by energy
#' 
#' @param accel A timeseries vector.
#' @param sampling_rate Sampling rate of the signal (by default it is 100 Hz).
#' @return A features data frame of dimension 1 x 48.
frequency_domain_energy <- function(accel, sampling_rate=NA) {
  if(is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for frequency_domain_energy")
    sampling_rate = 100
  }  
  spect = getSpectrum(accel, sampling_rate)
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
#' @param .x A (non-grouped) tibble
#' @param groups A character vector specifying which columns to group on.
#' @param col Column to pass as a vector to \code{.f}.
#' @param .f Function to be mapped to \code{col} for each group.
#' @param ... Additional arguments to \code{.f}.
#' @return A tibble indexed by groups with an additional column containing
#' the output of the mapped function.
map_groups <- function(x, groups, col, f, ...) {
  dots <- rlang::enquos(...) # can also use enexprs()
  x %>%
    dplyr::group_by_at(.vars = groups) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, ~ f(.[[col]], !!!dots))) %>%
    tidyr::unnest(data)
}
