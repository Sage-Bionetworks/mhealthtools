#' Returns statistical summary of the time series
#'
#' A convenience feature extraction function which characterises a given
#' time series in to statistical features in the time domain.
#'
#' @param values A numeric vector.
#' @param sampling_rate Sampling_rate of \code{values}. If NA it uses default
#' sampling rate of 100Hz.
#' @return A features data frame of dimension 1 x 20. See the
#' feature definition vignette:
#' \code{vignette("feature_definitions", package="mhealthtools")}
#' @author Thanneer Malai Perumal, Phil Snyder
#' @export
#' @examples
#' time_features = time_domain_summary(
#'   accelerometer_data$x,
#'   sampling_rate = 100.122)
time_domain_summary <- function(values, sampling_rate=NA) {
  if (is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for time_domain_summary")
    sampling_rate <- 100
  }
  features <- dplyr::tibble(
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
    mobility = sqrt(var(diff(values) * sampling_rate) / var(values)),
    mtkeo = mean(seewave::TKEO(
      values, f = sampling_rate, plot = F)[, 2], na.rm = T),
    dfa = tryCatch({fractal::DFA(values, sum.order = 1)[[1]]},
                   error =  function(e){NA}),
    rmsmag = sqrt(sum(values^2) / length(values))) %>% # Root Mean Square magnitude
    dplyr::mutate(IQR = Q25 - Q75,
                  complexity = sqrt(
                    var(diff(diff(values) * sampling_rate) * sampling_rate) /
                      var(diff(values) * sampling_rate)))
  names(features) <- paste0(names(features), ".tm")
  return(features)
}

#' Returns statistical summary of the frequency spectrum
#'
#' A convenience feature extraction function that characterises the
#' frequency spectrum of a given time series in to statistical features
#' in the frequency domain.
#'
#' @param values A numeric vector from a time series measurement.
#' @param sampling_rate Sampling_rate of \code{values}. If NA it uses default
#' sampling rate of 100Hz.
#' @param npeaks Number of peaks to be computed in emprical wavelet transformation (EWT).
#' If NA it uses the default value of 3.
#' @return A features data frame of dimension 1 x 19. See the
#' feature definition vignette:
#' \code{vignette("feature_definitions", package="mhealthtools")}
#' @author Thanneer Malai Perumal, Phil Snyder
#' @export
#' @examples
#' frequency_features = frequency_domain_summary(
#'   accelerometer_data$x,
#'   sampling_rate = 100.122,
#'   npeaks = 3)
frequency_domain_summary <- function(values, sampling_rate = NA, npeaks = NA) {
  if (is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for time_domain_summary")
    sampling_rate <- 100
  }
  if (is.na(npeaks)) {
    warning("Using default npeaks of 3 for frequency_domain_summary")
    npeaks <- 3
  }
  spect <- get_spectrum(values, sampling_rate)
  freq <- spect$freq
  pdf <- spect$pdf / sum(spect$pdf, na.rm = T)
  pdf_adjusted <- pdf - mean(pdf)
  cdf <- cumsum(pdf)
  w <- sd(pdf)

  # Get STFT spectrum based features
  features <- dplyr::tibble(
    mn = sum(freq * pdf),
    mx = max(pdf),
    sd = sqrt(sum(pdf * ((freq - mn)^2))),
    sem = sd / sqrt(dim(spect)[1]),
    md = freq[length(cdf[cdf <= 0.5]) + 1],
    mod = freq[which.max(pdf)],
    Q25 = freq[length(cdf[cdf <= 0.25]) + 1],
    Q75 = freq[length(cdf[cdf <= 0.75]) + 1],
    IQR = Q75 - Q25,
    cent = sum(freq * pdf),
    skew = (sum((pdf_adjusted)^3) / (dim(spect)[1] - 1)) / w^3,
    kurt = (sum((pdf_adjusted)^4) / (dim(spect)[1] - 1)) / w^4,
    sfm = seewave::sfm(pdf),
    sh = seewave::sh(pdf))

  # Get EWT spectrum
  ewt_spectrum <- data.frame(freq = freq, pdf = pdf) %>%
    get_ewt_spectrum(sampling_rate = sampling_rate, npeaks = npeaks)

  # Compute normalised point energies of each EW spctrum
  ew_energy <- colSums(ewt_spectrum^2, na.rm = T)
  ew_energy <- ew_energy / sum(ew_energy, na.rm = T)

  # Compute entropy with EWT approximated energies
  features <- features %>%
    dplyr::mutate(
      ewt.permEnt = statcomp::permutation_entropy(ew_energy),
      ewt.shannonEnt = seewave::sh(ew_energy, alpha = "shannon"),
      ewt.simpsonEnt = seewave::sh(ew_energy, alpha = "simpson"),
      ewt.renyiEnt = seewave::sh(
        ew_energy, alpha = 2), # alpha is hardcoded to be 2
      ewt.tsallisEnt = (
        1 - sum(ew_energy ^ 0.1)) / (0.1 - 1)) # q is hardcoded to be 0.1

  names(features) <- paste0(names(features), ".fr")

  return(data.frame(features))
}

#' Returns energy for each 0.5Hz band in the frequency spectrum.
#'
#' A convenience feature extraction function that converts a given
#' time series in to frequency spectrum and computes energy in each
#' 0.5Hz band from 0 to 25 Hz.
#'
#' @param values A numeric vector.
#' @param sampling_rate Sampling_rate of \code{values}. If NA it uses default
#' sampling rate of 100Hz.
#' @return A features data frame of dimension 1 x 48, each representing energy within
#' a 0.5Hz band. See the
#' feature definition vignette:
#' \code{vignette("feature_definitions", package="mhealthtools")}
#' @author Thanneer Malai Perumal, Phil Snyder
#' @export
#' @examples
#' frequency_energy_features = frequency_domain_energy(
#'   accelerometer_data$x,
#'   sampling_rate = 100.122)
frequency_domain_energy <- function(values, sampling_rate=NA) {
  if (is.na(sampling_rate)) {
    warning("Using default sampling rate of 100 for frequency_domain_energy")
    sampling_rate <- 100
  }
  spect <- get_spectrum(values, sampling_rate)
  freq <- spect$freq
  pdf <- spect$pdf / sum(spect$pdf, na.rm = T)

  st <- seq(1, 24.5, 0.5)
  en <- seq(1.5, 25, 0.5)

  features <- mapply(function(ind_str, ind_en){
    ind <- which(freq >= ind_str & freq <= ind_en)
    pracma::trapz(freq[ind], pdf[ind])
  }, st, en) %>% t %>% data.frame()
  colnames(features) <- paste0("EnergyInBand", gsub("\\.", "_", st))

  return(features)
}