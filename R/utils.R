### Utility/helper functions to be called by sensor modules ###
#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs

get_sampling_rate <- function(sensor_data) {
  t_length = length(sensor_data$t)
  sampling_rate = t_length / (sensor_data$t[t_length] - sensor_data$t[1])
  return(sampling_rate)
}

gather_axis <- function(sensor_data) {
  gathered_axis <- tryCatch({
    t0 <- sensor_data$t[1]
    normalized_sensor_data <-  sensor_data %>% mutate(t = t - t0)
    index = order(sensor_data$t)
    gathered_axis = normalized_sensor_data[index,] %>%
                    tidyr::gather(axis, metric, -t)
  }, error = function(e) { NA })
  return(gathered_axis)
}

detrend <- function(sensor_data) {
  detrended_sensor_data <- tryCatch({
    detrended_sensor_data <- sensor_data %>%
                             dplyr::group_by(axis) %>%
                             dplyr::mutate(metric = loess(metric ~ t)$residual)
  }, error = function(e) { NA })
  return(detrended_sensor_data)
}

bandpass <- function(sensor_data, window_length, sampling_rate, frequency_range) {
  bandpass_filter <- signal::fir1(window_length-1,
                                  c(frequency_range[1]*2/sampling_rate,
                                    frequency_range[2]*2/sampling_rate),
                                  type="pass",
                                  window=seewave::hamming.w(window_length))
  bandpass_filtered_sensor_data <- tryCatch({
    sensor_data %>%
      dplyr::group_by(axis) %>%
      dplyr::mutate(metric = signal::filtfilt(bandpass_filter, metric))
  }, error = function(e) { NA })
  return(bandpass_filtered_sensor_data)
}

filter_time <- function(sensor_data, t1, t2) {
  filtered_time_sensor_data <- tryCatch({
    filtered_time_sensor_data <- sensor_data %>% dplyr::filter(t >= t1, t <= t2)
    return(filtered_time_sensor_data)
  }, error = function(e) { NA })
}