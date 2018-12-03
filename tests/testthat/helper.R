# Shared test functions

### check for error dataframe
is_error_dataframe <- function(df) {
  return(rlang::has_name(df, "error") && !is.na(df$error))
}

### Get minified accelerometer and gyroscope data 
mini_accelerometer_data <- dplyr::filter(accelerometer_data, t < 2)
mini_gyroscope_data <- dplyr::filter(gyroscope_data, t < 2)
mini_gravity_data <- dplyr::filter(gravity_data, t < 2)