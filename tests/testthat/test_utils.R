context("Utility functions")

test_that("Check Sampling rate calculation", {
  samplingRate <- (length(accelerometer_data$t)) /
                 (max(accelerometer_data$t)-min(accelerometer_data$t))
  expect_equal(get_sampling_rate(sensor_data = accelerometer_data), samplingRate)
  expect_equal(get_sampling_rate(sensor_data = NA),
                         NA)
})

test_that(paste("Function to extract left, right tapping events",
                "and intertap intervals"), {
    expect_is(
      get_left_right_events_and_tap_intervals(tap_data = tap_data),
      "list")
    expect_is(
      get_left_right_events_and_tap_intervals(
        tap_data = tap_data,
        depress_threshold = 10),
      "list")
    tempDat <- tap_data[1:2,]
    # Only 2 rows, lesser than the required 5, Should expect an error
    expect_equal(
      get_left_right_events_and_tap_intervals(tap_data = tempDat),
      list(tap_data = NA, tap_intervals = NA, error = TRUE))
})

tapInter <- get_left_right_events_and_tap_intervals(
  tap_data = tap_data)$tap_intervals

test_that("Extract default inter tap time features", {
  expect_is(
    intertap_summary_features(tap_intervals = tapInter), "data.frame")
})

test_that("Extract default tap drift features", {
  # tap drift is a numeric vector, like tapInter. So we are going to
  # use tapInter to test the function rather than creating a seperate
  # tapDrift numeric vector
  expect_is(
    tapdrift_summary_features(tap_drift = tapInter), "data.frame")
})

test_that(
  "Extract deafult tap data features (features based on interaction
  between x and y, etc.)", {
    expect_is(
      tap_data_summary_features(tap_data = tap_data), "data.frame")
})

test_that("Tidying sensor data", {
  expect_is(
    tidy_sensor_data(sensor_data = accelerometer_data), "data.frame")
  tempDat <- data.table::copy(accelerometer_data)
  tempDat$t[1] <- NA # time column now has a NA in it
  expect_error(
    tidy_sensor_data(sensor_data = tempDat))
  tempDat$error <- rep(NA, length(tempDat$t))
  tempDat$error[1] <- "some error" # Just a non NA value for error
  # tidy_sensor_data should return the input, i.e, an identity function
  # because there is a non-zero error value in the error column
  expect_equal(tidy_sensor_data(sensor_data = tempDat), tempDat)
  tempDat <- tempDat %>% dplyr::select(-t, -error)
  # Removing the t and error columns, tidy_sensor_data throw an error
  expect_equal(is_error_dataframe(
    suppressWarnings(tidy_sensor_data(sensor_data = tempDat))), T)
})


# get tidy data as we will use that a lot to test the rest of the functions
accelerometer_dataTidy <- tidy_sensor_data(sensor_data = accelerometer_data)
gyroscope_dataTidy <- tidy_sensor_data(sensor_data = gyroscope_data)

test_that("Detrend the data given a time series", {
  expect_is(
    detrend(time = accelerometer_data$t, values =  accelerometer_data$y), "numeric")
  # Check to see how NA"s are handled (they are removed, i.e.,
  # below we will see points 2,3,4,6 as 1 has (time NA),5 has (values NA))
  suppressWarnings(detrend(time = c(NA,2,3,4,5,6), values = c(10,20,30,40,NA,60)))
})

test_that("Detrend the given sensor data", {
  expect_is(
    mutate_detrend(sensor_data = accelerometer_dataTidy), "data.frame")
  # Given wrong format of data, the function should throw an error
  expect_equal(is_error_dataframe(
    mutate_detrend(sensor_data = accelerometer_data)), T)
})

test_that("Bandpass a timeseries data", {
  # actual function in utils: bandpass
  testTimeSeries <- accelerometer_data$x
  # This is 990 points long timeseries, sampled at 100Hz
  expect_is(bandpass(values = testTimeSeries,
                     window_length = 120,
                     sampling_rate =  100,
                     frequency_range = c(1, 25)), "numeric")
  expect_error( # Frequency parameters violating Nyquist criterion
    bandpass(values = testTimeSeries,
             window_length =  120,
             sampling_rate =  100,
             frequency_range =  c(1, 100)),
    "Frequency parameters can be at most half the sampling rate.")
  expect_error(
    bandpass(values = rep(NA, 990),
             window_length =  120,
             sampling_rate = 100,
             frequency_range =  c(1, 10)))
})

test_that("Bandpass the tidy sensor data", {
  expect_is(
    mutate_bandpass(sensor_data = accelerometer_dataTidy,
                    window_length =  120,
                    sampling_rate =  100,
                    frequency_range =  c(1, 25)), "data.frame")
  expect_equal(is_error_dataframe(
    mutate_bandpass(
      sensor_data = accelerometer_data,
      window_length = 120,
      sampling_rate = 100,
      frequency_range = c(1, 25))), T)
  # Error if input data is of wrong format
  expect_equal(is_error_dataframe(
    mutate_bandpass(
      sensor_data = accelerometer_dataTidy,
      window_length = 120,
      sampling_rate = 100,
      frequency_range = c(1, 51))), T)
  # Error if freq ranges of bandpass filter violate Nyquist criterion
})

test_that("Filtering the time series data by selecting a time range", {
    expect_is(filter_time(sensor_data = accelerometer_dataTidy, t1 = 1, t2 = 2),
              "data.frame")
    expect_error(
      filter_time(sensor_data = accelerometer_data[, "x"], t1 = 1, t2 = 2))
    # throw an error if there is no t column
    tempDat <- data.frame(t = c(NA, seq(10), NA), x = sample(100, 12) / 10)
    tempDat$t[5] <- NA
    expect_is(filter_time(sensor_data = tempDat, t1 = 1, t2 = 20),
              "data.frame")
    # NA behavior, they are removed i.r t = c(1,2,NA,4,5,NA), t1 = 2, t2 = 4,
    # then the output will have t = c(2,4), NA"s (3 was missing) are removed
    expect_equal(has_error(filter_time(accelerometer_dataTidy, 11, 15)), TRUE)
})

test_that("Windowing a time series", {
  expect_is(window_signal(values = accelerometer_data$x), "matrix")
  # If input is all NA"s except for a value(the last one, we should expect something
  # similar, all NAs except for one value, which is what we see, also if we input all
  # NA"s we see an output with all NA"s )
  gravityVec <- c(rep(NA, 255), 1)
  expect_is(window_signal(values = gravityVec), "matrix")

  ############
  # UNCOMMENT FOLLOWING TEST AFTER EDITING window_signal
  ############
  # tempVec <- seq(1,50) # length 10
  # expect_error(window_signal(values = tempVec,
  #                                                     window_length = 256,
  #                                                     window_overlap = 0.5))
  # Above we have a length 50, and a window_length of 256,
  # if you plot tempVal(the output of window_signal), you will see a lot
  # of spurious data, which we don"t need
  # We should throw an error if the window_length is less than that of
  # the input signal, or correct it by changing the window_length to that
  # of the input, or zero-pad the input to get to the signal of the length
  # window_length
})

test_that("Windowing the sensor data by axis", {
  # actual function in utils: window
  expect_is(window(sensor_data = accelerometer_dataTidy,
                   window_length =  256,
                   window_overlap =  0.5), "data.frame")

  ############
  # UNCOMMENT FOLLOWING TEST AFTER EDITING window
  ############
  #
  # tempDat <- accelerometer_dataTidy %>%
  #   dplyr::filter(t < 2)
  # # A max possible window length of 2s ~ 2 x 100(sampling rate) = 200 samples
  # expect_equal(is_error_dataframe(
  #   window(sensor_data = tempDat,
  #                         window_length =  256,
  #                         window_overlap =  0.5)), T)
  # We should throw an error if window_length is more than that possible,
  # or we should handle these errors differently
  # Maybe throw an error frame with "window_length greater than max t" etc.,
  # Look at the test for window_signal that tackles the problem at a unit level,
  # because the above test for window would be a regression test.

  expect_equal(is_error_dataframe(
    window(
      sensor_data = accelerometer_data,
      window_length = 256,
      window_overlap = 0.5)), T)
  # throw an error if Input is not in correct format
})

test_that("Compute start and stop timestamp for each window", {
  expect_is(window_start_end_times(t = mini_accelerometer_data$t,
                                   window_length = 64,
                                   window_overlap = 1), "data.frame")
  expect_error(window_start_end_times(t = mini_accelerometer_data$t,
                                      window_length = NA,
                                      window_overlap = 0.5))
  start_end_times <- window_start_end_times(t = mini_accelerometer_data$t,
                                            window_length = 64,
                                            window_overlap = 0.65)
  expect_equal(is_integer(start_end_times$window), TRUE)
  expect_equal(is.numeric(start_end_times$window_start_time), TRUE)
  expect_equal(is.numeric(start_end_times$window_end_time), TRUE)
  expect_equal(is_integer(start_end_times$window_start_index), TRUE)
  expect_equal(is_integer(start_end_times$window_end_index), TRUE)
})

test_that(
  "Calculate Derivative given acceleration and sampling rate", {
    expect_is(derivative(v = accelerometer_data$x), "numeric")
})

test_that("Calculate and add a derivative column for the sensor data", {
    expect_is(
      mutate_derivative(sensor_data = accelerometer_data,
                        sampling_rate = 100,
                        col = "x",
                        derived_col = "dx"), "data.frame")

    expect_equal(is_error_dataframe(
      mutate_derivative(
        sensor_data = accelerometer_dataTidy,
        sampling_rate = 100,
        col = "x",
        derived_col = "dx")), T)
    expect_equal(is_error_dataframe(
      mutate_derivative(
        sensor_data = accelerometer_dataTidy,
        col = "x",
        derived_col = "dx")), T)
})

test_that("Calculate Integral given acceleration and sampling rate", {
    expect_is(integral(v = accelerometer_data$x), "numeric")
})

test_that("Calculate and add a integral column for the sensor data", {
    expect_is(mutate_integral(sensor_data = accelerometer_data,
                              sampling_rate = 100,
                              col = "x",
                              derived_col = "dx"), "data.frame")
})

test_that("Construct a dataframe with ACF values given tidy sensor data", {
    expect_is(mutate_acf(sensor_data = accelerometer_dataTidy, col = "value"), "data.frame")
    expect_equal(is_error_dataframe(
      mutate_acf(sensor_data = accelerometer_data, col = "value")), T)
})

test_that("Identify min, max gravity values for each window of tidy sensor data", {
    gravityVec <- gravity_data$x
    expect_is(tag_outlier_windows_(gravity_vector = gravityVec,
                                   window_length = 256,
                                   window_overlap =  0.5), "data.frame")
    gravityVec <- c(rep(NA, 255), 1)
    testOutput <- tag_outlier_windows_(
      gravity_vector = gravityVec,
      window_length = 256,
      window_overlap =  0.5) %>%
      as.data.frame()
    expect_equal(
      testOutput, data.frame(window = as.character(1),
                             max = as.numeric(NA),
                             min = as.numeric(NA),
                             stringsAsFactors=FALSE) )
})

test_that(paste("Identify windows in which Phone might have been",
                "flipped/rotated given gravity"), {
    expect_is(tag_outlier_windows(gravity = gravity_data,
                                  window_length = 256,
                                  window_overlap = 0.5), "data.frame")
    expect_equal(
      is_error_dataframe(
        tag_outlier_windows(gravity = gravity_data[1:100],
                            window_length = 256,
                            window_overlap = 0.5)), T)
    # Throw an error frame if window_length more than gravity
})

test_that("Time domain summary given acceleration", {
  accelVec <- accelerometer_data$x[1:256]
  expect_is(
    time_domain_summary(values = accelVec, sampling_rate = 100), "data.frame")
})

test_that("Frequency domain summary given acceleration", {
  accelVec <- accelerometer_data$x[1:256]
  expect_is(frequency_domain_summary(values = accelVec,
                                     sampling_rate = 100,
                                     npeaks = 3), "data.frame")
})

test_that("Frequency domain energy given acceleration", {
  accelVec <- accelerometer_data$x[1:256]
  expect_is(frequency_domain_energy(values = accelVec,
                                    sampling_rate = 100), "data.frame")
})

test_that("Get Spectrum given a time series and sampling rate", {
  accelVec <- accelerometer_data$x[1:512]
  expect_is(get_spectrum(values = accelVec,
                         sampling_rate = 100,
                         nfreq = 500), "data.frame")
  expect_equal(
    dim(get_spectrum(
      accelVec, sampling_rate = 100, nfreq = 256))[1], 256)
  # If nfreq is 256 points I need a spectrum that has 256 points in it, not 500!
})

test_that("Get EWT spectrum", {
  accelVec <- accelerometer_data$x[1:512] # A 512 length acceleration vector
  accelVecSpec <- get_spectrum(values = accelVec,
                               sampling_rate = 100,
                               nfreq = 500) # 500x2 spectrum
  expect_is(get_ewt_spectrum(spectrum = accelVecSpec), "matrix")
})

test_that("fatigue", {
  expect_is(fatigue(x = tapInter), "list")
})

test_that("Calculate Drift", {
  expect_is(calculate_drift(x = tap_data$x, y = tap_data$y), "numeric")
})

test_that("Mean Teager-Kaiser Energy (mtkeo)", {
  expect_is(mean_tkeo(x = tapInter), "numeric")
  testDat <- tapInter
  expect_equal(mean_tkeo(x = testDat), 0.01147268)
})

test_that("Co-efficient of Variation (coef_var)", {
  expect_is(coef_var(x = tapInter), "numeric")
  testDat <- tapInter
  expect_equal(coef_var(x = testDat), 86.62279)
})