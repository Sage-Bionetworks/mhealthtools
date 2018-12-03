context("Sensors")

test_that("Wrapper to extract accelerometer features", {
  expect_is(accelerometer_features(
    sensor_data = mini_accelerometer_data), "list")
})

test_that("Wrapper to extract gyroscope features", {
  expect_is(gyroscope_features(sensor_data = mini_gyroscope_data), "list")
})

test_that(
  "Return arguments to be used in general feature functions", {
    expect_is(
      prepare_kinematic_sensor_args(
        sensor_data = mini_accelerometer_data,
        metric = "acceleration",
        window_length = 256,
        window_overlap = 0.5,
        time_filter = c(1, 9),
        frequency_filter = c(1, 25)), "list")
})

test_that(
  "Initialize windowing transformation function", {
  expect_is(
    transformation_window(
      window_length = 256,
      window_overlap = 0.5),
    "function")
})

test_that(
  "Initialize IMF windowing transformation function", {
  expect_is(
    transformation_imf_window(
      window_length = 256,
      window_overlap = 0.5,
      max_imf = 4),
    "function")
})

test_that(
  "Initialize list of default kinematic feature extraction functions", {
    expect_is(
      default_kinematic_features(sampling_rate = 100),
      "list")
})

test_that("Extract kinematic sensor features", {
  expect_is(
    kinematic_sensor_features(
      sensor_data = mini_accelerometer_data,
      preprocess = NULL,
      transformation = NULL,
      extract = NULL,
      extract_on = NULL,
      models = NULL,
      acf_col = NULL),
    "list")
  expect_is(
    kinematic_sensor_features(
      sensor_data = mini_accelerometer_data,
      preprocess = mutate_detrend,
      transformation = NULL,
      extract = NULL,
      extract_on = NULL,
      models = NULL,
      acf_col = NULL),
    "list")
  expect_is(
    kinematic_sensor_features(
      sensor_data = mini_accelerometer_data,
      preprocess = list(purrr::partial(filter_time, t1 = 0.5, t2 = 1.9),
                        mutate_detrend),
      transformation = NULL,
      extract = NULL,
      extract_on = NULL,
      models = NULL,
      acf_col = NULL),
    "list")
})


test_that("Extract sensor features", {
  expect_is(sensor_features(sensor_data = mini_accelerometer_data), "list")
})