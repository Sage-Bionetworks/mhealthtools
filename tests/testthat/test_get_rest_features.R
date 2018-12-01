context("get_rest_features")

test_that("No arguments", {
  expect_equal(get_rest_features(),
               list(extracted_features = NULL,
                    model_features = NULL,
                    error = NULL,
                    outlier_windows = NULL))
})
  
test_that("Accelerometer data only", {
  features_accel_only <- get_rest_features(
    accelerometer_data = mini_accelerometer_data)
  expect_is(features_accel_only, "list")
  expect_is(features_accel_only$extracted_features, "data.frame")
})
# the test case is symmetrical for gyroscope data only, skipping
test_that("Both accelerometer and gyroscope data", {
  features_both <- get_rest_features(
      accelerometer_data = mini_accelerometer_data,
      gyroscope_data = mini_gyroscope_data)
  expect_is(features_both, "list")
  expect_is(features_both$extracted_features, "data.frame")
})
  
test_that("Gravity data", {
  features_with_gravity <- get_rest_features(
    accelerometer_data = mini_accelerometer_data,
    gravity_data = mini_gravity_data,
    window_length = 64,
    window_overlap = 0.5)
  expect_is(features_with_gravity, "list")
  expect_is(features_with_gravity$outlier_windows, "data.frame")
})
  
test_that("Passing a model", {
  custom_model <- function(dat) {
    feature <- dat[, 2]
    feature <- feature %>%  unlist() %>%  as.numeric()
    return(data.frame(f1 = mean(feature, na.rm = T)))
  }
  features_with_model <- get_rest_features(
      accelerometer_data = mini_accelerometer_data,
      gyroscope_data = mini_gyroscope_data,
      models = custom_model)
  expect_is(features_with_model, "list")
  expect_is(features_with_model$model_features, "list")
  expect_is(features_with_model$model_features$accelerometer[[1]],
            "data.frame")
  expect_is(features_with_model$model_features$gyroscope[[1]],
            "data.frame")
})
  
test_that("Input data contains NA", {
  expect_equal(is_error_dataframe(
    get_rest_features(accelerometer_data = NA)), T)
  expect_equal(is_error_dataframe(
    get_rest_features(gyroscope_data = NA)), T)
})