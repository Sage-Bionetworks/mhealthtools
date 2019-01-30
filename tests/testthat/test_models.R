context("Test models")

test_that('Generate GuanLab architechture',{
  expect_is(guanlab_nn_architecture(), 'keras.engine.sequential.Sequential')
})

MODELS <- NULL

test_that('Load weights into GuanLab architechture',{
  MODELS <- load_guanlab_model()
  expect_is(MODELS, "list")
  # output is a list of Keras sequential models
})

test_that("Extract features using GuanLab model", {
  expect_is(guanlab_model(
    sensor_data = mini_accelerometer_data, models = MODELS), "data.frame")
})