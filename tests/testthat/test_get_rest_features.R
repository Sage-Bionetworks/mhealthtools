####################################################
# File to test get_rest_features.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

# When I input gravity sensor data into the function get_rest_features, the whole error column is like
# 'Phone rotated within window' for all the windows. Is this normal, or is this happening because of the test data (I don't think so)
# This needs to be checked.
# Go to Line 55 to see code to emulate this situation

### Require mHealthTools
require(mhealthtools)

### Data file from a test user in Synapse
# Sample accelerometer data was taken from a control, test user with the recordId 5cf10e77-793f-49ab-ae96-38028aeefc28, from the table
# syn5734657, for his hand to nose left test - 'data/phone_data_test.json'

### Required Libraries
library(testthat)
library(jsonlite)
library(dplyr)
library(data.table)
library(signal)
library(seewave)
library(stringr)
library(purrr)

### Load data file
data("sensor_data")
dat <- sensor_data

### flatten data to the format needed for mHealthTools
flatten_data <- function(dat, metric) {
  dat <- dat %>% 
    select(timestamp, metric) %>% 
    jsonlite::flatten()
  names(dat) <- c("t", "x", "y", "z")
  return(as_tibble(dat))
}

### Get the formatted accelerometer and gyroscope data to use in testing below
datAccel <- flatten_data(dat,'userAcceleration')
datGyro  <- flatten_data(dat,'rotationRate')
datGravity <- flatten_data(dat, 'gravity')

### Individual test functions
context('Get Rest Features')
test_that('Get accelerometer, gyroscope features',{
  # actual function in get_rest_features.R: get_rest_features
  testTibble <- dplyr::tibble(Window = NA, error = NA)
  
  expect_is(mhealthtools::get_rest_features(accelerometer_data = datAccel, gyroscope_data = datGyro), 'data.frame') 
  # Give both Accelerometer and Gyroscope data and expect a dataframe, with rest of the inputs being default
  expect_is(mhealthtools::get_rest_features(accelerometer_data = datAccel, gyroscope_data = datGyro, gravity_data = datGravity), 'data.frame') 
  # Similar test to previous one except also included gravity data
  
  testTibble$error <- 'Malformed accelerometer data'
  expect_equal(mhealthtools:::get_rest_features(accelerometer_data = NA, gyroscope_data = datGyro), testTibble)
  # Give error tibble if accelerometer data has any NAs
  
  testTibble$error <- 'Malformed gyroscope data'
  expect_equal(mhealthtools:::get_rest_features(accelerometer_data = datAccel, gyroscope_data = NA), testTibble)
  # Give error tibble if gyroscope data has any NAs  
  
  # The processing errors for acceleromter_features and gyroscope_features have been handled in test_sensors.R
  # tag_outlier_windows was also handled in test_utils.R
  
})
