####################################################
# File to test getTremorFeatures.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

# When I input gravity sensor data into the function getTremorFeatures, the whole error column is like
# 'Phone rotated within window' for all the windows. Is this normal, or is this happening because of the test data (I don't think so)
# This needs to be checked.
# Go to Line 47 to see code to emulate this situation

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
jsonFileLoc <- '../data/phone_data_test.json'
dat = jsonlite::fromJSON(as.character(jsonFileLoc)) %>% as.data.frame()

### JSON reader to read the file from the mPower Study format to the format needed for mHealthTools
json_reader <- function(path, metric) {
  dat <- jsonlite::fromJSON(path) %>% 
    select(timestamp, metric) %>% 
    jsonlite::flatten()
  names(dat) <- c("t", "x", "y", "z")
  return(as_tibble(dat))
}

### Get the formatted accelerometer and gyroscope data to use in testing below
datAccel <- json_reader(jsonFileLoc,'userAcceleration')
datGyro  <- json_reader(jsonFileLoc,'rotationRate')
datGravity <- json_reader(jsonFileLoc, 'gravity')

# a <- mhealthtools:::get_tremor_features(datAccel, datGyro, gravity_data = datGravity)

### Individual test functions
context('Get Tremor Features')
test_that('Get accelerometer, gyroscope features',{
  # actual function in getTremorFeatures: get_tremor_featrues
  testTibble <- dplyr::tibble(Window = NA, error = NA)
  
  expect_is(mhealthtools::get_tremor_features(datAccel, datGyro), 'data.frame') 
  # Give both Accelerometer and Gyroscope data and expect a dataframe, with rest of the inputs being default
  expect_is(mhealthtools::get_tremor_features(datAccel, datGyro, gravity_data = datGravity), 'data.frame') 
  # Similar test to previous one except also included gravity data
  
  testTibble$error <- 'Malformed accelerometer data'
  expect_equal(mhealthtools:::get_tremor_features(NA, datGyro), testTibble)
  # Give error tibble if accelerometer data has any NAs
  
  testTibble$error <- 'Malformed gyroscope data'
  expect_equal(mhealthtools:::get_tremor_features(datAccel, NA), testTibble)
  # Give error tibble if gyroscope data has any NAs  
  
  # The processing errors for acceleromter_features and gyroscope_features have been handled in test_sensors.R
  # tag_outlier_windows was also handled in test_utils.R
  
})
