####################################################
# File to test sensors.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for extract_features (started, commented code at bottom)
######################## *** NOTE *** ########################

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
jsonFileLoc <- 'data/phone_data_test.json'
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

### Individual test functions
context('Extract Accelerometer features')
test_that('Function to extract accelerometer features',{
  # actual function in sensors: accelerometer_features
  testTibble <- dplyr::tibble(window = "NA", error = "Could not calculate sampling rate.")
    
  expect_is(mhealthtools:::accelerometer_features(datAccel), 'data.frame') # Check if output is in correct format
  
  tempData <- data.table::copy(datAccel)
  tempData$t[1:2] <- NA
  expect_equal(mhealthtools:::accelerometer_features(tempData), testTibble) # Check for error tibble for sampling rate issue
})

context('Extract Gyroscope features')
test_that('Function to extract gyroscope features',{
  # actual function in sensors: gyroscope_features
  testTibble <- dplyr::tibble(window = "NA", error = "Could not calculate sampling rate.")
  
  expect_is(mhealthtools:::gyroscope_features(datGyro), 'data.frame') # Check if output is in correct format
  
  tempData <- data.table::copy(datGyro)
  tempData$t[1:2] <- NA
  expect_equal(mhealthtools:::gyroscope_features(tempData), testTibble) # Check for error tibble for sampling rate issue
})

# context('Function Mapping onto a column')
# test_that('Extract features for a column from a list of given functions',{
#   # actual function in sensors: extract_features
#    
#   funs <- list(
#     time_domain_summary = function(sensor_data) {
#       sampling_rate <- mhealthtools:::get_sampling_rate(sensor_data)
#       return(function(accel) {
#         mhealthtools:::time_domain_summary(accel, sampling_rate = sampling_rate)
#       })
#     },
#     frequency_domain_summary = function(sensor_data) {
#       sampling_rate <- mhealthtools:::get_sampling_rate(sensor_data)
#       return(function(accel) {
#         mhealthtools:::frequency_domain_summary(accel, sampling_rate = sampling_rate)
#       })
#     },
#     frequency_domain_energy = function(sensor_data) {
#       sampling_rate <- mhealthtools:::get_sampling_rate(sensor_data)
#       return(function(accel) {
#         mhealthtools:::frequency_domain_energy(accel, sampling_rate = sampling_rate)
#       })
#     }
#   )
#   sensor_data <- data.table::copy(datAccel)
#   
#   # Preprocess data to be fed into extract_features
#   sampling_rate <- mhealthtools:::get_sampling_rate(sensor_data)
#   window_length <- 256
#   frequency_range <- c(1,25)
#   time_range = c(1,9)
#   overlap = 0.5
#   # preprocess and calculate jerk, velocity, displacement
#   sensor_data <- sensor_data %>%
#     mhealthtools:::tidy_sensor_data() %>% 
#     mhealthtools:::mutate_detrend() %>%
#     mhealthtools:::mutate_bandpass(window_length, sampling_rate, frequency_range) %>%
#     mhealthtools:::filter_time(time_range[1], time_range[2]) %>%
#     mhealthtools:::window(window_length, overlap) %>%
#     mhealthtools:::mutate_jerk(sampling_rate) %>%
#     mhealthtools:::mutate_velocity(sampling_rate) %>%
#     mhealthtools:::mutate_displacement(sampling_rate)
#      
#  })
