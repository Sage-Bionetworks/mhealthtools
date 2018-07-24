####################################################
# File to test sensors.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

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
data('tap_data')
dat <- sensor_data

### flatten data to the format needed for mHealthTools
flatten_data <- function(dat, metric) {
  dat <- dat %>% 
    select(timestamp, metric) %>% 
    jsonlite::flatten()
  names(dat) <- c("t", "x", "y", "z")
  return(as_tibble(dat))
}

### Get the formatted tapping, accelerometer and gyroscope data to use in testing below
datTap <- tap_data
datAccel <- flatten_data(dat,'userAcceleration')
datGyro  <- flatten_data(dat,'rotationRate')
datGravity <- flatten_data(dat, 'gravity')

### Individual test functions
context('Accelerometer features')
test_that('Wrapper to extract accelerometer features',{
  # actual function in sensors.R: accelerometer_features
  
  expect_is(mhealthtools:::accelerometer_features(sensor_data = datAccel), 'data.frame') # Check if output is in correct format
  
})

test_that('Function to extract accelerometer features',{
  # actual function in sensors.R: accelerometer_features_
  
  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  
  expect_is(mhealthtools:::accelerometer_features_(sensor_data = datAccel,
                                                   transform = purrr::partial(
                                                     mhealthtools:::transform_accelerometer_data,
                                                     transformation = transformation,
                                                     window_length = 256,
                                                     overlap = 0.5,
                                                     time_range = c(1,9),
                                                     frequency_range = c(1,25),
                                                     sampling_rate = 100),
                                                   extract = funs,
                                                   groups = c('axis','Window')), 'data.frame') # Check if output is in correct format
  
})

context('Gyroscope features')
test_that('Wrapper to extract gyroscope features',{
  # actual function in sensors.R: gyroscope_features
  
  expect_is(mhealthtools:::gyroscope_features(sensor_data = datGyro), 'data.frame') # Check if output is in correct format
  
})

test_that('Function to extract gyroscope features',{
  # actual function in sensors.R: gyroscope_features_
  
  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  
  expect_is(mhealthtools:::gyroscope_features_(sensor_data = datAccel,
                                               transform = purrr::partial(
                                                 mhealthtools:::transform_accelerometer_data,
                                                 transformation = transformation,
                                                 window_length = 256,
                                                 overlap = 0.5,
                                                 time_range = c(1,9),
                                                 frequency_range = c(1,25),
                                                 sampling_rate = 100),
                                               extract = funs,
                                               groups = c('axis','Window')), 'data.frame') # Check if output is in correct format
  
})

context('Tapping features')
test_that('Function to extract tapping features',{
  # actual function in sensors.R: tapping_features
  
  expect_is(mhealthtools:::tapping_features(tap_data = datTap), 'data.frame') # Check if output is in correct format
})

context('Transformation functions')
test_that('Function to transform accelometer data with given input parameters',{
  # actual function in sensors.R: transform_accelerometer_data
  
  expect_is(mhealthtools:::transform_accelerometer_data(sensor_data = datAccel), 'data.frame') # Check if output is in correct format
})

test_that('Function to transform gyroscope data with given input parameters',{
  # actual function in sensors.R: transform_gyroscope_data
  
  expect_is(mhealthtools:::transform_gyroscope_data(sensor_data = datGyro), 'data.frame') # Check if output is in correct format
})

test_that('Function to transform kinematic sensor with given input parameters',{
  # actual function in sensors.R: transform_kinematic_sensor_data
  
  expect_is(mhealthtools:::transform_kinematic_sensor_data(sensor_data = datAccel,
                                                           transformation = NA,
                                                           window_length = 256,
                                                           overlap = 0.5,
                                                           time_range = c(1,9),
                                                           frequency_range = c(1,25),
                                                           sampling_rate = 100),
            'data.frame') # Check if output is in correct format
})

test_that('Function to initialize windowing transformation function with input parameters',{
  # actual function in sensors.R: transformation_window
  
  expect_is(mhealthtools:::transformation_window(window_length = 256,
                                                 overlap = 0.5),
            'function') # Check if output is in correct format
})

test_that('Function to initialize IMF windowing transformation function with input parameters',{
  # actual function in sensors.R: transformation_imf_window
  
  expect_is(mhealthtools:::transformation_imf_window(window_length = 256, 
                                                     overlap = 0.5,
                                                     max_imf = 4),
            'function') # Check if output is in correct format
})

test_that('Function to initialize list of default kinematic feature extraction functions',{
  # actual function in sensors.R: default_kinematic_features
  
  expect_is(mhealthtools:::default_kinematic_features(sampling_rate = 100,
                                                      npeaks = 4),
            'list') # Check if output is in correct format  
})

context('Processing sensor data')
test_that('Preprocess sensor data',{
  # actual function in sensors.R: preprocess_sensor_data
  
  expect_is(mhealthtools:::preprocess_sensor_data(sensor_data = datAccel,
                                                  window_length = 256,
                                                  sampling_rate = 100,
                                                  frequency_range = c(1,25),
                                                  time_range = c(1,9)),
            'data.frame') # Check if output is in correct format
})

context('Kinematic sensor features')
test_that('Extract kinematic sensor features',{
  # actual function in sensors.R: kinematic_sensor_features

  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  
  expect_is(mhealthtools:::kinematic_sensor_features(sensor_data = datAccel,
                                                     transform = purrr::partial(
                                                       mhealthtools:::transform_accelerometer_data,
                                                       transformation = transformation,
                                                       window_length = 256,
                                                       overlap = 0.5,
                                                       time_range = c(1,9),
                                                       frequency_range = c(1,25),
                                                       sampling_rate = 100),
                                                     extract = funs,
                                                     extract_on = c("acceleration", "jerk", 
                                                                    "velocity", "displacement"),
                                                     
                                                     groups = c('axis','Window'),
                                                     acf_col = "acceleration"),
            'data.frame') # Check if output is in correct format
})


context('sensor features')
test_that('Extract sensor features',{
  # actual function in sensors.R: sensor_features
  
  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  transform = purrr::partial(mhealthtools:::transform_accelerometer_data,
                             transformation = transformation,
                             window_length = 256,
                             overlap = 0.5,
                             time_range = c(1,9),
                             frequency_range = c(1,25),
                             sampling_rate = 100)

  transformed_accelerometer_data <- transform(datAccel)
    
  expect_is(mhealthtools:::sensor_features(sensor_data = transformed_accelerometer_data,
                                           transform = function(x) x,
                                           extract = funs,
                                           extract_on = c("acceleration", "jerk", 
                                                          "velocity", "displacement"),
                                           
                                           groups = c('axis','Window')),
            'data.frame') # Check if output is in correct format
})
