####################################################
# File to test sensors.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for 
# (throws error) transform_kinematic_sensor_data
# (throws error) transform_gyroscope_data
# (suspect chek) transform_accelerometer_data
######################## *** NOTE *** ########################

### Require mHealthTools
# require(mhealthtools)

### Data file from a test user in Synapse
# Sample accelerometer data was taken from a control, test user with the recordId 5cf10e77-793f-49ab-ae96-38028aeefc28, from the table
# syn5734657, for his hand to nose left test - 'data/phone_data_test.json'

### Required Libraries
# library(testthat)
# library(jsonlite)
# library(dplyr)
# library(data.table)
# library(signal)
# library(seewave)
# library(stringr)
# library(purrr)

### Load data file
testthat::context('Load Required Data Files')
dat <- mhealthtools::sensor_data
datTap <- mhealthtools::tap_data

### flatten data to the format needed for mHealthTools
flatten_data <- function(dat, metric) {
  dat <- dat %>% 
    dplyr::select(timestamp, metric) %>% 
    jsonlite::flatten()
  names(dat) <- c("t", "x", "y", "z")
  return(tibble::as_tibble(dat))
}

### Get the formatted tapping, accelerometer and gyroscope data to use in testing below
datAccel <- flatten_data(dat,'userAcceleration')
datGyro  <- flatten_data(dat,'rotationRate')
datGravity <- flatten_data(dat, 'gravity')

## get tidy data as we will use that a lot to test the rest of the functions
datAccelTidy <- mhealthtools:::tidy_sensor_data(datAccel)
datGyroTidy <- mhealthtools:::tidy_sensor_data(datGyro)
datGravityTidy <- mhealthtools:::tidy_sensor_data(datGravity)


### Individual test functions
testthat::context('Accelerometer features')
testthat::test_that('Wrapper to extract accelerometer features',{
  # actual function in sensors.R: accelerometer_features
  
  testthat::expect_is(mhealthtools:::accelerometer_features(sensor_data = datAccelTidy), 'list') # Check if output is in correct format
  
})

testthat::test_that('Function to extract accelerometer features',{
  # actual function in sensors.R: accelerometer_features_
  
  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  
  testthat::expect_is(mhealthtools:::accelerometer_features_(sensor_data = datAccel,
                                                   transform = purrr::partial(
                                                     mhealthtools:::transform_accelerometer_data,
                                                     transformation = transformation,
                                                     window_length = 256,
                                                     time_range = c(1,9),
                                                     frequency_range = c(1,25),
                                                     sampling_rate = 100),
                                                   extract = funs),
            'list') # Check if output is in correct format
  
})

testthat::context('Gyroscope features')
testthat::test_that('Wrapper to extract gyroscope features',{
  # actual function in sensors.R: gyroscope_features
  
  testthat::expect_is(mhealthtools:::gyroscope_features(sensor_data = datGyro), 'list') # Check if output is in correct format
  
})

testthat::test_that('Function to extract gyroscope features',{
  # actual function in sensors.R: gyroscope_features_
  
  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  
  testthat::expect_is(mhealthtools:::gyroscope_features_(sensor_data = datAccel,
                                               transform = purrr::partial(
                                                 mhealthtools:::transform_accelerometer_data,
                                                 transformation = transformation,
                                                 window_length = 256,
                                                 time_range = c(1,9),
                                                 frequency_range = c(1,25),
                                                 sampling_rate = 100),
                                               extract = funs),
            'list') # Check if output is in correct format
  
})

testthat::context('Transformation functions')
testthat::test_that('Function to transform accelometer data with given input parameters',{
  # actual function in sensors.R: transform_accelerometer_data
  
  testthat::expect_is(mhealthtools:::transform_accelerometer_data(sensor_data = datAccel), 'data.frame') # Check if output is in correct format
})

testthat::test_that('Function to transform gyroscope data with given input parameters',{
  # actual function in sensors.R: transform_gyroscope_data
  
  testthat::expect_is(mhealthtools:::transform_gyroscope_data(sensor_data = datGyro), 'data.frame') # Check if output is in correct format
})

testthat::test_that('Function to transform kinematic sensor with given input parameters',{
  # actual function in sensors.R: transform_kinematic_sensor_data
  
  testthat::expect_is(mhealthtools:::transform_kinematic_sensor_data(sensor_data = datAccel,
                                                           transformation = NULL,
                                                           window_length = 256,
                                                           time_range = c(1,9),
                                                           frequency_range = c(1,25),
                                                           sampling_rate = 100),
            'data.frame') # Check if output is in correct format
})

testthat::test_that('Function to initialize windowing transformation function with input parameters',{
  # actual function in sensors.R: transformation_window
  
  testthat::expect_is(mhealthtools:::transformation_window(window_length = 256,
                                                 overlap = 0.5),
            'function') # Check if output is in correct format
})

testthat::test_that('Function to initialize IMF windowing transformation function with input parameters',{
  # actual function in sensors.R: transformation_imf_window
  
  testthat::expect_is(mhealthtools:::transformation_imf_window(window_length = 256, 
                                                     overlap = 0.5,
                                                     max_imf = 4),
            'function') # Check if output is in correct format
})

testthat::test_that('Function to initialize list of default kinematic feature extraction functions',{
  # actual function in sensors.R: default_kinematic_features
  
  testthat::expect_is(mhealthtools:::default_kinematic_features(sampling_rate = 100),
            'list') # Check if output is in correct format  
})

testthat::context('Processing sensor data')
testthat::test_that('Preprocess sensor data',{
  # actual function in sensors.R: preprocess_sensor_data
  
  testthat::expect_is(mhealthtools:::preprocess_sensor_data(sensor_data = datAccel,
                                                  window_length = 256,
                                                  sampling_rate = 100,
                                                  frequency_range = c(1,25),
                                                  time_range = c(1,9)),
            'data.frame') # Check if output is in correct format
})

testthat::context('Kinematic sensor features')
testthat::test_that('Extract kinematic sensor features',{
  # actual function in sensors.R: kinematic_sensor_features

  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  
  testthat::expect_is(mhealthtools:::kinematic_sensor_features(sensor_data = datAccel,
                                                     transform = purrr::partial(
                                                       mhealthtools:::transform_accelerometer_data,
                                                       transformation = transformation,
                                                       window_length = 256,
                                                       time_range = c(1,9),
                                                       frequency_range = c(1,25),
                                                       sampling_rate = 100),
                                                     extract = funs,
                                                     extract_on = c("acceleration", "jerk", 
                                                                    "velocity", "displacement"),
                                                     acf_col = "acceleration"),
            'list') # Check if output is in correct format
})


testthat::context('sensor features')
testthat::test_that('Extract sensor features',{
  # actual function in sensors.R: sensor_features
  
  transformation <- mhealthtools:::transformation_window(window_length = 256,
                                                         overlap = 0.5) # Required default transformation for the data
  funs <- mhealthtools:::default_kinematic_features(sampling_rate = 100) # Required default feature extraction functions
  transform = purrr::partial(mhealthtools:::transform_accelerometer_data,
                             transformation = transformation,
                             window_length = 256,
                             time_range = c(1,9),
                             frequency_range = c(1,25),
                             sampling_rate = 100)

  transformed_accelerometer_data <- transform(datAccel)
    
  testthat::expect_is(mhealthtools:::sensor_features(sensor_data = transformed_accelerometer_data,
                                           transform = function(x) x,
                                           extract = funs,
                                           extract_on = c("acceleration", "jerk", 
                                                          "velocity", "displacement")),
            'list') # Check if output is in correct format
})
