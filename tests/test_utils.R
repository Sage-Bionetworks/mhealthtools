####################################################
# File to test utils.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

### Source file
source('utils.R')

### Data file from a test user in Synapse
# Sample accelerometer data was taken from a control, test user with the recordId 5cf10e77-793f-49ab-ae96-38028aeefc28, from the table
# syn5734657, for his hand to nose left test - 'phone_data_test.json'

### Required Libraries
library(testthat)
library(jsonlite)
library(dplyr)
library(data.table)
library(signal)
library(seewave)
library(stringr)

### Load data file
jsonFileLoc <- 'phone_data_test.json'
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

### Individual test functions

test_that("Check Sampling rate calculation",{
  # actual function in utils: get_sampling_rate
  samplingRate = (length(datAccel$t))/(max(datAccel$t)-min(datAccel$t))
  expect_that(get_sampling_rate(dat), equals(samplingRate)) # Is the function returning expected numeric output for a valid input
  expect_that(get_sampling_rate(NA), equals(NA)) # Is the function giving NA for an invalid input
  
})

context('Tidy the data')
test_that("Tidying sensor data",{
  # actual function in utils: tidy_sensor_data
  expect_is(tidy_sensor_data(datAccel), 'data.frame') # Is the function returning expected data.frame output for a valid input

  tempDat <- data.table::copy(datAccel)
  tempDat$t[1] <- NA # time column now has a NA in it
  expect_that(tidy_sensor_data(tempDat), equals(NA)) # Is the function giving NA/error for an invalid input

  tempDat$error <- rep(NA, length(tempDat$t))
  tempDat$error[1] <- 'some error' # Just a non NA value for error
  # This should make the tidy_sensor_data return the input, i.e, an identity function
  expect_that(tidy_sensor_data(tempDat), equals(tempDat))

  # this will set off warnings of not having columns error and t, so we are supressing warnings and then will re-start them
  options(warn = 0) # Turn off warnings
  tempDat <- tempDat %>% dplyr::select(x,y,z) # Removing the t and error columns, this should make the tidy_sensor_data throw an error
  testTibble <- dplyr::tibble(
    window = NA,
    error = "Could not put sensor data in tidy format by gathering the axes.")
  expect_equal(tidy_sensor_data(tempDat), testTibble)
  options(warn = 1) # Turn warnings on again
  
})


## get tidy data as we will use that a lot to test the rest of the functions
datAccelTidy <- tidy_sensor_data(datAccel)
datGyroTidy <- tidy_sensor_data(datGyro)

context('Detrending of the data')

test_that("Detrend the data given a time series",{
  # actual function in utils: detrend
  expect_is(detrend(datAccel$t, datAccel$y), 'numeric') # Expected format
  
})

test_that("Detrend the given sensor data",{
  # actual function in utils: mutate_detrend
  expect_is(mutate_detrend(datAccelTidy),'data.frame') # Should give a dataframe if data is input in tidy format

  # Given wrong format of data, the function should throw an error
  testTibble <- dplyr::tibble(window = NA, error = "Detrend error")
  expect_equal(mutate_detrend(datAccel), testTibble)
  
})

context('Bandpass and filtering by time ')

test_that('Bandpass a timeseries data',{
  # actual function in utils: bandpass
  testTimeSeries <- datAccel$x
  # This is 990 points long timeseries, sampled at 100Hz
  expect_is(bandpass(testTimeSeries, 120, 100,c(1,25)), 'numeric') # 120 sample window, 100Hz sampling rate, 1-25Hz freq range

  expect_error(bandpass(testTimeSeries, 120, 100, c(1, 100)), "Frequency parameters can be at most half the sampling rate.")
  # Frequency parameters violating Nyquist criterion

  # introduce an NA timeseries data, this should throw an error (Should be the same if atleast one point was NA)
  expect_error(bandpass(rep(NA,990), 120, 100, c(1,10)), "Corrupted Input") # Input is NA, so expect an error

})

test_that('Bandpass the tidy sensor data', {
  # actual function in utils: mutate_bandpass
  testTibble <- dplyr::tibble(window = NA, error = "Bandpass filter error")

  expect_is(mutate_bandpass(datAccelTidy, 120, 100, c(1,25)), 'data.frame') # Check if output is of the correct format
  expect_equal(mutate_bandpass(datAccel, 120, 100, c(1,25)), testTibble) # Error if input data is of wrong format
  expect_equal(mutate_bandpass(datAccelTidy, 120, 100, c(1,51)), testTibble) # Error if freq ranges of bandpass filter violate Nyquist criterion

})

test_that('Filtering the time series data by selecting a time range',{
  # actual function in utils: filter_time
  testTibble <- dplyr::tibble(window = NA, error = "'Not enough time samples")
  
  expect_is(filter_time(datAccelTidy, 1,2), 'data.frame') # Check if output is in correct format
  expect_equal(filter_time(datAccel,1,2), testTibble) # throw an error if input is not in tidy format
  expect_equal(filter_time(datAccelTidy,1,100), testTibble) # throw error if time out of range (test file is from 0-10s)
  # Maybe throw an error if t2(100s) of the window (t1,t2) is more than the actual time in the sensor data (0-10s)
  expect_equal(filter_time(datAccelTidy,11,20), testTibble) # throw error if time out of range (test file is from 0-10s)
  # Maybe throw an error if t1(11s) of the window (t1,t2) is more than the actual time in the sensor data (0-10s)
  # The error for the above two expectations can just be like 'selected time range(s) is invalid'
  
})

context('Windowing')

test_that('Windowing a time series',{
  # actual function in utils: windowSignal
  
  expect_is(windowSignal(datAccel$x),'matrix') # Check if output is in correct format
  expect_error(windowSignal(rep(NA,256)),"invalid input") 
  # If input has NAs maybe throw an error(?), output will have NAs
  # but better if we just give a warning / error message 
  
})

test_that('Windowing the sensor data by axis',{
  # actual function in utils: window
  testTibble <- dplyr::tibble(window = NA, error = "Windowing error")  
  
  expect_is(window(datAccelTidy, 256, 0.5),'data.frame') # 256 window length, 0.5 overlap, checking output format
  expect_equal(window(datAccel,256, 0.5), testTibble) # throw an error if Input is not in correct format
  
})

context('Jerk Calculation')
test_that('Calculate Jerk given acceleration and sampling rate',{
  # actual function in utils: jerk  
  expect_is(jerk(datAccel$x,100), 'numeric') # Check if output is in correct format

})

test_that('Calculate and add Jerk column for the tidy sensor data',{
  # actual function in utils: mutate_jerk  
  testTibble <- dplyr::tibble(window = NA, error = "Error calculating jerk")  
  
  expect_is(mutate_jerk(datAccelTidy,100),'data.frame') # Check if output is in correct format
  expect_equal(mutate_jerk(datAccel,100),testTibble) # Throw an error if input is not in correct format

})

context('Velocity Calculation')
test_that('Calculate velocity given acceleration and sampling rate',{
  # actual function in utils: velocity  
  expect_is(velocity(datAccel$x,100), 'numeric') # Check if output is in correct format
  
})

test_that('Calculate and add Velocity column for the tidy sensor data',{
  # actual function in utils: mutate_velocity 
  testTibble <- dplyr::tibble(window = NA, error = "Error calculating velocity")  
  
  expect_is(mutate_velocity(datAccelTidy,100),'data.frame') # Check if output is in correct format
  expect_equal(mutate_velocity(datAccel,100),testTibble) # Throw an error if input is not in correct format
  
})

context('Displacement Calculation')
test_that('Calculate Displacement given acceleration and sampling rate',{
  # actual function in utils: displacement  
  expect_is(displacement(datAccel$x,100), 'numeric') # Check if output is in correct format
  
})

test_that('Calculate and add Displacement column for the tidy sensor data',{
  # actual function in utils: mutate_displacement  
  testTibble <- dplyr::tibble(window = NA, error = "Error calculating displacement")  
  
  expect_is(mutate_displacement(datAccelTidy,100),'data.frame') # Check if output is in correct format
  expect_equal(mutate_displacement(datAccel,100),testTibble) # Throw an error if input is not in correct format
  
})

context('ACF calculation')
test_that('Construct a dataframe with ACF values given tidy sensor data',{
  # actual function in utils: calculate_acf  
  testTibble <- dplyr::tibble(window = NA, error = "Error calculating ACF")
  
  expect_is(calculate_acf(datAccelTidy),'data.frame') # Check if output is in correct format
  expect_equal(calculate_acf(datAccel),testTibble) # Throw an error if input is not in correct format
  
})

context('Tag and Identify outlier windows based on device rotation(using gravity)')
test_that('Identify min, max gravity values for each window of tidy sensor data',{
  # actual function in utils: tag_outlier_windows_  
  
  gravityVec <- dat$gravity$x 
  expect_is(tag_outlier_windows_(gravityVec, 256, 0.5), 'data.frame')
  # Check if output is in correct format. 256 Window length and 0.5 overlap
  
  gravityVec <- c(rep(NA,255),1) 
  # All values are NA except for the last value, our window length is 256, same as the length of gravityVec
  # min(gravityVec) and max(gravityVec) should be NA, so the testOutput needs to be a dataframe with one obs. for all
  # the three output vars with max and min having NAs
  
  testOutput <- tag_outlier_windows_(seq(256), 256, 0.5) # Initialize the data
  testOutput$max <- NA
  testOutput$min <- NA
  
  expect_equal(tag_outlier_windows_(gravityVec, 256, 0.5), testOutput) 
  # Min and Max should be NA if any window has NAs in it
})

test_that('Identify windows in which Phone might have been flipped/rotated given gravity',{
  # actual function in utils: tag_outlier_windows 
  testTibble <- dplyr::tibble(window = NA, error = "Error tagging outlier windows")  
  
  gravityVec <- dat$gravity$x
  
  # Given a gravity vector as the function requires, we should not get the error tibble(testTibble back)
  expect_false(all.equal(tag_outlier_windows(gravityVec,256,0.5),testTibble)) 
  # I think the error is because the input needs to be formatted before feeding it in, or if fed in a gravity vector, 
  # it needs to be handled inside (or especially before L 331 of utils, ie the first line after tryCatch in tag_outlier_windows)
  
  expect_equal(tag_outlier_windows(dat$gravity,256,0.5), testTibble) # Wrong input data format(not a vector, but a dataframe), expect an error tibble 
  
})

context('Features')
test_that('Time domain summary given acceleration',{
  # actual function in utils: time_domain_summary 
  accelVec <- datAccel$x[1:256] # A 256 (default window length) length acceleration vector  
  
  expect_is(time_domain_summary(accelVec,100),'data.frame') # Check if output is of correct format
  
})

test_that('Frequency domain summary given acceleration',{
  # actual function in utils: time_domain_summary 
  accelVec <- datAccel$x[1:256] # A 256 (default window length) length acceleration vector  
  
  expect_is(frequency_domain_summary(accelVec,100,3),'data.frame') # Check if output is of correct format
  
})

test_that('Frequency domain energy given acceleration',{
  # actual function in utils: time_domain_summary 
  accelVec <- datAccel$x[1:256] # A 256 (default window length) length acceleration vector  
  
  expect_is(frequency_domain_energy(accelVec,100),'data.frame') # Check if output is of correct format
  
})

######################## *** NOTE *** ########################
## Still have to write tests for getSpectrum, getEWTspectrum and map_groups
######################## *** NOTE *** ########################

