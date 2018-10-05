####################################################
# File to test utils.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for map_groups
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

### Load data file
data("sensor_data")
dat <- sensor_data
data("tap_data")
datTap <- tap_data

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
context('Sampling rate')
test_that("Check Sampling rate calculation",{
  # actual function in utils: get_sampling_rate
  samplingRate = (length(datAccel$t))/(max(datAccel$t)-min(datAccel$t))
  expect_that(mhealthtools:::get_sampling_rate(dat), equals(samplingRate)) # Is the function returning expected numeric output for a valid input
  expect_that(mhealthtools:::get_sampling_rate(NA), equals(NA)) # Is the function giving NA for an invalid input
  
})

context('Tapping')
test_that('Function to extract left, right tapping events and intertap intervals',{
  # actual function in sensors.R: get_left_right_events_and_tap_intervals
  
  expect_is(mhealthtools:::get_left_right_events_and_tap_intervals(tapData = datTap), 'list') # Check if output is in correct format
  
  tempDat <- datTap[1:2,] # Only 2 rows, lesser than the required 5, Should expect an error
  expect_equal(mhealthtools:::get_left_right_events_and_tap_intervals(tapData = tempDat),
               list(tapData = NA, tapInter = NA, error = TRUE))
})

tapInter <- mhealthtools:::get_left_right_events_and_tap_intervals(datTap)$tapInter 
# Get inter tap intervals

test_that('Extract default inter tap time features',{
  # actual function in utils.R: intertap_summary_features
  
  expect_is(mhealthtools:::intertap_summary_features(tapInter = tapInter),'data.frame')  
})

test_that('Extract default tap drift features',{
  # actual function in utils.R: tapdrift_summary_features
  
  # tap drift is a numeric vector, like tapInter. So we are going to use tapInter to test the function 
  # rather than creating a seperate tapDrift numeric vector
  expect_is(mhealthtools:::tapdrift_summary_features(tapDrift = tapInter),'data.frame')
})

test_that('Extract deafult tap data features(features based on interaction between x and y, etc.)',{
  # actual function in utils.R: tap_data_summary_features
  
  expect_is(mhealthtools:::tap_data_summary_features(tapData = datTap),'data.frame')  
})

context('Tidy the data')
test_that("Tidying sensor data",{
  # actual function in utils: tidy_sensor_data
  expect_is(mhealthtools:::tidy_sensor_data(datAccel), 'data.frame') # Is the function returning expected data.frame output for a valid input
  
  tempDat <- data.table::copy(datAccel)
  tempDat$t[1] <- NA # time column now has a NA in it
  # Does the function throw an error when t values are missing
  expect_error(mhealthtools:::tidy_sensor_data(tempDat))
  
  tempDat$error <- rep(NA, length(tempDat$t))
  tempDat$error[1] <- 'some error' # Just a non NA value for error
  # This should make the tidy_sensor_data return the input, i.e, an identity function
  expect_that(mhealthtools:::tidy_sensor_data(tempDat), equals(tempDat))
  
  # this will set off warnings of not having columns error and t, so we are supressing warnings and then will re-start them
  options(warn = 0) # Turn off warnings
  tempDat <- tempDat %>% dplyr::select(x,y,z) # Removing the t and error columns, this should make the tidy_sensor_data throw an error
  testTibble <- dplyr::tibble(
    Window = NA,
    error = "Could not put sensor data in tidy format by gathering the axes.")
  expect_equal(mhealthtools:::tidy_sensor_data(tempDat), testTibble)
  options(warn = 1) # Turn warnings on again
  
})


## get tidy data as we will use that a lot to test the rest of the functions
datAccelTidy <- mhealthtools:::tidy_sensor_data(datAccel)
datGyroTidy <- mhealthtools:::tidy_sensor_data(datGyro)

context('Detrending of the data')

test_that("Detrend the data given a time series",{
  # actual function in utils: detrend
  expect_is(mhealthtools:::detrend(datAccel$t, datAccel$y), 'numeric') # Expected format
  
})

test_that("Detrend the given sensor data",{
  # actual function in utils: mutate_detrend
  expect_is(mhealthtools:::mutate_detrend(datAccelTidy),'data.frame') # Should give a dataframe if data is input in tidy format
  
  # Given wrong format of data, the function should throw an error
  testTibble <- dplyr::tibble(Window = NA, error = "Detrend error")
  expect_equal(mhealthtools:::mutate_detrend(datAccel), testTibble)
  
})

context('Bandpass and filtering by time ')

test_that('Bandpass a timeseries data',{
  # actual function in utils: bandpass
  testTimeSeries <- datAccel$x
  # This is 990 points long timeseries, sampled at 100Hz
  expect_is(mhealthtools:::bandpass(testTimeSeries, 120, 100,c(1,25)), 'numeric') # 120 sample window, 100Hz sampling rate, 1-25Hz freq range
  
  expect_error(mhealthtools:::bandpass(testTimeSeries, 120, 100, c(1, 100)), "Frequency parameters can be at most half the sampling rate.")
  # Frequency parameters violating Nyquist criterion
  
  # introduce an NA timeseries data, this should throw an error (Should be the same if atleast one point was NA)
  expect_error(mhealthtools:::bandpass(rep(NA,990), 120, 100, c(1,10))) # Input is NA, so expect an error
  
})

test_that('Bandpass the tidy sensor data', {
  # actual function in utils: mutate_bandpass
  testTibble <- dplyr::tibble(Window = NA, error = "Bandpass filter error")
  
  expect_is(mhealthtools:::mutate_bandpass(datAccelTidy, 120, 100, c(1,25)), 'data.frame') # Check if output is of the correct format
  expect_equal(mhealthtools:::mutate_bandpass(datAccel, 120, 100, c(1,25)), testTibble) # Error if input data is of wrong format
  expect_equal(mhealthtools:::mutate_bandpass(datAccelTidy, 120, 100, c(1,51)), testTibble) # Error if freq ranges of bandpass filter violate Nyquist criterion
  
})

test_that('Filtering the time series data by selecting a time range',{
  # actual function in utils: filter_time
  testTibble <- dplyr::tibble(Window = NA, error = "'Not enough time samples")
  
  expect_is(mhealthtools:::filter_time(datAccelTidy, 1,2), 'data.frame') # Check if output is in correct format
  expect_error(mhealthtools:::filter_time(datAccel[,"x"],1,2)) # throw an error if there is no t column
  # Maybe throw an error if t2(100s) of the window (t1,t2) is more than the actual time in the sensor data (0-10s)
  # Maybe throw an error if t1(11s) of the window (t1,t2) is more than the actual time in the sensor data (0-10s)
  
})

context('Windowing')

test_that('Windowing a time series',{
  # actual function in utils: windowSignal
  
  expect_is(mhealthtools:::windowSignal(datAccel$x),'matrix') # Check if output is in correct format
})

test_that('Windowing the sensor data by axis',{
  # actual function in utils: window
  testTibble <- dplyr::tibble(window = NA, error = "Windowing error")  
  
  expect_is(mhealthtools:::window(datAccelTidy, 256, 0.5),'data.frame') # 256 window length, 0.5 overlap, checking output format
  expect_error(mhealthtools:::window(datAccel,256, 0.5)) # throw an error if Input is not in correct format
  
})

context('Jerk Calculation')
test_that('Calculate Jerk given acceleration and sampling rate',{
  # actual function in utils: jerk  
  expect_is(mhealthtools:::jerk(datAccel$x,100), 'numeric') # Check if output is in correct format
  
})

test_that('Calculate and add Jerk column for the tidy sensor data',{
  # actual function in utils: mutate_jerk  
  testTibble <- dplyr::tibble(Window = NA, error = "Error calculating jerk")  
  
  expect_is(mhealthtools:::mutate_jerk(datAccelTidy,100),'data.frame') # Check if output is in correct format
  expect_equal(mhealthtools:::mutate_jerk(datAccel,100),testTibble) # Throw an error if input is not in correct format
  
})

context('Velocity Calculation')
test_that('Calculate velocity given acceleration and sampling rate',{
  # actual function in utils: velocity  
  expect_is(mhealthtools:::velocity(datAccel$x,100), 'numeric') # Check if output is in correct format
  
})

test_that('Calculate and add Velocity column for the tidy sensor data',{
  # actual function in utils: mutate_velocity 
  testTibble <- dplyr::tibble(Window = NA, error = "Error calculating velocity")  
  
  expect_is(mhealthtools:::mutate_velocity(datAccelTidy,100),'data.frame') # Check if output is in correct format
  expect_equal(mhealthtools:::mutate_velocity(datAccel,100),testTibble) # Throw an error if input is not in correct format
  
})

context('Displacement Calculation')
test_that('Calculate Displacement given acceleration and sampling rate',{
  # actual function in utils: displacement  
  expect_is(mhealthtools:::displacement(datAccel$x,100), 'numeric') # Check if output is in correct format
  
})

test_that('Calculate and add Displacement column for the tidy sensor data',{
  # actual function in utils: mutate_displacement  
  testTibble <- dplyr::tibble(Window = NA, error = "Error calculating displacement")  
  
  expect_is(mhealthtools:::mutate_displacement(datAccelTidy,100),'data.frame') # Check if output is in correct format
  expect_equal(mhealthtools:::mutate_displacement(datAccel,100),testTibble) # Throw an error if input is not in correct format
  
})

context('ACF calculation')
test_that('Construct a dataframe with ACF values given tidy sensor data',{
  # actual function in utils: calculate_acf  
  testTibble <- dplyr::tibble(Window = NA, error = "Error calculating ACF")
  
  expect_is(mhealthtools:::calculate_acf(datAccelTidy),'data.frame') # Check if output is in correct format
  expect_equal(mhealthtools:::calculate_acf(datAccel),testTibble) # Throw an error if input is not in correct format
  
})

context('Tag and Identify outlier windows based on device rotation(using gravity)')
test_that('Identify min, max gravity values for each window of tidy sensor data',{
  # actual function in utils: tag_outlier_windows_  
  
  gravityVec <- dat$gravity$x 
  expect_is(mhealthtools:::tag_outlier_windows_(gravityVec, 256, 0.5), 'data.frame')
  # Check if output is in correct format. 256 Window length and 0.5 overlap
  
  gravityVec <- c(rep(NA,255),1)
  testOutput <- mhealthtools:::tag_outlier_windows_(gravityVec, 256, 0.5) %>% as.data.frame()
  expect_equal(testOutput, data.frame(Window = as.character(1), max = NA, min = NA, stringsAsFactors=FALSE) )
  # This needs to have max and min as NA, but is not. The actual output is [Window = '1', max = 0.08, min =0.08]
  # All values are NA except for the last value, our window length is 256, same as the length of gravityVec
  # min(gravityVec) and max(gravityVec) should be NA
  
})

test_that('Identify windows in which Phone might have been flipped/rotated given gravity',{
  # actual function in utils: tag_outlier_windows
  
  expect_is(mhealthtools:::tag_outlier_windows(dat$gravity,256,0.5), 'data.frame') 
})

context('Features')
test_that('Time domain summary given acceleration',{
  # actual function in utils: time_domain_summary 
  accelVec <- datAccel$x[1:256] # A 256 (default window length) length acceleration vector  
  
  expect_is(mhealthtools:::time_domain_summary(accelVec,100),'data.frame') # Check if output is of correct format
  
})

test_that('Frequency domain summary given acceleration',{
  # actual function in utils: frequency_domain_summary 
  accelVec <- datAccel$x[1:256] # A 256 (default window length) length acceleration vector  
  
  expect_is(mhealthtools:::frequency_domain_summary(accelVec,100,3),'data.frame') # Check if output is of correct format
  
})

test_that('Frequency domain energy given acceleration',{
  # actual function in utils: frequency_domain_energy 
  accelVec <- datAccel$x[1:256] # A 256 (default window length) length acceleration vector  
  
  expect_is(mhealthtools:::frequency_domain_energy(accelVec,100),'data.frame') # Check if output is of correct format
  
})

context('Spectrum')
test_that('Get Spectrum given a time series and sampling rate',{
  # actual function in utils: getSpectrum 
  accelVec <- datAccel$x[1:512] # A 512 length acceleration vector  
  
  expect_is(mhealthtools:::getSpectrum(accelVec,sampling_rate = 100, nfreq = 500),'data.frame') # Check if output is in correct format
  # 100 Hz sampling rate and nfreq = 512
  
  expect_equal(dim(mhealthtools:::getSpectrum(accelVec, sampling_rate = 100, nfreq = 256))[1], 256)
  # If nfreq is 256 points I need a spectrum that has 256 points in it, not 500!
  
})

context('Empirical Wavelet Transform')
test_that('Get EWT spectrum',{
  # actual function in utils: getEWTspectrum 
  accelVec <- datAccel$x[1:512] # A 512 length acceleration vector  
  accelVecSpec <- mhealthtools:::getSpectrum(accelVec,sampling_rate = 100, nfreq = 500) # 500x2 spectrum
  
  expect_is(mhealthtools:::getEWTspectrum(accelVecSpec),'matrix') # Check if output is in correct format
  
})

context('Individual feature extraction functions')
test_that('fatigue',{
  # actual function in utils.R: fatigue 
  
  expect_is(mhealthtools:::fatigue(x = tapInter),'list')
})

test_that('Calculate Drift',{
  # actual function in utils.R: calculate_drift
  
  expect_is(mhealthtools:::calculate_drift(x = datTap$x,
                                           y = datTap$y),
            'numeric')
})

test_that('Mean Teager-Kaiser Energy (mtkeo)',{
  # actual function in utils.R: mean_tkeo 
  
  expect_is(mhealthtools:::mean_tkeo(tapInter), 'numeric')
})

test_that('Co-efficient of Variation (coef_var)',{
  # actual function in utils.R: coef_var
  
  expect_is(mhealthtools:::coef_var(tapInter), 'numeric')
})

