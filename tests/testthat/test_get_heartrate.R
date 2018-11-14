####################################################
# File to test get_heartrate.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for 
# (throws error, handle funs = NULL, models = NULL case) get_heartrate
######################## *** NOTE *** ########################

### Require mHealthTools
# require(mhealthtools)

### Data file from a test user in Synapse
# Sample camera data was taken from a control, test user with the 
# recordId ec82434e-0ca7-4425-b7bb-d87c37f063db,
# from the table syn11665074, from the column heartRate_before_recorder.json

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
testthat::context('Load Required data Files')
datHR <- mhealthtools::heartrate_data

### Individual test functions
testthat::context('Extract Heart rate')
testthat::test_that('Function to extract heart rate per channel(R,G,B)',{
  # actual function in get_heartrate: get_heartrate
  testthat::expect_equal(is_error_dataframe( 
    # Check output is in correct format
    mhealthtools:::get_heartrate(heartrate_data = datHR)), T)
  
  tempDat <- data.table::copy(datHR)
  tempDat$t <- rep(1, length(datHR$t))
  
  # Error if sampling rate cannot be calculated from timestamp
  testthat::expect_equal(is_error_dataframe( 
    mhealthtools:::get_heartrate(heartrate_data = tempDat)), T) 
  
  tempDat <- data.table::copy(datHR)
  tempDat <- tempDat %>%
    dplyr::rename('rex' = 'red') # Changed the column name of red to rex
  
  # Error if any channel red, green or blue is missing
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::get_heartrate(heartrate_data = tempDat)), T)
  
  tempDat <- data.table::copy(datHR)
  tempDat$red <- rep(NA, length(datHR$red))
})

testthat::test_that('Extract heart rate given a timeseries',{
  # actual function in get_heartRate: get_hr_from_time_series
  timeSeries <- datHR$red
  
  testthat::expect_is(mhealthtools:::get_hr_from_time_series(
    x = timeSeries, sampling_rate = 100), 'numeric') 
  # Check output is in correct format
  
  # NA's are handled as 0s, so even if the input has NA's (
  # not all of them we should get a numeric output)
  timeSeries[1:10] <- NA
  testthat::expect_is(mhealthtools:::get_hr_from_time_series(
    x = timeSeries, sampling_rate = 100), 'numeric') 
  # Check output is in correct format
  
  # If all the input is NA's then the output will be c(NA,NA)
  testthat::expect_equal(mhealthtools:::get_hr_from_time_series(
    x = rep(NA,1000), sampling_rate = 100), c(NA, NA)) 
  # Check output is in correct format
})

testthat::context('Filtering the raw avg pixel waveform')
testthat::test_that('Bandpass filter the input signal',{
  # actual function in get_heartRate: get_filtered_signal
  timeSeries <- datHR$red
  
  testthat::expect_is(mhealthtools:::get_filtered_signal(
    x = timeSeries, sampling_rate = 60), 'numeric') 
  # Check output is in correct format
})