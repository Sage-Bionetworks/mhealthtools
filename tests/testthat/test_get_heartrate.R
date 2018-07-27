####################################################
# File to test get_heartrate.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

### Require mHealthTools
require(mhealthtools)

### Data file from a test user in Synapse
# Sample camera data was taken from a control, test user with the recordId ec82434e-0ca7-4425-b7bb-d87c37f063db,
# from the table syn11665074, from the column heartRate_before_recorder.json

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
data("heartrate_data")
datHR <- heartrate_data

### Individual test functions
context('Extract Heart rate')
test_that('Function to extract heart rate per channel(R,G,B)',{
  # actual function in get_heartrate: get_heartrate
  testTibble <- data.frame(red = NA, green = NA, blue = NA, 
                           error = NA,
                           samplingRate = NA)
  testTibble$error = 'Sampling Rate calculated from timestamp is Inf or NaN / timestamp not found in json'
  
  expect_is(mhealthtools:::get_heartrate(dat = datHR), 'list') # Check if output is in correct format
  
  tempDat <- copy(datHR)
  tempDat <- tempDat %>% dplyr::rename('t' = 'timestamp') # Changed the column name of timestamp to t
  expect_equal(mhealthtools:::get_heartrate(dat = tempDat), testTibble) # Error if timestamp column is missing
  
  tempDat <- copy(datHR)
  tempDat$timestamp <- rep(1, length(datHR$timestamp))
  expect_equal(mhealthtools:::get_heartrate(dat = tempDat), testTibble) # Error if sampling rate cannot be calculated from timestamp
  
  tempDat <- copy(datHR)
  tempDat <- tempDat %>% dplyr::rename('rex' = 'red') # Changed the column name of red to rex
  testTibble$error = 'red, green, blue cannot be read from JSON'
  expect_equal(mhealthtools:::get_heartrate(dat = tempDat), testTibble) # Error if any channel red, green or blue is missing
  
  tempDat <- copy(datHR)
  tempDat$red <- rep(NA, length(datHR$red))
})

test_that('Extract heart rate given a timeseries',{
  # actual function in get_heartRate: getHrFromTimeSeries
  timeSeries <- datHR$red
  
  expect_is(mhealthtools:::getHrFromTimeSeries(x = timeSeries,samplingRate = 100), 'numeric') # Check if output is in correct format
  
  # NA's are handled as 0s, so even if the input has NA's (not all of them we should get a numeric output)
  timeSeries[1:10] <- NA
  expect_is(mhealthtools:::getHrFromTimeSeries(x = timeSeries,samplingRate = 100), 'numeric') # Check if output is in correct format

  # If all the input is NA's then the output will be c(NA,NA)
  expect_equal(mhealthtools:::getHrFromTimeSeries(x = rep(NA,1000),samplingRate = 100), c(NA,NA)) # Check if output is in correct format
  
})

context('Filtering the raw avg pixel waveform')
test_that('Bandpass filter the input signal',{
  # actual function in get_heartRate: getfilteredsignal
  timeSeries <- datHR$red
  
  expect_is(mhealthtools:::getfilteredsignal(x = timeSeries,samplingRate = 60), 'numeric') # Check if output is in correct format
  
})