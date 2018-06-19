####################################################
# File to test getHR.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for getfilteredsignal, getHrFromTimeSeries
######################## *** NOTE *** ########################

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
jsonFileLoc <- 'data/hr_test.json'
datHR = jsonlite::fromJSON(as.character(jsonFileLoc)) %>% as.data.frame()

### Individual test functions
context('Extract Heart rate')
test_that('Function to extract heart rate per channel(R,G,B)',{
  # actual function in getHR: getHR
  testTibble <- data.frame(red = NA, green = NA, blue = NA, 
                           error = NA,
                           samplingRate = NA)
  testTibble$error = 'Sampling Rate calculated from timestamp is Inf or NaN / timestamp not found in json'
  
  expect_is(mhealthtools:::getHR(datHR), 'list') # Check if output is in correct format
  
  tempDat <- copy(datHR)
  tempDat <- tempDat %>% dplyr::rename('t' = 'timestamp') # Changed the column name of timestamp to t
  expect_equal(mhealthtools:::getHR(tempDat), testTibble) # Error if timestamp column is missing
  
  tempDat <- copy(datHR)
  tempDat$timestamp <- rep(1, length(datHR$timestamp))
  expect_equal(mhealthtools:::getHR(tempDat), testTibble) # Error if sampling rate cannot be calculated from timestamp
  
})
