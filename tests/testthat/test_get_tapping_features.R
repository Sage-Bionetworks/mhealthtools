####################################################
# File to test get_tapping_features.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

### Require mHealthTools
require(mhealthtools)

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
data("tap_data")
dat <- tap_data

### Individual test functions
context('Extract tapping features')
test_that('Wrapper to extract tapping features',{
  # actual function in get_tapping_features.R: get_tapping_features
  
  
  expect_is(mhealthtools::get_tapping_features(tap_data = dat), 'data.frame') # Is output in the correct format
  
  tempDat <- 'not a dataframe' # The input is not a data frame, we should expect the relevant error
  testTibble <- dplyr::tibble(error = "expected data frame object") %>% 
    as.data.frame()
  expect_equal(mhealthtools::get_tapping_features(tap_data = tempDat), 
               testTibble)
  
  tempDat <- dat[1:2,] # tempDat now has just 2 rows of observations, we should expect the relevant error as this is less that the required 5
  testTibble <- dplyr::tibble(error = "raw tapping data has less than 5 rows") %>% 
    as.data.frame()
  expect_equal(mhealthtools::get_tapping_features(tap_data = tempDat), 
               testTibble)
  
  tempDat <- dat[1:2,]
  tempDat$buttonid[1:2] <- 'TappedButtonNone'
  tempDat <- rbind(tempDat, tempDat, tempDat, tempDat, tempDat)
  testTibble <- dplyr::tibble(error = "post duplication removal tapping data has less than 5 rows") %>% 
    as.data.frame()
  # tempDat now has 10 rows, but they are all duplicates of the first two (of the buttonid 'TappedButtonNone'),
  # so ideally speaking it just has two rows of non-duplicated unique data
  # so if we remove duplicates (we can only remove duplicates of buttonid 'TappedButtonNone'),
  # we should get an errow saying that the number of rows is less than 5
  expect_equal(mhealthtools::get_tapping_features(tap_data = tempDat, removeDups = TRUE), 
               testTibble)
  
})

context('Duplicate removal')
test_that('Remove TappedButtonNone duplicates',{
  # actual function in get_tapping_features.R: clean_tapped_button_none

  tempDat <- dat[1:2,] # 2 Rows
  
  expect_is(mhealthtools:::clean_tapped_button_none(tap_data = tempDat),'data.frame') # Check if output is in correct format
  
  tempDat$buttonid[1:2] <- 'TappedButtonNone' # Make the buttonid 'TappedButtonNone'
  tempDatDuplicated <- rbind(tempDat, tempDat, tempDat, tempDat, tempDat) # Duplicated dataframe with only 2 rows of actual data
  expect_equal(mhealthtools:::clean_tapped_button_none(tap_data = tempDatDuplicated),tempDat) # Check if duplicates are removed
  
})