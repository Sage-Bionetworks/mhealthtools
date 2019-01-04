####################################################
# File to test get_tapping_features.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

### Load data file
testthat::context('Load Required Data Files')
dat <- tap_data

### Individual test functions
testthat::context('Extract tapping features')
testthat::test_that('Wrapper to extract tapping features',{
  # actual function in get_tapping_features.R: get_tapping_features
  testthat::expect_is(get_tapping_features(tap_data = dat),
                      'data.frame') # Is output in the correct format
  
  tempDat <- 'not a dataframe' 
  # The input is not a data frame, we should expect the relevant error
  testthat::expect_equal(is_error_dataframe(
    get_tapping_features(tap_data = tempDat)), T)
  
  # Checking depress_threshold parameter
  testthat::expect_is(get_tapping_features(tap_data = dat,
                                                         depress_threshold = 20),
                      'data.frame')
  # For an invalid depress_threshold parameter we should get an error, e.g
  # if it is not a number (of it is a negative number, no problem as we will
  # be doing x > depress_threshold, so..)
  testthat::expect_equal(is_error_dataframe(
    get_tapping_features(tap_data = dat, depress_threshold = NA)), T)
  
  tempDat <- dat[1:2,] 
  # tempDat now has just 2 rows of observations, we should expect the
  # relevant error as this is less that the required 5
  testthat::expect_equal(is_error_dataframe(
    get_tapping_features(tap_data = tempDat)), T)
  
  tempDat <- dat[1:2,]
  tempDat$buttonid[1:2] <- 'TappedButtonNone'
  tempDat <- rbind(tempDat, tempDat, tempDat, tempDat, tempDat)
  # tempDat now has 10 rows, but they are all duplicates of the first two 
  # (of the buttonid 'TappedButtonNone'),
  # so ideally speaking it just has two rows of non-duplicated unique data
  # so if we remove duplicates
  # (we can only remove duplicates of buttonid 'TappedButtonNone'),
  # we should get an errow saying that the number of rows is less than 5
  testthat::expect_equal(is_error_dataframe(
    get_tapping_features(
      tap_data = tempDat,
      remove_duplicates = TRUE)), T)
  
})

testthat::context('Duplicate removal')
testthat::test_that('Remove TappedButtonNone duplicates',{
  # actual function in get_tapping_features.R: clean_tapped_button_none

  tempDat <- dat[1:2,] # 2 Rows
  
  testthat::expect_is(clean_tapped_button_none(
    tap_data = tempDat),'data.frame') # Check if output is in correct format
  
  tempDat$buttonid[1:2] <- 'TappedButtonNone' 
  # Make the buttonid 'TappedButtonNone'
  tempDatDuplicated <- rbind(tempDat, tempDat, tempDat, tempDat, tempDat)
  # Duplicated dataframe with only 2 rows of actual data
  testthat::expect_equal(clean_tapped_button_none(
    tap_data = tempDatDuplicated),tempDat) # Check if duplicates are removed
  
})