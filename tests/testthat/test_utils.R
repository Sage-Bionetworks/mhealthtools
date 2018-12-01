####################################################
# File to test utils.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for 
# map_groups [~PHIL]
# extract_features [~PHIL]
# (throws error) tag_outlier_windows_ 
# [The error is valid, reaffirming my suspicion that NA's
# are handled differently in the underlying signal package]
# (throws error for NA window length) window_start_end_times 
######################## *** NOTE *** ########################

### Require mHealthTools
# require(mhealthtools)

### Data file from a test user in Synapse
# Sample accelerometer data was taken from a control, test user with 
# the recordId 5cf10e77-793f-49ab-ae96-38028aeefc28, from the table
# syn5734657, for his hand to nose left test - 'data/phone_data_test.json'

### Required Libraries
# library(testthat)
# library(jsonlite)
# library(dplyr)
# library(data.table)
# library(signal)
# library(seewave)
# library(stringr)

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

### Get the formatted accelerometer and gyroscope data to use in testing below
datAccel <- flatten_data(dat,'userAcceleration')
datGyro  <- flatten_data(dat,'rotationRate')
datGravity <- flatten_data(dat, 'gravity')

### Individual test functions
testthat::context('Sampling rate')
testthat::test_that("Check Sampling rate calculation",{
  # actual function in utils: get_sampling_rate
  samplingRate = (length(datAccel$t))/(max(datAccel$t)-min(datAccel$t))
  testthat::expect_equal(get_sampling_rate(sensor_data = dat), samplingRate) 
  # Is the function returning expected numeric output for a valid input
  testthat::expect_equal(get_sampling_rate(sensor_data = NA), NA)
})

testthat::context('Tapping')
testthat::test_that(
  'Function to extract left, right tapping events and intertap intervals',{
    # actual function in sensors.R: get_left_right_events_and_tap_intervals
    
    testthat::expect_is(
      mhealthtools:::get_left_right_events_and_tap_intervals(tap_data = datTap),
      'list') # Check if output is in correct format
    
    tempDat <- datTap[1:2,] 
    # Only 2 rows, lesser than the required 5, Should expect an error
    testthat::expect_equal(
      mhealthtools:::get_left_right_events_and_tap_intervals(tap_data = tempDat),
      list(tap_data = NA, tap_intervals = NA, error = TRUE))
  })

tapInter <- mhealthtools:::get_left_right_events_and_tap_intervals(
  tap_data = datTap)$tapInter 
# Get inter tap intervals

testthat::test_that('Extract default inter tap time features',{
  # actual function in utils.R: intertap_summary_features
  
  testthat::expect_is(
    mhealthtools:::intertap_summary_features(tap_intervals = tapInter),'data.frame')  
})

testthat::test_that('Extract default tap drift features',{
  # actual function in utils.R: tapdrift_summary_features
  
  # tap drift is a numeric vector, like tapInter. So we are going to
  # use tapInter to test the function rather than creating a seperate 
  # tapDrift numeric vector
  testthat::expect_is(
    mhealthtools:::tapdrift_summary_features(tap_drift = tapInter),'data.frame')
})

testthat::test_that(
  'Extract deafult tap data features (features based on interaction 
  between x and y, etc.)',{
    # actual function in utils.R: tap_data_summary_features
    
    testthat::expect_is(
      mhealthtools:::tap_data_summary_features(tap_data = datTap), 'data.frame')  
  })

testthat::context('Tidy the data')
testthat::test_that("Tidying sensor data",{
  # actual function in utils: tidy_sensor_data
  testthat::expect_is(
    mhealthtools:::tidy_sensor_data(sensor_data = datAccel), 'data.frame') 
  # Is the function returning expected data.frame output for a valid input
  
  tempDat <- data.table::copy(datAccel)
  tempDat$t[1] <- NA # time column now has a NA in it
  # Does the function throw an error when t values are missing
  testthat::expect_error(
    mhealthtools:::tidy_sensor_data(sensor_data = tempDat))
  
  tempDat$error <- rep(NA, length(tempDat$t))
  tempDat$error[1] <- 'some error' # Just a non NA value for error
  # tidy_sensor_data should return the input, i.e, an identity function
  testthat::expect_equal(tidy_sensor_data(sensor_data = tempDat), tempDat)
  
  # this will set off warnings of not having columns error and t,
  # so we are supressing warnings and then will re-start them
  options(warn = 0) # Turn off warnings
  tempDat <- tempDat %>% dplyr::select(x,y,z) 
  # Removing the t and error columns, tidy_sensor_data throw an error
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::tidy_sensor_data(sensor_data = tempDat)), T)
  options(warn = 1) # Turn warnings on again
  
})


## get tidy data as we will use that a lot to test the rest of the functions
datAccelTidy <- mhealthtools:::tidy_sensor_data(sensor_data = datAccel)
datGyroTidy <- mhealthtools:::tidy_sensor_data(sensor_data = datGyro)

testthat::context('Detrending of the data')
testthat::test_that("Detrend the data given a time series",{
  # actual function in utils: detrend
  testthat::expect_is(
    mhealthtools:::detrend(time = datAccel$t,values =  datAccel$y), 'numeric') 
  # Expected format
  
})

testthat::test_that("Detrend the given sensor data",{
  # actual function in utils: mutate_detrend
  testthat::expect_is(
    mhealthtools:::mutate_detrend(sensor_data = datAccelTidy),'data.frame') 
  # Should give a dataframe if data is input in tidy format
  
  # Given wrong format of data, the function should throw an error
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::mutate_detrend(sensor_data = datAccel)), T)
  
})

testthat::context('Bandpass and filtering by time ')
testthat::test_that('Bandpass a timeseries data',{
  # actual function in utils: bandpass
  testTimeSeries <- datAccel$x
  # This is 990 points long timeseries, sampled at 100Hz
  testthat::expect_is(
    mhealthtools:::bandpass(values = testTimeSeries,
                            window_length = 120,
                            sampling_rate =  100,
                            frequency_range = c(1,25)), 'numeric') 
  # 120 sample window, 100Hz sampling rate, 1-25Hz freq range
  
  testthat::expect_error(
    mhealthtools:::bandpass(values = testTimeSeries,
                            window_length =  120,
                            sampling_rate =  100,
                            frequency_range =  c(1, 100)),
    "Frequency parameters can be at most half the sampling rate.")
  # Frequency parameters violating Nyquist criterion
  
  # introduce an NA timeseries data, this should throw an error 
  # (Should be the same if atleast one point was NA)
  testthat::expect_error(
    mhealthtools:::bandpass(values = rep(NA,990),
                            window_length =  120,
                            sampling_rate = 100,
                            frequency_range =  c(1,10))) 
  # Input is NA, so expect an error
})

testthat::test_that('Bandpass the tidy sensor data', {
  # actual function in utils: mutate_bandpass
  
  testthat::expect_is(
    mhealthtools:::mutate_bandpass(sensor_data = datAccelTidy,
                                   window_length =  120,
                                   sampling_rate =  100,
                                   frequency_range =  c(1,25)), 'data.frame') 
  # Check if output is of the correct format
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::mutate_bandpass(
      sensor_data = datAccel,
      window_length = 120,
      sampling_rate = 100,
      frequency_range = c(1,25))), T) 
  # Error if input data is of wrong format
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::mutate_bandpass(
      sensor_data = datAccelTidy,
      window_length = 120,
      sampling_rate = 100,
      frequency_range = c(1,51))), T) 
  # Error if freq ranges of bandpass filter violate Nyquist criterion
  
})

testthat::test_that(
  'Filtering the time series data by selecting a time range',{
    # actual function in utils: filter_time
    testthat::expect_is(mhealthtools:::filter_time(sensor_data = datAccelTidy,
                                                   t1 = 1,
                                                   t2 = 2),
                        'data.frame') 
    # Check if output is in correct format
    testthat::expect_error(
      mhealthtools:::filter_time(sensor_data = datAccel[,"x"],
                                 t1 = 1,
                                 t2 = 2)) 
    # throw an error if there is no t column
    # Maybe throw an error if t2(100s) of the window (t1,t2) 
    # is more than the actual time in the sensor data (0-10s)
    # Maybe throw an error if t1(11s) of the window (t1,t2) 
    # is more than the actual time in the sensor data (0-10s)
    
  })

testthat::context('Windowing')
testthat::test_that('Windowing a time series',{
  # actual function in utils: window_signal
  
  testthat::expect_is(mhealthtools:::window_signal(values = datAccel$x), 'matrix')
  # Check if output is in correct format
})

testthat::test_that('Windowing the sensor data by axis',{
  # actual function in utils: window
  testthat::expect_is(mhealthtools:::window(sensor_data = datAccelTidy,
                                            window_length =  256,
                                            window_overlap =  0.5),
                      'data.frame') 
  # 256 window length, 0.5 overlap, checking output format
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::window(
      sensor_data = datAccel,
      window_length = 256,
      window_overlap = 0.5)), T)
  # throw an error if Input is not in correct format
})

testthat::test_that('Compute start and stop timestamp for each window',{
  # actual function in utils: window_start_end_times
  testthat::expect_is(
    mhealthtools:::window_start_end_times(t = datAccel$t,
                                          window_length = 10,
                                          window_overlap = 0.5), 'data.frame')
  # Check output format
  
  testthat::expect_error(mhealthtools:::window_start_end_times(
    t = datAccel$t, 
    window_length = NA,
    window_overlap = 0.5))
  # Throw a data error if any parameter doesn't confirm to norms
})

testthat::context('Derivative Calculation')
testthat::test_that(
  'Calculate Derivative given acceleration and sampling rate',{
    # actual function in utils: derivative
    
    testthat::expect_is(mhealthtools:::derivative(v = datAccel$x), 'numeric') 
    # Check if output is in correct format
    
  })

testthat::test_that(
  'Calculate and add a derivative column for the sensor data',{
    # actual function in utils: mutate_derivative  
    
    # mutate_derivative and mutate_integral functions are a stand in 
    # for mutate_*(jerk, velocity, displacement) 
    testthat::expect_is(
      mhealthtools:::mutate_derivative(sensor_data = datAccel,
                                       sampling_rate = 100,
                                       col = 'x',
                                       derived_col = 'dx'),'data.frame') 
    # Check output format
    
    testthat::expect_equal(is_error_dataframe(
      mhealthtools:::mutate_derivative(
        sensor_data = datAccelTidy,
        sampling_rate = 100,
        col = 'x',
        derived_col = 'dx')), T) 
    # Throw an error if input is not in correct format
    
  })

testthat::context('Integral Calculation')
testthat::test_that(
  'Calculate Integral given acceleration and sampling rate',{
    # actual function in utils: integral
    
    testthat::expect_is(mhealthtools:::integral(v = datAccel$x), 'numeric') 
    # Check if output is in correct format
    
  })

testthat::test_that(
  'Calculate and add a integral column for the sensor data',{
    # actual function in utils: mutate_integral
    
    # mutate_derivative and mutate_integral functions are a stand in 
    # for mutate_*(jerk, velocity, displacement) 
    testthat::expect_is(
      mhealthtools:::mutate_integral(sensor_data = datAccel,
                                     sampling_rate = 100,
                                     col = 'x',
                                     derived_col = 'dx'),'data.frame') 
    # Check output format
    
    testthat::expect_equal(is_error_dataframe( 
      # Throw an error if input is not in correct format
      mhealthtools:::mutate_integral(
        sensor_data = datAccelTidy,
        sampling_rate = 100,
        col = 'x',
        derived_col = 'dx')), T) 
  })

testthat::context('ACF calculation')
testthat::test_that(
  'Construct a dataframe with ACF values given tidy sensor data',{
    # actual function in utils: calculate_acf  
    testthat::expect_is(
      mhealthtools:::calculate_acf(sensor_data = datAccelTidy)
      ,'data.frame') 
    # Check if output is in correct format
    testthat::expect_equal(is_error_dataframe( 
      # Throw an error if input is not in correct format
      mhealthtools:::calculate_acf(
        sensor_data = datAccel)), T) 
  })

testthat::context('Tag and Identify outlier windows based on device rotation
                  (using gravity)')
testthat::test_that(
  'Identify min, max gravity values for each window of tidy sensor data',{
    # actual function in utils: tag_outlier_windows_  
    
    gravityVec <- dat$gravity$x 
    testthat::expect_is(
      mhealthtools:::tag_outlier_windows_(gravity_vector = gravityVec,
                                          window_length = 256,
                                          window_overlap =  0.5),
      'data.frame')
    # Check if output is in correct format. 256 Window length and 0.5 window_overlap
    
    gravityVec <- c(rep(NA,255),1)
    testOutput <- mhealthtools:::tag_outlier_windows_(
      gravity_vector = gravityVec,
      window_length = 256,
      window_overlap =  0.5) %>%
      as.data.frame()
    
    testthat::expect_equal(
      testOutput, data.frame(window = as.character(1),
                             max = NA, min = NA, stringsAsFactors=FALSE) )
    # This needs to have max and min as NA, but is not. 
    # The actual output is [window = '1', max = 0.08, min =0.08]
    # All values are NA except for the last value, our window length is 256,
    # same as the length of gravityVec
    # min(gravityVec) and max(gravityVec) should be NA
    
  })

testthat::test_that(
  'Identify windows in which Phone might have been flipped/rotated given gravity',{
    # actual function in utils: tag_outlier_windows
    
    testthat::expect_is(
      mhealthtools:::tag_outlier_windows(gravity = dat$gravity,
                                         window_length = 256,
                                         window_overlap = 0.5),
      'data.frame') 
  })

testthat::context('Features')
testthat::test_that('Time domain summary given acceleration',{
  # actual function in utils: time_domain_summary 
  accelVec <- datAccel$x[1:256] 
  # A 256 (default window length) length acceleration vector  
  
  testthat::expect_is(
    mhealthtools:::time_domain_summary(values = accelVec,
                                       sampling_rate = 100),
    'data.frame') # Check if output is of correct format
  
})

testthat::test_that('Frequency domain summary given acceleration',{
  # actual function in utils: frequency_domain_summary 
  accelVec <- datAccel$x[1:256] 
  # A 256 (default window length) length acceleration vector  
  
  testthat::expect_is(
    mhealthtools:::frequency_domain_summary(values = accelVec,
                                            sampling_rate = 100,
                                            npeaks = 3),
    'data.frame') # Check if output is of correct format
  
})

testthat::test_that('Frequency domain energy given acceleration',{
  # actual function in utils: frequency_domain_energy 
  accelVec <- datAccel$x[1:256] 
  # A 256 (default window length) length acceleration vector  
  
  testthat::expect_is(
    mhealthtools:::frequency_domain_energy(values = accelVec,
                                           sampling_rate = 100),
    'data.frame') # Check if output is of correct format
  
})

testthat::context('Spectrum')
testthat::test_that('Get Spectrum given a time series and sampling rate',{
  # actual function in utils: get_spectrum 
  accelVec <- datAccel$x[1:512] # A 512 length acceleration vector  
  
  testthat::expect_is(mhealthtools:::get_spectrum(values = accelVec,
                                                 sampling_rate = 100,
                                                 nfreq = 500),'data.frame') 
  # Check if output is in correct format
  # 100 Hz sampling rate and nfreq = 512
  
  testthat::expect_equal(
    dim(mhealthtools:::get_spectrum(
      accelVec, sampling_rate = 100, nfreq = 256))[1], 256)
  # If nfreq is 256 points I need a spectrum that has 256 points in it, not 500!
  
})

testthat::context('Empirical Wavelet Transform')
testthat::test_that('Get EWT spectrum',{
  # actual function in utils: get_ewt_spectrum
  accelVec <- datAccel$x[1:512] # A 512 length acceleration vector  
  accelVecSpec <- mhealthtools:::get_spectrum(values = accelVec,
                                             sampling_rate = 100,
                                             nfreq = 500) # 500x2 spectrum
  
  testthat::expect_is(
    mhealthtools:::get_ewt_spectrum(spectrum = accelVecSpec),'matrix') 
  # Check if output is in correct format
  
})

testthat::context('Individual feature extraction functions')
testthat::test_that('fatigue',{
  # actual function in utils.R: fatigue 
  
  testthat::expect_is(mhealthtools:::fatigue(x = tapInter),'list')
})

testthat::test_that('Calculate Drift',{
  # actual function in utils.R: calculate_drift
  
  testthat::expect_is(mhealthtools:::calculate_drift(x = datTap$x,
                                                     y = datTap$y),
                      'numeric')
})

testthat::test_that('Mean Teager-Kaiser Energy (mtkeo)',{
  # actual function in utils.R: mean_tkeo 
  
  testthat::expect_is(mhealthtools:::mean_tkeo(x = tapInter), 'numeric')
})

testthat::test_that('Co-efficient of Variation (coef_var)',{
  # actual function in utils.R: coef_var
  
  testthat::expect_is(mhealthtools:::coef_var(x = tapInter), 'numeric')
})



