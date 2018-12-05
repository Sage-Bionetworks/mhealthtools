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
# (throws error for NA window length) window_start_end_times 
# All HC - Hard Coded examples have their data stored in
# R/sysdata.rda (http://r-pkgs.had.co.nz/data.html)
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
  
  # HC - Hard Coded example
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
    
    testthat::expect_is(
      mhealthtools:::get_left_right_events_and_tap_intervals(tap_data = datTap,
                                                             depress_threshold = 10),
      'list') # Check if output is in correct format, for a different 
    # depress_threshold (default is 20)
    
    tempDat <- datTap[1:2,] 
    # Only 2 rows, lesser than the required 5, Should expect an error
    testthat::expect_equal(
      mhealthtools:::get_left_right_events_and_tap_intervals(tap_data = tempDat),
      list(tap_data = NA, tap_intervals = NA, error = TRUE))
    
    # HC - Hard Coded example  
    testDat <- datTap[1:8,]
    testthat::expect_equal(
      mhealthtools:::get_left_right_events_and_tap_intervals(tap_data = testDat),
      mhealthtools:::test_get_left_right_events_and_tap_intervals
    )
    
  })

tapInter <- mhealthtools:::get_left_right_events_and_tap_intervals(
  tap_data = datTap)$tap_intervals
# Get inter tap intervals

testthat::test_that('Extract default inter tap time features',{
  # actual function in utils.R: intertap_summary_features
  
  testthat::expect_is(
    mhealthtools:::intertap_summary_features(tap_intervals = tapInter),'data.frame')
  
  # HC - Hard Coded example
  testthat::expect_equal(
    mhealthtools:::intertap_summary_features(tap_intervals = tapInter),
    mhealthtools:::test_intertap_summary_features
  )
  
})

testthat::test_that('Extract default tap drift features',{
  # actual function in utils.R: tapdrift_summary_features
  
  # tap drift is a numeric vector, like tapInter. So we are going to
  # use tapInter to test the function rather than creating a seperate 
  # tapDrift numeric vector
  testthat::expect_is(
    mhealthtools:::tapdrift_summary_features(tap_drift = tapInter),'data.frame')
  
  # HC - Hard Coded example
  testthat::expect_equal(
    mhealthtools:::tapdrift_summary_features(tap_drift = tapInter),
    mhealthtools:::test_tapdrift_summary_features
  )
  
})

testthat::test_that(
  'Extract deafult tap data features (features based on interaction 
  between x and y, etc.)',{
    # actual function in utils.R: tap_data_summary_features
    
    testthat::expect_is(
      mhealthtools:::tap_data_summary_features(tap_data = datTap), 'data.frame')
    
    # HC - Hard Coded example    
    testthat::expect_equal(
      mhealthtools:::tap_data_summary_features(tap_data = datTap),
      mhealthtools:::test_tap_data_summary_features
    )
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
  # because there is a non-zero error value in the error column
  testthat::expect_equal(tidy_sensor_data(sensor_data = tempDat), tempDat)
  
  # this will set off warnings of not having columns error and t,
  # so we are supressing warnings and then will re-start them
  options(warn = 0) # Turn off warnings
  tempDat <- tempDat %>% dplyr::select(x,y,z) 
  # Removing the t and error columns, tidy_sensor_data throw an error
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::tidy_sensor_data(sensor_data = tempDat)), T)
  options(warn = 1) # Turn warnings on again
  
  # HC - Hard Coded example
  testthat::expect_equal(
    mhealthtools::tidy_sensor_data(sensor_data = datAccel),
    mhealthtools:::test_tidy_sensor_data
  )
  
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
  
  # Check to see how NA's are handled (they are removed, i.e.,
  # below we will see points 2,3,4,6 as 1 has (time NA),5 has (values NA))
  mhealthtools:::detrend(time = c(NA,2,3,4,5,6), values = c(10,20,30,40,NA,60))
  
  # HC - Hard Coded example
  testthat::expect_equal(
    mhealthtools:::detrend(time = datAccel$t[1:100], values = datAccel$y[1:100]),
    mhealthtools:::test_detrend
  )

})

testthat::test_that("Detrend the given sensor data",{
  # actual function in utils: mutate_detrend
  testthat::expect_is(
    mhealthtools:::mutate_detrend(sensor_data = datAccelTidy),'data.frame') 
  # Should give a dataframe if data is input in tidy format
  
  # Given wrong format of data, the function should throw an error
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::mutate_detrend(sensor_data = datAccel)), T)
  
  # HC - Hard Coded example
  testDat <- datAccelTidy %>% dplyr::filter(t < 1)
  testthat::expect_equal(
    mhealthtools:::mutate_detrend(sensor_data = testDat),
    mhealthtools:::test_mutate_detrend
  )
  
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
  
  # HC - Hard Coded example
  testDat <- datAccel$x[1:200]
  testthat::expect_equal(
    mhealthtools:::bandpass(values = testDat,
                            window_length = 20,
                            sampling_rate = 100,
                            frequency_range = c(2,8)),
    mhealthtools:::test_bandpass
  )
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
  
  # HC - Hard Coded example
  testDat <- datAccelTidy %>% dplyr::filter(t < 1)
  testthat::expect_equal(
    mhealthtools:::mutate_bandpass(
      sensor_data = testDat,
      window_length = 50,
      sampling_rate = 100,
      frequency_range = c(4,20)),
    mhealthtools:::test_mutate_bandpass
  )
  
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
    
    tempDat <- data.frame(t = c(NA, seq(10),NA), x = sample(100,12)/10)
    tempDat$t[5] <- NA
    testthat::expect_is(mhealthtools:::filter_time(sensor_data = tempDat,
                                                   t1 = 1,
                                                   t2 = 20),
                        'data.frame') 
    # NA behavior, they are removed i.r t = c(1,2,NA,4,5,NA), t1 = 2, t2 = 4,
    # then the output will have t = c(2,4), NA's (3 was missing) are removed
    
    # HC - Hard Coded example
    testthat::expect_equal(
      mhealthtools:::filter_time(sensor_data = datAccelTidy,
                                 t1 = 1, t2 = 2),
      mhealthtools:::test_filter_time
    )
})

testthat::context('Windowing')
testthat::test_that('Windowing a time series',{
  # actual function in utils: window_signal
  
  testthat::expect_is(mhealthtools:::window_signal(values = datAccel$x), 'matrix')
  # Check if output is in correct format
  
  # If input is all NA's except for a value(the last one, we should expect something
  # similar, all NAs except for one value, which is what we see, also if we input all
  # NA's we see an output with all NA's )
  gravityVec <- c(rep(NA,255),1)
  tempVal <- mhealthtools:::window_signal(values = gravityVec)
  testthat::expect_is(tempVal, 'matrix')
  
  tempVec <- seq(1,50) # length 10
  tempVal <- mhealthtools:::window_signal(values = tempVec,
                                          window_length = 256,
                                          window_overlap = 0.5)
  testthat::expect_error(mhealthtools:::window_signal(values = tempVec,
                                                      window_length = 256,
                                                      window_overlap = 0.5))
  # Above we have a length 50, and a window_length of 256,
  # if you plot tempVal(the output of window_signal), you will see a lot
  # of spurious data, which we don't need
  # We should throw an error if the window_length is less than that of
  # the input signal, or correct it by changing the window_length to that
  # of the input, or zero-pad the input to get to the signal of the length
  # window_length
  
  # HC - Hard Coded example
  testDat <- seq(16)
  testthat::expect_equal(
    mhealthtools:::window_signal(values = testDat, 
                                 window_length = 4,
                                 window_overlap = 0.5),
    mhealthtools:::test_window_signal
  )
})

testthat::test_that('Windowing the sensor data by axis',{
  # actual function in utils: window
  testthat::expect_is(mhealthtools:::window(sensor_data = datAccelTidy,
                                            window_length =  256,
                                            window_overlap =  0.5),
                      'data.frame')
  # 256 window length, 0.5 overlap, checking output format
  
  tempDat <- datAccelTidy %>%
    dplyr::filter(t < 2)
  # A max possible window length of 2s ~ 2 x 100(sampling rate) = 200 samples 
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::window(sensor_data = tempDat,
                          window_length =  256,
                          window_overlap =  0.5)), T)
  # We should throw an error if window_length is more than that possible,
  # or we should handle these errors differently
  # Maybe throw an error frame with 'window_length greater than max t' etc.,
  # Look at the test for window_signal that tackles the problem at a unit level,
  # because the above test for window would be a regression test.
  
  testthat::expect_equal(is_error_dataframe(
    mhealthtools:::window(
      sensor_data = datAccel,
      window_length = 256,
      window_overlap = 0.5)), T)
  # throw an error if Input is not in correct format
  
  # HC - Hard Coded example
  testDat <- datAccelTidy %>% dplyr::filter(t<1)
  testthat::expect_equal(
    mhealthtools:::window(
      sensor_data = testDat,
      window_length = 60,
      window_overlap = 0.5),
    mhealthtools:::test_window
  )
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
  
  
  # HC - Hard Coded example
  testDat <- datAccel$t[1:40]
  testthat::expect_equal(
    mhealthtools:::window_start_end_times(
      t = testDat,
      window_length = 10,
      window_overlap = 0.5),
    mhealthtools:::test_window_start_end_times
  )
  
})

testthat::context('Derivative Calculation')
testthat::test_that(
  'Calculate Derivative given acceleration and sampling rate',{
    # actual function in utils: derivative
    
    testthat::expect_is(mhealthtools:::derivative(v = datAccel$x), 'numeric') 
    # Check if output is in correct format
    
    # HC - Hard Coded example
    testDat <- datAccel$x[1:100]
    testthat::expect_equal(
      mhealthtools:::derivative(testDat),
      mhealthtools:::test_derivative
    )
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
    
    testthat::expect_equal(is_error_dataframe(
      mhealthtools:::mutate_derivative(
        sensor_data = datAccelTidy,
        col = 'x',
        derived_col = 'dx')), T) 
    # If Sampling rate is not provided, instead of outputting error(current status)
    # can we just calculate sampling rate using mhealthtools:::get_sampling_rate
    # for the default setting if sampling_rate is not provided, because the docstring
    # for the function, asks for a data frame with the t column
    
    # HC - Hard Coded example
    testDat <- datAccel %>% dplyr::filter(t < 1)
    testthat::expect_equal(
      mhealthtools:::mutate_derivative(sensor_data = testDat,
                                       sampling_rate = 100,
                                       col = 'x',
                                       derived_col = 'dx'),
      mhealthtools:::test_mutate_derivative
    )
    
  })

testthat::context('Integral Calculation')
testthat::test_that(
  'Calculate Integral given acceleration and sampling rate',{
    # actual function in utils: integral
    
    testthat::expect_is(mhealthtools:::integral(v = datAccel$x), 'numeric') 
    # Check if output is in correct format
    
    # HC - Hard Coded example
    testDat <- datAccel$x[1:100]
    testthat::expect_equal(
      mhealthtools:::integral(v = testDat),
      mhealthtools:::test_integral
    )
    
    
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
    
    testthat::expect_equal(is_error_dataframe(
      mhealthtools:::mutate_integral(
        sensor_data = datAccelTidy,
        col = 'x',
        derived_col = 'dx')), F) 
    # If Sampling rate is not provided, instead of outputting error(current status)
    # can we just calculate sampling rate using mhealthtools:::get_sampling_rate
    # for the default setting if sampling_rate is not provided, because the docstring
    # for the function, asks for a data frame with the t column
    
    # HC - Hard Coded example
    testDat <- datAccel %>% dplyr::filter(t < 1)
    testthat::expect_equal(
      mhealthtools:::mutate_integral(sensor_data = testDat,
                                       sampling_rate = 100,
                                       col = 'x',
                                       derived_col = 'dx'),
      mhealthtools:::test_mutate_integral
    )
    
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
    
    ## HC EXAMPLE TO BE WRITTEN AFTER FUNCTION CHANGES
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
                             max = as.numeric(NA),
                             min = as.numeric(NA),
                             stringsAsFactors=FALSE) )
    
    # HC - Hard Coded example
    testDat <- c(1,2,5,9,10,22,4,5,90)
    testthat::expect_equal(
      mhealthtools:::tag_outlier_windows_(gravity_vector = testDat,
                                          window_length = 4,
                                          window_overlap =  0.5)
    ) 
    ## HC EXAMPLE TO BE WRITTEN AFTER FUNCTION CHANGES
    
  })

testthat::test_that(
  'Identify windows in which Phone might have been flipped/rotated given gravity',{
    # actual function in utils: tag_outlier_windows
    
    testthat::expect_is(
      mhealthtools:::tag_outlier_windows(gravity = dat$gravity,
                                         window_length = 256,
                                         window_overlap = 0.5),
      'data.frame')
    
    testthat::expect_equal(
      is_error_dataframe(mhealthtools:::tag_outlier_windows(gravity = dat$gravity[1:100],
                                                            window_length = 256,
                                                            window_overlap = 0.5)),
      T)
    # Throw an error frame if window_length more than gravity
    
    ## HC EXAMPLE TO BE WRITTEN AFTER FUNCTION CHANGES
    
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
  
  # HC - Hard Coded example
  testDat <- datAccel$x[1:100]
  testthat::expect_equal(
    mhealthtools:::time_domain_summary(values = testDat,
                                       sampling_rate = 100),
    mhealthtools:::test_time_domain_summary
  )
  
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
  
  # HC - Hard Coded example
  testDat <- datAccel$x[1:200]
  testthat::expect_equal(
    mhealthtools:::frequency_domain_summary(values = testDat,
                                            sampling_rate = 100,
                                            npeaks = 3),
    mhealthtools:::test_frequency_domain_summary
  )
  
})

testthat::test_that('Frequency domain energy given acceleration',{
  # actual function in utils: frequency_domain_energy 
  accelVec <- datAccel$x[1:256] 
  # A 256 (default window length) length acceleration vector  
  
  testthat::expect_is(
    mhealthtools:::frequency_domain_energy(values = accelVec,
                                           sampling_rate = 100),
    'data.frame') # Check if output is of correct format
  
  # HC - Hard Coded example
  testDat <- datAccel$x[1:200]
  testthat::expect_equal(
    mhealthtools:::frequency_domain_energy(values = testDat,
                                            sampling_rate = 100),
    mhealthtools:::test_frequency_domain_energy
  )
  
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
  
  # HC - Hard Coded example
  testDat <- datAccel$x[1:100]
  testthat::expect_equal(
    mhealthtools:::get_spectrum(values = testDat,
                                sampling_rate = 100,
                                nfreq = 20),
    mhealthtools:::test_get_spectrum
  )
  
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
  
  # HC - Hard Coded example
  testDat <- mhealthtools:::get_spectrum(values =  datAccel$x[1:200],
                                         sampling_rate = 100,
                                         nfreq = 50)
  testthat::expect_equal(
    mhealthtools:::get_ewt_spectrum(spectrum = testDat,
                                    npeaks = 3,
                                    fraction_min_peak_height = 0.1,
                                    min_peak_distance = 1,
                                    sampling_rate = 100),
    mhealthtools:::test_get_ewt_spectrum
  )
  
})

testthat::context('Individual feature extraction functions')
testthat::test_that('fatigue',{
  # actual function in utils.R: fatigue 
  
  testthat::expect_is(mhealthtools:::fatigue(x = tapInter),'list')
  
  # HC - Hard Coded example
  testDat <- tapInter
  testthat::expect_equal(
    mhealthtools:::fatigue(x = testDat),
    mhealthtools:::test_fatigue
  )
})

testthat::test_that('Calculate Drift',{
  # actual function in utils.R: calculate_drift
  
  testthat::expect_is(mhealthtools:::calculate_drift(x = datTap$x,
                                                     y = datTap$y),
                      'numeric')
  
  # HC - Hard Coded example
  testX <- datTap$x
  testY <- datTap$y
  testthat::expect_equal(mhealthtools:::calculate_drift(x = testX,y = testY),
                         mhealthtools:::test_calculate_drift)
})

testthat::test_that('Mean Teager-Kaiser Energy (mtkeo)',{
  # actual function in utils.R: mean_tkeo 
  
  testthat::expect_is(mhealthtools:::mean_tkeo(x = tapInter), 'numeric')
  
  # HC - Hard Coded example
  testDat <- tapInter
  testthat::expect_equal(mhealthtools:::mean_tkeo(x = testDat), 0.01147268)
})

testthat::test_that('Co-efficient of Variation (coef_var)',{
  # actual function in utils.R: coef_var
  
  testthat::expect_is(mhealthtools:::coef_var(x = tapInter), 'numeric')
  
  # HC - Hard Coded example
  testDat <- tapInter
  testthat:::expect_equal(mhealthtools:::coef_var(x = testDat), 86.62279)
})



