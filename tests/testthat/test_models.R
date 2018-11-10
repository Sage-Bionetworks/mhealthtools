####################################################
# File to test models.R of the mHealthTools package
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
####################################################

######################## *** NOTE *** ########################
## Still have to write tests for 
# (throws error) guanlab_model
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
# library(purrr)

### Load data file
testthat::context('Load Required Data Files')
dat <- mhealthtools::sensor_data

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
testthat::context('Setting up GuanLab Model')
testthat::test_that('Generate GuanLab architechture',{
  # actual function in models.R: guanlab_nn_architecture
  
  testthat::expect_is(mhealthtools:::guanlab_nn_architecture(),
                      'keras.engine.sequential.Sequential')
  # Keras sequential model
  
})

testthat::test_that('Load weights into GuanLab architechture',{
  # actual function in models.R: load_guanlab_model
  
  testthat::expect_is(mhealthtools:::load_guanlab_model(),'list')
  # output is a list of Keras sequential models
  
})

testthat::context('Extract features using GuanLab Model',{
  # actual function in models.R: guanlab_model
  
  testthat::expect_is(mhealthtools::guanlab_model(sensor_data = datAccel),
                      'data.frame')
  # expose guanlab_model to main functions (:: vs :::)
  # Also predict_proba in guanlab_model predicts 1 or 0, but we need a probability
})