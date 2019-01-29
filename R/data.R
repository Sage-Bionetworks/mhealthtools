#' Weights for the included GuanLab model
#' 
#' A list of 10 sets of weights to be used with \code{guanlab_nn_architecture}.
#' 
#' @format A list of length 10. Each list item contains a 
#' matrix/array object of varying dimensionality.
#' @source \url{https://www.synapse.org/#!Synapse:syn10146135/wiki/448409}
"guanlab_nn_weights"

#' Accelerometer sensor measurements
#' 
#' @format An 990 x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing accelerometer measurements. \code{t} is
#' the timestamp of each measurement, and \code{x}, \code{y} and \code{z}
#' are linear acceleration measurements along the respective axis. 
"accelerometer_data"

#' Gyroscope sensor measurements
#' 
#' @format An 990 x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing accelerometer measurements. \code{t} is
#' the timestamp of each measurement, and \code{x}, \code{y} and \code{z}
#' are linear velocity measurements along the respective axis. 
"gyroscope_data"

#' Gravity sensor measurements
#' 
#' @format An 990 x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing acceleration measurements due to gravity. 
#' \code{t} is the timestamp of each measurement, and \code{x}, \code{y}
#' and \code{z} are linear gravity measurements along the respective axis. 
"gravity_data"

#' Device-Motion data from a performed balance assay
#' 
#' The balance assay entails participants standing still for 30 seconds
#' with the mobile device in their pocket or in a bag.
#' 
#' @format A 3002 x 6 data frame with column names \code{attitude},
#' \code{timestamp}, \code{rotationRate}, \code{userAcceleration},
#' \code{gravity}, and \code{magneticField}. The column names correspond
#' to the measurements returned by a
#' [CMDeviceMotion](https://developer.apple.com/documentation/coremotion/cmdevicemotion)
#' object as described in Apple's developer documentation. \code{rotationRate},
#' \code{userAcceleration},\code{gravity} are each 3002 x 3 data frames with column names
#' \code{x}, \code{y}, \code{z} containing axial measurements for their respective sensor.
#' \code{magneticField} has an additional column \code{accuracy} specifying
#' the calibration accuracy of the magnetic field estimate. \code{attitude}
#' columns are quaternion coefficients. 
"balance_data"

#' Device-Motion data from a performed walk assay
#' 
#' The walk assay entails participants walking in a straight line for
#' approximately 20 steps with the mobile device in their pocket or in a bag.
#' 
#' @format A 3002 x 6 data frame with column names \code{attitude},
#' \code{timestamp}, \code{rotationRate}, \code{userAcceleration},
#' \code{gravity}, and \code{magneticField}. The column names correspond
#' to the measurements returned by a
#' [CMDeviceMotion](https://developer.apple.com/documentation/coremotion/cmdevicemotion)
#' object as described in Apple's developer documentation. \code{rotationRate},
#' \code{userAcceleration},\code{gravity} are each 3002 x 3 data frames with column names
#' \code{x}, \code{y}, \code{z} containing axial measurements for their respective sensor.
#' \code{magneticField} has an additional column \code{accuracy} specifying
#' the calibration accuracy of the magnetic field estimate. \code{attitude}
#' columns are quaternion coefficients. 
"walk_data"

#' Device-Motion data from a performed resting tremor assay
#' 
#' The resting tremor assay entails participants holding the mobile device
#' in their hand (either right or left) while resting that hand in their lap
#' for approximately 10 seconds.
#' 
#' @format A 3002 x 6 data frame with column names \code{attitude},
#' \code{timestamp}, \code{rotationRate}, \code{userAcceleration},
#' \code{gravity}, and \code{magneticField}. The column names correspond
#' to the measurements returned by a
#' [CMDeviceMotion](https://developer.apple.com/documentation/coremotion/cmdevicemotion)
#' object as described in Apple's developer documentation. \code{rotationRate},
#' \code{userAcceleration},\code{gravity} are each 3002 x 3 data frames with column names
#' \code{x}, \code{y}, \code{z} containing axial measurements for their respective sensor.
#' \code{magneticField} has an additional column \code{accuracy} specifying
#' the calibration accuracy of the magnetic field estimate. \code{attitude}
#' columns are quaternion coefficients. 
"rest_tremor_data"

#' Device-Motion data from a performed kinetic tremor assay
#' 
#' The kinetic tremor assay (also known as the finger/hand to nose test)
#' entails participants holding the mobile device
#' in their hand (either right or left) with both the mobile device and the
#' participants elbow at nose level. The participant then moves the mobile device
#' away from the nose, with both mobile device and elbow parallel to the ground
#' and the elbow joint acting as an axis of rotation. When the arm is fully extended,
#' the participant moves the mobile device back to the nose. This happens in a 
#' continuous motion and the motion is repeated for approximately 10 seconds.
#' 
#' @format A 3002 x 6 data frame with column names \code{attitude},
#' \code{timestamp}, \code{rotationRate}, \code{userAcceleration},
#' \code{gravity}, and \code{magneticField}. The column names correspond
#' to the measurements returned by a
#' [CMDeviceMotion](https://developer.apple.com/documentation/coremotion/cmdevicemotion)
#' object as described in Apple's developer documentation. \code{rotationRate},
#' \code{userAcceleration},\code{gravity} are each 3002 x 3 data frames with column names
#' \code{x}, \code{y}, \code{z} containing axial measurements for their respective sensor.
#' \code{magneticField} has an additional column \code{accuracy} specifying
#' the calibration accuracy of the magnetic field estimate. \code{attitude}
#' columns are quaternion coefficients. 
"kinetic_tremor_data"