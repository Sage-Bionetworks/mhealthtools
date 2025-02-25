% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{walk_data}
\alias{walk_data}
\title{Device-Motion data from a performed walk activity}
\format{
A 3002 x 6 data frame with column names \code{attitude},
\code{timestamp}, \code{rotationRate}, \code{userAcceleration},
\code{gravity}, and \code{magneticField}. The column names correspond
to the measurements returned by a
[CMDeviceMotion](https://developer.apple.com/documentation/coremotion/cmdevicemotion)
object as described in Apple's developer documentation. \code{rotationRate},
\code{userAcceleration},\code{gravity} are each 3002 x 3 data frames with column names
\code{x}, \code{y}, \code{z} containing axial measurements for their respective sensor.
\code{magneticField} has an additional column \code{accuracy} specifying
the calibration accuracy of the magnetic field estimate. \code{attitude}
columns are quaternion coefficients.
}
\usage{
walk_data
}
\description{
The walk activity entails participants walking in a straight line for
approximately 20 steps with the mobile device in their pocket or in a bag.
}
\keyword{datasets}
