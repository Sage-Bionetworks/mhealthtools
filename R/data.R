#' Weights for the included GuanLab model
#' 
#' A list of 10 sets of weights to be used with \code{guanlab_nn_architecture}.
#' 
#' @format A list of length 10. Each list item contains a 
#' matrix/array object of varying dimensionality.
#' @source \url{https://www.synapse.org/#!Synapse:syn10146135/wiki/448409}
"guanlab_nn_weights"

#' Sample heartrate data from a smartphone camera
#' 
#' A dataframe containing sample JSON output format of the heartrate data,
#' containing red, green, blue levels from the camera sensor, along with the
#' timestamp, and the derived metrics of hue, saturation and brightness.
#' 
#' A video(240p) of a finger covering the smartphone camera with the flash
#' turned on was recorded at 60fps. The average intensity of all the pixels per
#' frame in all the three color channels was calculated, and indexed along
#' the timestamp of the frame. We also have hue, brightness and saturation
#' calculated in a similar way.
#' 
#' @format A data frame with 3661 rows (observations) and 7 variables:
#' \describe{
#'   \item{t}{time, in seconds}
#'   \item{green}{mean green intensity across the frame at the given timestamp}
#'   \item{blue}{mean blue intensity across the frame at the given timestamp}
#'   \item{red}{mean red intensity across the frame at the given timestamp}
#'   \item{brightness}{mean brightness across the frame at the given timestamp}
#'   \item{hue}{mean hue across the frame at the given timestamp}
#'   \item{saturation}{mean saturation across the frame at the given timestamp}
#' }
"heartrate_data"

#' Sample tapping data from the tapping assay on a smartphone
#' 
#' A dataframe containing sample JSON output format of the tap data,
#' containing t(time), x and y (the location of the tap on the screen),
#' and buttonid (which of left/right/No button was tapped). 
#' 
#' Participants were shown a screen with two buttons on it, 
#' indicating left and right. They were asked to tap on the buttons
#' alternatingly as fast as they can for 30s. 
#' 
#' @format A data frame with 181 rows (observations) and 4 variables:
#' \describe{
#'   \item{t}{time, in seconds}
#'   \item{x}{the x location of the tap on the phone screen}
#'   \item{y}{the y location of the tap on the phone screen}
#'   \item{buttonid}{a string from 'TappedButonLeft', 'TappedButtonRight' or
#'   'TappedButtonNone' indicating that at that time the left, right or No 
#'   button was tapped respectively}
#' }
"tap_data"