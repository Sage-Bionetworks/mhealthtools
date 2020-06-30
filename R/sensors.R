#' Extract sensor features
#'
#' A 'pure' implementation of the feature extraction process. This function
#' is not normally called directly.
#'
#' The feature extraction paradigm is implemented as such:
#'
#' Input: raw sensor data
#' Transform: into a format suitable for calculating statistics upon
#' Extract: features by computing statistics upon individual columns
#'          within grouped rows -- or by using a model
#' Return: A list of features
#'
#' @param sensor_data A dataframe.
#' @param transform A function or list of functions to be applied sequentially
#' to \code{sensor_data}.
#' @param extract A function or list of functions to be applied to each of the
#' columns in \code{extract_on} from the output of \code{transform}. Each function
#' should return a single-row dataframe of features. If the output of
#' \code{transform} has incidental columns (columns not contained in either the
#' grouping columns of \code{transform} nor \code{extract_on}, they will be
#' preserved in the outputted features.
#' @param extract_on A string or list of column names to compute features from.
#' If \code{NULL}, features will be extracted from all non-grouped columns.
#' @param models A function or list of functions which accept the output from
#' \code{transform} as input and output features.
#' @return A list of features. The output from \code{extract} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If \code{transform}
#' returns an error dataframe (see \code{has_error}), the error dataframe is stored
#' under \code{$error}.
sensor_features <- function(sensor_data, transform = NULL, extract = NULL,
                            extract_on = NULL, models = NULL) {

  features <- list(extracted_features = NULL,
                   model_features = NULL,
                   error = NULL)

  if (is.null(transform)) {
    transform <- list(function(x) x)
  } else if (!is.list(transform)) {
    transform <- list(transform)
  }

  transformed_sensor_data <- sensor_data %>%
    purrr::reduce(rev(transform), purrr::compose,
                  .init = function(x) x, .dir = "backward")()
  if (has_error(transformed_sensor_data)) {
    features$error <- transformed_sensor_data
    return(features)
  }

  if (is.function(extract)) {
    extract <- list(extract)
  }
  if (is.null(extract_on)) {
    extract_on <- setdiff(colnames(transformed_sensor_data),
                          dplyr::group_vars(transformed_sensor_data))
  }
  if (is.function(models)) {
    models <- list(models)
  }

  if (!is.null(extract) && !is.null(extract_on)) {
    transformed_sensor_data_groups <- dplyr::groups(transformed_sensor_data)
    incidental_cols_to_preserve <- transformed_sensor_data %>%
      dplyr::select(-dplyr::one_of(extract_on)) %>%
      dplyr::distinct(!!!transformed_sensor_data_groups, .keep_all = T)
    features$extracted_features <- purrr::map_dfr(
      extract_on,
      ~ extract_features(transformed_sensor_data, ., extract)) %>%
      dplyr::distinct(!!!transformed_sensor_data_groups,
                      measurementType,
                      .keep_all = T) %>%
      dplyr::left_join(incidental_cols_to_preserve) %>%
      dplyr::select(measurementType,
                    dplyr::one_of(names(incidental_cols_to_preserve)),
                    dplyr::everything())
  }
  if (!is.null(models)) {
    features$model_features <- purrr::map(models, ~ .(transformed_sensor_data))
  }
  if (is.null(extract) && is.null(models)) {
    warning(paste("No feature extraction function list or model list passed.",
                  "Returning an empty feature list."))
  }
  return(features)
}

#' Default feature extraction functions for accelerometer and gyroscope data.
#'
#' @param sampling_rate Sampling rate of the data in Hz.
#' @return A list of closures for three feature extraction functions supplied
#' with this package that can be passed to the \code{funs} parameter
#' in \code{*_features} functions.
#' @export
#' @author Phil Snyder
#' @examples
#' funs = default_kinematic_features(100)
default_kinematic_features <- function(sampling_rate) {
  funs <- list(
    time_domain_summary = purrr::partial(time_domain_summary,
                                         sampling_rate = sampling_rate),
    frequency_domain_summary = purrr::partial(frequency_domain_summary,
                                              sampling_rate = sampling_rate,
                                              npeaks = 3),
    frequency_domain_energy = purrr::partial(frequency_domain_energy,
                                             sampling_rate = sampling_rate))
  return(funs)
}

#' Extract accelerometer features
#'
#' A convenience wrapper function for extracting interpretable features
#' from triaxial accelerometer data collected through smartphones.
#'
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing accelerometer measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear acceleration measurements.
#' @param time_filter A length 2 numeric vector specifying the time range
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not
#' filter any measurements.
#' @param detrend A logical value indicating whether to detrend the signal.
#' By default the value is FALSE.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition (EMD)
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length A numerical value representing the length (in number of samples)
#' of the sliding window used during the windowing transformation. Both
#' \code{window_length} and \code{window_overlap} must be set for the windowing
#' transformation to be applied.
#' @param window_overlap Fraction in the interval [0, 1) specifying the amount of
#' window overlap during a windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics A logical value specifying whether to add derived
#' kinematic measurements (\code{displacement}, \code{velocity}, and \code{jerk})
#' to \code{sensor_data} after the transform defined by the above parameters has
#' been applied to the raw sensor measurements.
#' @param funs A function or list of functions that each accept a single numeric
#' vector as input. Each function should return a dataframe of features
#' (normally a single-row datafame, with column names as feature names).
#' The input vectors will be the axial measurements from \code{sensor_data}
#' after the transform defined by the above parameters has been applied.
#' If no argument is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A function or list of functions that each accept
#' \code{sensor_data} as input after the transform defined by the above
#' parameters has been applied and returns features. Useful for functions
#' which compute individual features using multiple input variables.
#' @return A list of accelerometer features. The output from \code{funs} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If there is an error during
#' extraction, the returned result will be stored under \code{$error}.
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
#' @examples
#' accel_features <- accelerometer_features(accelerometer_data)
#'
#' accel_features <- accelerometer_features(accelerometer_data, IMF = 3)
#'
#' accel_features <- accelerometer_features(
#'   accelerometer_data,
#'   time_filter = c(2, 5),
#'   detrend = TRUE,
#'   frequency_filter = c(4, 16),
#'   window_length = 256,
#'   window_overlap = 0.5,
#'   derived_kinematics = TRUE,
#'   funs = time_domain_summary)
#'
#' @importFrom magrittr "%>%"
accelerometer_features <- function(sensor_data, time_filter = NULL, detrend = F,
                                   frequency_filter = NULL, IMF = 1,
                                   window_length = NULL, window_overlap = NULL,
                                   derived_kinematics = F, funs = NULL,
                                   models = NULL) {
  args <- prepare_kinematic_sensor_args(
    sensor_data = sensor_data,
    metric = "acceleration",
    time_filter = time_filter,
    detrend = detrend,
    frequency_filter = frequency_filter,
    IMF = IMF,
    window_length = window_length,
    window_overlap = window_overlap,
    derived_kinematics = derived_kinematics,
    funs = funs,
    models = models)
  features <- sensor_features(
    sensor_data = sensor_data,
    transform = args$transform,
    extract = args$extract,
    extract_on = args$extract_on,
    models = models)
  return(features)
}

#' Extract gyroscope features
#'
#' A convenience wrapper function for extracting interpretable features
#' from triaxial gyroscope data collected through smartphones.
#'
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing gyroscope measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear velocity measurements.
#' @param time_filter A length 2 numeric vector specifying the time range
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not
#' filter any measurements.
#' @param detrend A logical value indicating whether to detrend the signal.
#' By default the value is FALSE.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition (EMD)
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length A numerical value representing the length (in number of samples)
#' of the sliding window used during the windowing transformation. Both
#' \code{window_length} and \code{window_overlap} must be set for the windowing
#' transformation to be applied.
#' @param window_overlap Fraction in the interval [0, 1) specifying the amount of
#' window overlap during a windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics A logical value specifying whether to add derived
#' kinematic measurements (\code{displacement}, \code{acceleration}, and \code{jerk})
#' to \code{sensor_data} after the transform defined by the above parameters has
#' been applied to the raw sensor measurements.
#' @param funs A function or list of functions that each accept a single numeric
#' vector as input. Each function should return a dataframe of features
#' (normally a single-row datafame, with column names as feature names).
#' The input vectors will be the axial measurements from \code{sensor_data}
#' after the transform defined by the above parameters has been applied.
#' If no argument is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A function or list of functions that each accept
#' \code{sensor_data} as input after the transform defined by the above
#' parameters has been applied and returns features. Useful for functions
#' which compute individual features using multiple input variables.
#' @return A list of gyroscope features. The output from \code{funs} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If there is an error during
#' extraction, the returned result will be stored under \code{$error}.
#' @export
#' @author Thanneer Malai Perumal, Meghasyam Tummalacherla, Phil Snyder
#' @examples
#' gyro_features <- gyroscope_features(gyroscope_data)
#'
#' gyro_features <- gyroscope_features(gyroscope_data, IMF = 3)
#'
#' gyro_features <- gyroscope_features(
#'   gyroscope_data,
#'   time_filter = c(2, 5),
#'   detrend = TRUE,
#'   frequency_filter = c(4, 16),
#'   window_length = 256,
#'   window_overlap = 0.5,
#'   derived_kinematics = TRUE,
#'   funs = time_domain_summary)
#'
#' @importFrom magrittr "%>%"
gyroscope_features <- function(sensor_data, time_filter = NULL, detrend = F,
                               frequency_filter = NULL, IMF = 1,
                               window_length = NULL, window_overlap = NULL,
                               derived_kinematics = F, funs = NULL,
                               models = NULL) {
  args <- prepare_kinematic_sensor_args(
    sensor_data = sensor_data,
    metric = "velocity",
    time_filter = time_filter,
    detrend = detrend,
    frequency_filter = frequency_filter,
    IMF = IMF,
    window_length = window_length,
    window_overlap = window_overlap,
    derived_kinematics = derived_kinematics,
    funs = funs,
    models = models)
  features <- sensor_features(
    sensor_data = sensor_data,
    transform = args$transform,
    extract = args$extract,
    extract_on = args$extract_on,
    models = models)
  return(features)
}

#' Ensure the kinematic sensor arguments are well formed
#'
#' Verify that the arguments passed to \code{accelerometer_features} and
#' \code{gyroscope_features} are valid.
#'
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing kinematic sensor (accelerometer or gyroscope)
#' measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear axial measurements.
#' @param time_filter A length 2 numeric vector specifying the time range
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not
#' filter any measurements.
#' @param detrend A logical value indicating whether to detrend the signal.
#' @param sampling_rate Sampling rate of \code{sensor_data}.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length A numerical value representing the length (in number of samples)
#' of the sliding window used during the windowing transformation. Both
#' \code{window_length} and \code{window_overlap} must be set for the windowing
#' transformation to be applied.
#' @param window_overlap Fraction in the interval [0, 1) specifying the amount of
#' window overlap during a windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics A logical value specifying whether to add derived
#' kinematic measurements to \code{sensor_data} after the transform defined by
#' the above parameters has been applied to the raw sensor measurements.
#' @param funs A function or list of functions that each accept a single numeric
#' vector as input. Each function should return a dataframe of features
#' (normally a single-row datafame, with column names as feature names).
#' The input vectors will be the axial measurements from \code{sensor_data}
#' after the transform defined by the above parameters has been applied.
#' If no argument is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A function or list of functions that each accept
#' \code{sensor_data} as input after the transform defined by the above
#' parameters has been applied and returns features. Useful for functions
#' which compute individual features using multiple input variables.
#' @return A list of gyroscope features. The output from \code{funs} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If there is an error during
#' extraction, the returned result will be stored under \code{$error}.
kinematic_sensor_argument_validator <- function(
  sensor_data, time_filter, detrend, sampling_rate, frequency_filter, IMF,
  window_length, window_overlap, derived_kinematics, funs, models) {
  if (!all(is.data.frame(sensor_data),
           hasName(sensor_data, "t"),
           hasName(sensor_data, "x"),
           hasName(sensor_data, "y"),
           hasName(sensor_data, "z"),
           all(unlist(purrr::map(sensor_data, is.numeric))))) {
    stop("sensor_data must be a dataframe with numeric columns t, x, y, z")
  }
  if (!is.null(time_filter) && !all(is.numeric(time_filter),
                                    length(time_filter) == 2,
                                    time_filter[1] >= 0,
                                    time_filter[2] > 0,
                                    time_filter[2] > time_filter[1])) {
    stop(paste("If time_filter is set to a non-NULL value, it must be numeric,",
               "have length two, the first value must be greater or equal to 0,",
               "and the second value must be strictly greater than the first,"))
  }
  if (!is.logical(detrend)) {
    stop("detrend must be a logical value.")
  }
  if (!is.null(frequency_filter) && !all(is.numeric(frequency_filter),
                                         length(frequency_filter) == 2,
                                         frequency_filter[1] >= 0,
                                         frequency_filter[2] > 0,
                                         frequency_filter[2] > frequency_filter[1])) {
    stop(paste("If frequency_filter is set to a non-NULL value, it must be numeric,",
               "have length two, the first value must be greater or equal to 0,",
               "the second value must be strictly greater than 0, and the",
               "second value must be strictly greater than the first"))
  }
  if (!is.null(frequency_filter) && frequency_filter[1] >= sampling_rate / 2) {
    stop(paste("The min value in frequency_filter must be strictly less than",
               "the Nyquist frequency of sensor_data."))
  }
  if (!is.numeric(IMF) || IMF < 1 || (IMF != as.integer(IMF))) {
    stop("IMF must be a whole number strictly greater than 0")
  }
  if (!is.null(window_length) && !all(is.numeric(window_length),
                                      window_length > 0,
                                      window_length == as.integer(window_length))) {
    stop(paste("If window_length is set to a non-NULL value, it must be a",
               "positive whole number"))
  }
  if (!is.null(window_overlap) && !all(is.numeric(window_overlap),
                                       window_overlap >= 0,
                                       window_overlap < 1)) {
    stop(paste("If window_overlap is set to a non-NULL value, it must be a",
               "positive real number in the interval [0, 1)."))
  }
  if (!is.logical(derived_kinematics)) {
    stop("derived_kinematics must be a logical value.")
  }
  if (!is.null(funs) && !is.function(funs)) {
    if (!all(is.list(funs), all(unlist(purrr::map(funs, is.function))))) {
      stop(paste("If funs is set to a non-NULL value, it must be a function",
                 "or a list of functions"))
    }
  }
  if (!is.null(models) && !is.function(models)) {
    if (!all(is.list(models), all(unlist(purrr::map(models, is.function))))) {
      stop(paste("If models is set to a non-NULL value, it must be a function",
                 "or a list of functions"))
    }
  }
  if (xor(is.null(window_length), is.null(window_overlap)) && IMF == 1) {
    stop(paste("If one of window_length or window_overlap is set to a non-NULL",
               "value, then both must be set to non-NULL values"))
  }
}


#' Return arguments to be used in general feature functions
#'
#' Using the arguments passed to convenience functions like
#' \code{accelerometer_features} and \code{gyroscope_features},
#' build an argument set that can be used with more general functions
#' like \code{kinematic_sensor_data} and \code{sensor_data}. This function
#' is not normally called directly. See \code{accelerometer_features} and
#' \code{gyroscope_features}.
#'
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing kinematic sensor (accelerometer or gyroscope)
#' measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are axial measurements.
#' @param metric Name of the metric measured by this sensor. For accelerometer
#' data, the metric is acceleration. Whereas for gyroscope data the metric is
#' velocity.
#' @param time_filter A length 2 numeric vector specifying the time range
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not
#' filter any measurements.
#' @param detrend A logical value indicating whether to detrend the signal.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length A numerical value representing the length (in number of samples)
#' of the sliding window used during the windowing transformation. Both
#' \code{window_length} and \code{window_overlap} must be set for the windowing
#' transformation to be applied.
#' @param window_overlap Fraction in the interval [0, 1) specifying the amount of
#' window overlap during a windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics A logical value specifying whether to add derived
#' kinematic measurements to \code{sensor_data} after the transform defined by
#' the above parameters has been applied to the raw sensor measurements.
#' @param funs A function or list of functions that each accept a single numeric
#' vector as input. Each function should return a dataframe of features
#' (normally a single-row datafame, with column names as feature names).
#' The input vectors will be the axial measurements from \code{sensor_data}
#' after the transform defined by the above parameters has been applied.
#' If no argument is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A function or list of functions that each accept
#' \code{sensor_data} as input after the transform defined by the above
#' parameters has been applied and returns features. Useful for functions
#' which compute individual features using multiple input variables.
#' @return A list of arguments to be used in the general feature functions.
prepare_kinematic_sensor_args <- function(sensor_data, metric,
                                          time_filter = NULL, detrend = F,
                                          frequency_filter = NULL, IMF = 1,
                                          window_length = NULL,
                                          window_overlap = NULL,
                                          derived_kinematics = F, funs = NULL,
                                          models = NULL) {
  sampling_rate <- get_sampling_rate(sensor_data)
  kinematic_sensor_argument_validator(
    sensor_data = sensor_data,
    time_filter = time_filter,
    detrend = detrend,
    sampling_rate = sampling_rate,
    frequency_filter = frequency_filter,
    IMF = IMF,
    window_length = window_length,
    window_overlap = window_overlap,
    derived_kinematics = derived_kinematics,
    funs = funs,
    models = models)
  if (is.null(funs) && is.null(models)) {
    funs <- default_kinematic_features(sampling_rate = sampling_rate)
  } else if (!is.null(funs)) {
    if (!is.list(funs)) funs <- list(funs)
    funs <- purrr::map(funs, function(f) {
      if (identical(f, time_domain_summary)) {
        return(purrr::partial(time_domain_summary,
                              sampling_rate = sampling_rate))
      } else if (identical(f, frequency_domain_energy)) {
        return(purrr::partial(frequency_domain_energy,
                              sampling_rate = sampling_rate))
      } else if (identical(f, frequency_domain_summary)) {
        return(purrr::partial(frequency_domain_summary,
                              sampling_rate = sampling_rate,
                              npeaks = 3))
        warning(paste("Supplying default parameter npeaks = 3 to",
                      "function frequency_domain_summary"))
      } else {
        return(f)
      }
    })
  }

  transform <- list(tidy_sensor_data)
  if (!is.null(time_filter)) {
    transform[[length(transform) + 1]] <-
      purrr::partial(filter_time, t1 = time_filter[[1]], t2 = time_filter[[2]])
  }
  if (detrend) {
    transform[[length(transform) + 1]] <- mutate_detrend
  }
  if (!is.null(frequency_filter)) {
    if (frequency_filter[[2]] > sampling_rate / 2) {
      frequency_filter[[2]] <- sampling_rate / 2
      warning(paste("Reducing the max value of frequency_filter to the",
                    "Nyquist frequency of the input."))
    }
    transform[[length(transform) + 1]] <-
      purrr::partial(mutate_bandpass,
                     window_length = 256,
                     sampling_rate = sampling_rate,
                     frequency_range = frequency_filter)
  }

  if (IMF == 1 && !is.null(window_length) && !is.null(window_overlap)) {
    transform[[length(transform) + 1]] <-
      transformation_window(
        window_length = window_length,
        window_overlap = window_overlap)
  } else if (IMF > 1) {
    if (is.null(window_length)) window_length <- 256
    if (is.null(window_overlap)) window_overlap <- 0.5
    transform[[length(transform) + 1]] <-
      transformation_imf_window(
        window_length = window_length,
        window_overlap = window_overlap,
        max_imf = IMF)
  } else {
    transform[[length(transform) + 1]] <-
      function(sensor_data) {
        if (has_error(sensor_data)) return(sensor_data)
        sensor_data %>% dplyr::select(-t)
      }
  }
  transform[[length(transform) + 1]] <- function(transformed_sensor_data) {
    if (has_error(transformed_sensor_data)) return(transformed_sensor_data)
    transformed_sensor_data <- transformed_sensor_data %>%
      dplyr::rename(!!metric := value)
    return(transformed_sensor_data)
  }
  if (derived_kinematics && metric == "acceleration") {
    mutate_kinematics <- function(transformed_sensor_data) {
      if (has_error(transformed_sensor_data)) return(transformed_sensor_data)
      transformed_sensor_data <- transformed_sensor_data %>%
        mutate_derivative(sampling_rate = sampling_rate,
                          col = "acceleration", derived_col = "jerk") %>%
        mutate_integral(sampling_rate = sampling_rate,
                        col = "acceleration", derived_col = "velocity") %>%
        mutate_integral(sampling_rate = sampling_rate,
                        col = "velocity", derived_col = "displacement") %>%
        mutate_acf(col = "acceleration", lag_max = dplyr::group_size(.))
      return(transformed_sensor_data)
    }
    transform[[length(transform) + 1]] <- mutate_kinematics
    extract_on <- c("acceleration", "jerk", "velocity", "displacement", "acf")
  } else if (derived_kinematics && metric == "velocity") {
    mutate_kinematics <- function(transformed_sensor_data) {
      if (has_error(transformed_sensor_data)) return(transformed_sensor_data)
      transformed_sensor_data <- transformed_sensor_data %>%
        mutate_derivative(sampling_rate = sampling_rate,
                          col = "velocity", derived_col = "acceleration") %>%
        mutate_derivative(sampling_rate = sampling_rate,
                          col = "acceleration", derived_col = "jerk") %>%
        mutate_integral(sampling_rate = sampling_rate,
                        col = "velocity", derived_col = "displacement") %>%
        mutate_acf(col = "velocity", lag_max = dplyr::group_size(.))
      if (is.null(models)) {
        # Don't bother with consistent accelerometer/gyroscope schema
        # if there is no model. Otherwise jerk will be included as an
        # incidental column during feature extraction.
        transformed_sensor_data <- dplyr::select(transformed_sensor_data, -jerk)
      }
      return(transformed_sensor_data)
    }
    transform[[length(transform) + 1]] <- mutate_kinematics
    extract_on <- c("acceleration", "velocity", "displacement", "acf")
  } else {
    extract_on <- metric
  }

  args <- list(transform = transform,
               extract = funs,
               extract_on = extract_on)

  return(args)
}

#' Generate a function for applying a window transformation to sensor data
#'
#' @param window_length Length of the sliding window.
#' @param window_overlap Fraction in the interval [0, 1) specifying the amount of
#' window overlap.
#' @return A function that accepts as input a dataframe with columns
#' t, axis, value and outputs a windowed transformation of that dataframe
transformation_window <- function(window_length, window_overlap) {
  purrr::partial(
    transformation_window <- function(sensor_data, window_length, window_overlap) {
      if (has_error(sensor_data)) return(sensor_data)
      window(sensor_data = sensor_data,
             window_length = window_length,
             window_overlap = window_overlap) %>%
        dplyr::group_by(axis, window)
    },
    window_length = window_length,
    window_overlap = window_overlap)
}

#' Generate a function for windowing sensor data after applying EMD
#'
#' Apply empirical mode decomposition to sensor data, then window the result.
#'
#' @param window_length Length of the sliding window.
#' @param window_overlap Fraction in the interval [0, 1) specifying the amount of
#' window overlap.
#' @param max_imf The maximum number of IMF's during EMD
#' @return A function that accepts as input a dataframe with columns
#' t, axis, value and outputs a windowed transformation of that
#' dataframe's EMD.
transformation_imf_window <- function(window_length, window_overlap, max_imf) {
  purrr::partial(
    function(sensor_data, window_length, window_overlap, max_imf) {
      if (has_error(sensor_data)) return(sensor_data)
      values <- sensor_data %>%
        tidyr::spread(key = "axis", value = "value")
      imf <- purrr::map2(values %>% dplyr::select(t), values %>% dplyr::select(-t),
                         function(t, v) {
                           dplyr::as_tibble(
                             EMD::emd(v, t, max.imf = max_imf)$imf) %>%
                             setNames(1:max_imf)
                         })
      names(imf) <- names(values %>% dplyr::select(-t))
      windowed_imf <- purrr::map_dfr(
        imf, function(df) {
          windowed_imf <- purrr::map_dfr(
            df, function(col) {
              window_signal(col, window_length = window_length,
                            window_overlap = window_overlap) %>%
                dplyr::as_tibble() %>%
                tidyr::gather(key = "window", value = "value", convert = T)
            }, .id = "IMF") %>% dplyr::mutate(IMF = as.integer(IMF))
        }, .id = "axis") %>%
        dplyr::group_by(axis, IMF, window)
      return(windowed_imf)
    },
    window_length = window_length,
    window_overlap = window_overlap,
    max_imf = max_imf)
}

