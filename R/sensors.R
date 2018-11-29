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
#' should return a dataframe of features (normally a single-row datafame).
#' @param extract_on A string or list of column names to compute features from.
#' If \code{NULL}, features will be extracted from all non-grouped columns.
#' @param models A function or list of functions which accept the output from
#' \code{transform} as input and output a dataframe.
#' @return A list of features. The output from \code{extract} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If \code{preprocess} or \code{transform}
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
    purrr::reduce(rev(transform), purrr::compose)()
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
    features$extracted_features <- purrr::map_dfr(
      extract_on,
      ~ extract_features(transformed_sensor_data, ., extract)) %>%
      dplyr::distinct(
        !!!transformed_sensor_data_groups, measurementType, .keep_all = T)
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

#' Extract kinematic sensor features
#' 
#' Extract kinematic (accelerometer/gyroscope) features. This function is not 
#' normally called directly. See \code{accelerometer_features}
#' and \code{gyroscope_features}.
#' 
#' Although this function is primarily a helper function for
#' \code{accelerometer_features} and \code{gyroscope_features}, you
#' may want to use this function rather than \code{sensor_features}
#' if your transformed sensor data will have incidental columns
#' (columns that belong neither to the index nor \code{extract_on})
#' or you would like ACF features.
#' 
#' @param sensor_data A dataframe.
#' @param preprocess A function or list of functions to be applied sequentially
#' to \code{sensor_data}. In practice, there is no difference between \code{preprocess}
#' and \code{transformation}, except the functions in \code{preprocess} are applied
#' before those in \code{transformation}. It is suggested that \code{preprocess}
#' is used to apply mathematical transformations to the data that stay within
#' the original vector space (i.e., the values are indexed by timestamp and
#' axis), and that \code{transformation} is used to apply mathematical transformations
#' that put the data in new vector spaces (e.g., by windowing the data).
#' @param transformation A function or list of functions to be applied sequentially
#' to \code{sensor_data}. See \code{preprocess}.
#' @param extract A function or list of functions to be applied to each of the
#' columns in \code{extract_on} from the output of \code{transform}. Each function
#' should return a dataframe of features (normally a single-row datafame).
#' @param extract_on A string or list of column names to compute features from.
#' If \code{NULL}, features will be extracted from all non-grouped columns.
#' @param models A function or list of functions which accept the output from
#' \code{transform} as input and output a dataframe.
#' @param acf_col Column name to calculate acf upon.
#' @return A list of features. The output from \code{extract} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If applying \code{preprocess}
#' or \code{transformation} returns an error dataframe (see \code{has_error}),
#' the error dataframe is stored under \code{$error}.
kinematic_sensor_features <- function(sensor_data, preprocess = NULL,
                                      transformation = NULL, extract = NULL,
                                      extract_on = NULL, models = NULL,
                                      acf_col = NULL) {
  # TODO what to do with error element within features?
  features <- list(extracted_features = NULL,
                   model_features = NULL,
                   error = NULL)
  
  if (!is.list(preprocess)) {
    preprocess <- list(preprocess)
  }
  if (!is.list(transformation)) {
    transformation <- list(transformation)
  }
  transform <- purrr::flatten(list(preprocess, transformation))
  is_function <- unlist(purrr::map(transform, is.function))
  transform <- transform[is_function]
  if (length(transform) == 0) {
    transform <- list(function(x) x)
  }
  
  transformed_sensor_data <- sensor_data %>%
    purrr::reduce(rev(transform), purrr::compose)()
  if (has_error(transformed_sensor_data)) {
    features$error <- transformed_sensor_data
    return(features)
  }
  
  if (!is.null(extract) && !is.null(extract_on)) {
    transformed_sensor_data_groups <- dplyr::groups(transformed_sensor_data)
    incidental_cols_to_preserve <- transformed_sensor_data %>%
      dplyr::select(-dplyr::one_of(extract_on)) %>%
      dplyr::distinct(!!!transformed_sensor_data_groups, .keep_all = T)
    movement_features <- sensor_features(
      sensor_data = transformed_sensor_data,
      extract = extract,
      extract_on = extract_on)
    acf_features <- sensor_features(
      sensor_data = transformed_sensor_data,
      transform = list(purrr::partial(calculate_acf, col = acf_col)),
      extract = extract,
      extract_on = "acf")
    extracted_features <- list(movement_features, acf_features) %>% 
      purrr::map(function(f) if (!is.null(f$error)) f$error else f$extracted_features)
    features$extracted_features <- extracted_features %>% 
      dplyr::bind_rows() %>%
      dplyr::left_join(incidental_cols_to_preserve) %>%
      dplyr::select(measurementType,
                    dplyr::one_of(names(incidental_cols_to_preserve)),
                    dplyr::everything())
  }
  if (!is.null(models)) {
    model_features <- sensor_features(
      sensor_data = transformed_sensor_data,
      models = models)
    features$model_features <- model_features$model_features
  }
  return(features)
}

#' Get list of default feature extraction functions
#' 
#' @param sampling_rate Sampling rate of the data these functions will be
#' applied to.
#' @return A list of closures for three feature extraction functions supplied
#' with this package that can be passed to the \code{extract} parameter
#' in \code{*_features} functions.
#' @export
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
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing accelerometer measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear acceleration measurements. 
#' @param time_filter A length 2 numeric vector specifying the time range 
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not 
#' filter any measurements.
#' @param detrend Whether to detrend the signal.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length Length of the sliding window used during the windowing 
#' transformation. Both \code{window_length} and \code{window_overlap} must be
#' set for the windowing transformation to be applied.
#' @param window_overlap Window overlap used during the windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics Whether to add columns for \code{jerk}, \code{velocity},
#' and \code{displacement} before extracting features.
#' @param funs A function or list of feature extraction functions that each
#' accept a single numeric vector as input. Each function should return a 
#' dataframe of features (normally a single-row datafame). The input vectors
#' will be the axial measurements from \code{sensor_data} after the chosen
#' preprocessing and transformation steps have been applied. If no argument
#' is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A list of functions, each of which accept as input 
#' \code{sensor_data} after the chosen preprocessing and transformation
#' steps have been applied and return features. Useful for models which compute
#' individual statistics using multiple input variables.
#' @return A list of accelerometer features. The output from \code{funs} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If there is an error during
#' extraction, the returned result will be stored under \code{$error}.
#' @export
accelerometer_features <- function(sensor_data, time_filter = NULL, detrend = F,
                                   frequency_filter = NULL, IMF = 1,
                                   window_length = NULL, window_overlap = NULL,
                                   derived_kinematics = F, funs = NULL,
                                   models = NULL) {
  parameters <- transform_kinematic_sensor_data(
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
  features <- kinematic_sensor_features(
    sensor_data = sensor_data,
    preprocess = parameters$preprocess,
    transformation = parameters$transformation,
    extract = parameters$extract,
    extract_on = parameters$extract_on,
    models = models,
    acf_col = parameters$acf_col)
  return(features)
}

#' Extract gyroscope features
#' 
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing gyroscope measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear velocity measurements. 
#' @param time_filter A length 2 numeric vector specifying the time range 
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not 
#' filter any measurements.
#' @param detrend Whether to detrend the signal.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length Length of the sliding window used during the windowing 
#' transformation. Both \code{window_length} and \code{window_overlap} must be
#' set for the windowing transformation to be applied.
#' @param window_overlap Window overlap used during the windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics Whether to add columns for \code{jerk}, \code{velocity},
#' and \code{displacement} before extracting features.
#' @param funs A function or list of feature extraction functions that each
#' accept a single numeric vector as input. Each function should return a 
#' dataframe of features (normally a single-row datafame). The input vectors
#' will be the axial measurements from \code{sensor_data} after the chosen
#' preprocessing and transformation steps have been applied. If no argument
#' is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A list of functions, each of which accept as input 
#' \code{sensor_data} after the chosen preprocessing and transformation
#' steps have been applied and return features. Useful for models which compute
#' individual statistics using multiple input variables.
#' @return A list of gyroscope features. The output from \code{funs} will
#' be stored under \code{$extracted_features} and the output from \code{models}
#' will be stored under \code{$model_features}. If there is an error during
#' extraction, the returned result will be stored under \code{$error}.
#' @export
gyroscope_features <- function(sensor_data, time_filter = NULL, detrend = F,
                                   frequency_filter = NULL, IMF = 1,
                                   window_length = NULL, window_overlap = NULL,
                                   derived_kinematics = F, funs = NULL,
                                   models = NULL) {
  parameters <- transform_kinematic_sensor_data(
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
  features <- kinematic_sensor_features(
    sensor_data = sensor_data,
    preprocess = parameters$preprocess,
    transformation = parameters$transformation,
    extract = parameters$extract,
    extract_on = parameters$extract_on,
    models = models,
    acf_col = parameters$acf_col)
  return(features)
}

#' Ensure the kinematic sensor arguments are well formed
#' 
#' Verify that the arguments passed to \code{accelerometer_features} and
#' \code{gyroscope_features} are valid.
#' 
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing gyroscope measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear velocity measurements. 
#' @param time_filter A length 2 numeric vector specifying the time range 
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not 
#' filter any measurements.
#' @param detrend Whether to detrend the signal.
#' @param sampling_rate Sampling rate of \code{sensor_data}.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length Length of the sliding window used during the windowing 
#' transformation. Both \code{window_length} and \code{window_overlap} must be
#' set for the windowing transformation to be applied.
#' @param window_overlap Window overlap used during the windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics Whether to add columns for \code{jerk}, \code{velocity},
#' and \code{displacement} before extracting features.
#' @param funs A function or list of feature extraction functions that each
#' accept a single numeric vector as input. Each function should return a 
#' dataframe of features (normally a single-row datafame). The input vectors
#' will be the axial measurements from \code{sensor_data} after the chosen
#' preprocessing and transformation steps have been applied. If no argument
#' is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A list of functions, each of which accept as input 
#' \code{sensor_data} after the chosen preprocessing and transformation
#' steps have been applied and return features. Useful for models which compute
#' individual statistics using multiple input variables.
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
                                    time_filter[2] > time_filter[1],
                                    all(time_filter <= 50))) {
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


#' Return parameters to be used in general feature functions
#' 
#' Using the parameters passed to convenience functions like 
#' \code{accelerometer_features} and \code{gyroscope_features},
#' build a parameter set that can be used with more general functions
#' like \code{kinematic_sensor_data} and \code{sensor_data}. This function
#' is not normally called directly. See \code{accelerometer_features} and
#' \code{gyroscope_features}.
#' 
#' @param sensor_data An \code{n} x 4 data frame with column names \code{t}, \code{x},
#' \code{y}, \code{z} containing gyroscope measurements. Here \code{n} is the
#' total number of measurements, \code{t} is the timestamp of each measurement, and
#' \code{x}, \code{y} and \code{z} are linear velocity measurements. 
#' @param time_filter A length 2 numeric vector specifying the time range 
#' of measurements to use during preprocessing and feature extraction after
#' normalizing the first timestamp to 0. A \code{NULL} value means do not 
#' filter any measurements.
#' @param detrend Whether to detrend the signal.
#' @param frequency_filter A length 2 numeric vector specifying the frequency range
#' of the signal (in hertz) to use during preprocessing and feature extraction.
#' A \code{NULL} value means do not filter frequencies.
#' @param IMF The number of IMFs used during an empirical mode decomposition
#' transformation. The default value of 1 means do not apply EMD to the signal.
#' @param window_length Length of the sliding window used during the windowing 
#' transformation. Both \code{window_length} and \code{window_overlap} must be
#' set for the windowing transformation to be applied.
#' @param window_overlap Window overlap used during the windowing transformation.
#' Both \code{window_length} and \code{window_overlap} must be set for the
#' windowing transformation to be applied.
#' @param derived_kinematics Whether to add columns for \code{jerk}, \code{velocity},
#' and \code{displacement} before extracting features.
#' @param funs A function or list of feature extraction functions that each
#' accept a single numeric vector as input. Each function should return a 
#' dataframe of features (normally a single-row datafame). The input vectors
#' will be the axial measurements from \code{sensor_data} after the chosen
#' preprocessing and transformation steps have been applied. If no argument
#' is supplied to either \code{funs} or \code{models}, a default set
#' of feature extraction functions (as described in \code{default_kinematic_features})
#' will be supplied for this parameter.
#' @param models A list of functions, each of which accept as input 
#' \code{sensor_data} after the chosen preprocessing and transformation
#' steps have been applied and return features. Useful for models which compute
#' individual statistics using multiple input variables.
#' @return A list of parameters to be used in the general feature functions.
transform_kinematic_sensor_data <- function(sensor_data, metric,
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
 
  preprocess <- list(tidy_sensor_data)
  if (!is.null(time_filter)) {
    preprocess[[length(preprocess) + 1]] <- 
      purrr::partial(filter_time, t1 = time_filter[[1]], t2 = time_filter[[2]])
  }
  if (detrend) {
    preprocess[[length(preprocess) + 1]] <- mutate_detrend
  }
  if (!is.null(frequency_filter)) {
    if (frequency_filter[[2]] > sampling_rate / 2) {
      frequency_filter[[2]] <- sampling_rate / 2
      warning(paste("Reducing the max value of frequency_filter to the",
                    "Nyquist frequency of the input."))
    }
    preprocess[[length(preprocess) + 1]] <-
      purrr::partial(mutate_bandpass,
                     window_length = 256,
                     sampling_rate = sampling_rate,
                     frequency_range = frequency_filter)
  }
  
  if (IMF == 1 && !is.null(window_length) && !is.null(window_overlap)) {
    transformation <- list(
      transformation_window(
        window_length = window_length,
        overlap = window_overlap))
  } else if (IMF > 1) {
    if (is.null(window_length)) window_length <- 256
    if (is.null(window_overlap)) window_overlap <- 0.5
    transformation <- list(
      transformation_imf_window(
        window_length = window_length,
        overlap = window_overlap,
        max_imf = IMF))
  } else {
    transformation <- list(
      function(preprocessed_sensor_data) {
        preprocessed_sensor_data %>% dplyr::select(-t)
      })
  }
  transformation[[2]] <- function(transformed_sensor_data) {
    transformed_sensor_data <- transformed_sensor_data %>% 
      dplyr::rename(!!metric := value)
    return(transformed_sensor_data)
  }
  if (derived_kinematics && metric == "acceleration") {
    mutate_kinematics <- function(transformed_sensor_data) {
      transformed_sensor_data <- transformed_sensor_data %>%
        mutate_derivative(sampling_rate = sampling_rate,
                          col = "acceleration", derived_col = "jerk") %>%
        mutate_integral(sampling_rate = sampling_rate,
                        col = "acceleration", derived_col = "velocity") %>%
        mutate_integral(sampling_rate = sampling_rate,
                        col = "velocity", derived_col = "displacement")
      return(transformed_sensor_data)
    }
    transformation[[3]] <- mutate_kinematics
    extract_on <- c("acceleration", "jerk", "velocity", "displacement")
  } else if (derived_kinematics && metric == "velocity") {
    mutate_kinematics <- function(transformed_sensor_data) {
      transformed_sensor_data <- transformed_sensor_data %>%
        mutate_derivative(sampling_rate = sampling_rate,
                          col = "velocity", derived_col = "acceleration") %>%
        mutate_derivative(sampling_rate = sampling_rate,
                          col = "acceleration", derived_col = "jerk") %>%
        mutate_integral(sampling_rate = sampling_rate,
                        col = "velocity", derived_col = "displacement")
      if (is.null(models)) {
        # Don't bother with consistent accelerometer/gyroscope schema
        # if there is no model. Otherwise jerk will be included as an
        # incidental column during feature extraction.
        transformed_sensor_data <- dplyr::select(transformed_sensor_data, -jerk)
      }
      return(transformed_sensor_data)
    }
    transformation[[3]] <- mutate_kinematics
    extract_on <- c("acceleration", "velocity", "displacement")
  } else {
    extract_on <- metric
  } 
  
  parameters <- list(preprocess = preprocess,
                     transformation = transformation,
                     extract = funs,
                     extract_on = extract_on,
                     acf_col = metric)
  
  return(parameters)
}

#' Generate a function for applying a window transformation to sensor data
#' 
#' @param window_length Length of the sliding window
#' @param overlap Window overlap
#' @return A function that accepts as input a dataframe with columns 
#' t, axis, value and outputs a windowed transformation of that dataframe
transformation_window <- function(window_length, overlap) {
  purrr::partial(
    transformation_window <- function(sensor_data, window_length, overlap) {
      if (has_error(sensor_data)) return(sensor_data)
      window(sensor_data = sensor_data,
             window_length = window_length,
             overlap = overlap) %>%
        dplyr::group_by(axis, window)
    },
    window_length = window_length,
    overlap = overlap)
}

#' Generate a function for windowing sensor data after applying EMD
#' 
#' Apply empirical mode decomposition to sensor data, then window the result.
#' 
#' @param window_length Length of the sliding window
#' @param overlap Window overlap
#' @param max_imf The maximum number of IMF's during EMD
#' @return A function that accepts as input a dataframe with columns 
#' t, axis, value and outputs a windowed transformation of that 
#' dataframe's EMD.
transformation_imf_window <- function(window_length, overlap, max_imf) {
  purrr::partial(
    function(sensor_data, window_length, overlap, max_imf) {
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
                           overlap = overlap) %>%
                dplyr::as_tibble() %>%
                tidyr::gather(key = "window", value = "value", convert = T)
            }, .id = "IMF") %>% dplyr::mutate(IMF = as.integer(IMF))
        }, .id = "axis") %>%
        dplyr::group_by(axis, IMF, window)
      return(windowed_imf)
    },
    window_length = window_length,
    overlap = overlap,
    max_imf = max_imf)
}