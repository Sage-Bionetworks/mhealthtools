---
title: 'mhealthtools: A Modular R Package for Extracting Features from Mobile and Wearable Sensor Data'
tags:
  - R
  - mhealth
  - mobile health
  - digital health
  - ehealth
  - accelerometer
  - feature extraction
  - gait
  - heart rate
  - tapping
  - tremor
  - parkinsons
authors:
  - name: Phil Snyder
    orcid: 0000-0002-6967-2185
    affiliation: 1
  - name: Meghasyam Tummalacherla
    orcid: 0000-0002-0741-8683
    affiliation: 1
  - name: Thanneer Perumal
    orcid: 0000-0003-1168-8982
    affiliation: 1
  - name: Larsson Omberg
    orcid: 0000-0002-4719-9120
    affiliation: 1
affiliations:
  - name: Sage Bionetworks
    index: 1
date: 6 January 2020
bibliography: paper.bib
---

# Introduction

As life expectancy continues to rise with the increasing prevalence of modern medical practices, the costs associated with caring for an increasingly large elderly population have climbed as well. In response, there has been an evolving interest in using relatively inexpensive remote monitoring methods outside of the clinic to aid in the early detection and real-time assessment of Alzheimer's disease [@kourtis_2019], Parkinson's disease [@monje_2019], mood disorders [@jacobson_2019], and a wide array of other diseases [@majumder_deen_2019]. Consumer-grade sensors embedded in smartphones and wearables have the potential to provide clinicians with remote _digital biomarkers_ that can aid in the early detection and monitoring of disease via frequent, objective, and unobtrusive assessments [@coravos_2019]. The extraction of relevant digital biomarkers from raw sensor data requires a combination of signal processing, data science and biological expertise in addition to extensive validation data [@goldsack_chasse_wood_2019; @monje_2019]. Software libraries to preprocess and extract features from such data are becoming increasingly numerous and varied [@saez-pons_2019; @czech_2019; @paul_van_gent_2019]. But without an established set of preprocessing techniques and features for capturing disease signal, it becomes necessary to make use of a broad spectrum of methods and tools when exploring potential biomarkers [@kumar_nilsen_2013]. To this end we have developed mhealthtools: an R package that aids in the construction of data pipelines for remote sensor data analysis.

mhealthtools is an open-source, modular R package for preprocessing and extracting features from remote sensor data collected using mobile and wearable devices. The package is written in a functional style so that various components of the preprocessing and feature extraction pipeline can be inserted, omitted, moved around, extended, and shared between multiple libraries with minimal overhead. Depending on the data source, the package includes a default set of preprocessing and feature extraction behavior for convenient exploratory analysis. The inertial measurement unit (IMU) (e.g., accelerometer and gyroscope) features extracted using this package have been used by  @perumal_2018 and the tapping features by @neto_2019, both using data from the mPower study [@bot_2016]. The heart rate features have been used as part of a validation study [@omberg_2020].

# Design

Data collection from sensors embedded in wearables and mobile devices can occur through active tasks, where participants perform a specified task according to a protocol, or passive tasks, where sensor measurements are collected discreetly in the background during daily living conditions. Features computed from these measurements are typically used as the basis of statistical models designed to explore potential patient outcomes or measures of disease. mhealthtools provides functionality to construct processing pipelines of raw data sourced from sensor measurements recorded during these tasks. This includes data transformations, filtering and feature extraction. In addition to these generalizable pipeline-building functions, mhealthtools provides a set of curated, high level functions that compute features using already constructed pipelines designed for specific active tasks.

In the case of accelerometer and gyroscope sensors, mhealthtools provides functions (`accelerometer_features` and `gyroscope_features`) for extracting general features — agnostic of whether data is being collected as part of an active or passive task. Higher level functions, `get_heartrate`, `get_tapping_features`, `get_walk_features` and `get_tremor_features` are designed to compute features for specific active tasks that use the camera, touch screen sensors, and the underlying `accelerometer_features` and `gyroscope_features`, respectively. Unlike accelerometer and gyroscope, touch screen and camera sensors do not have their own task agnostic feature extraction functions. This hierarchical organization of modules is illustrated in the figure below.

![The heirarchical organization of mhealthtools. Note that touch screen and camera sensors do not have their own task agnostic feature extraction functions.](figure_one.png)

All modules, whether they operate at the activity or sensor level, provide some level of parameterization to modify the default feature extraction pipeline. This concept is best illustrated by an activity-level module which makes use of both the accelerometer and gyroscope feature extraction functions. For example, `get_tremor_features` is sufficiently straightforward to compute features when provided with only the input data as a dataframe in a standardized format — while being flexible enough to accept an assemblage of parameters that control signal detrending, frequency filtering, windowing, which feature sets to compute and more. Additionally, it’s possible to modify the order in which certain preprocessing steps are applied or to include external functions within the pipeline.

This flexibility in parameterization is an intentional design choice of the mhealthtools package, made possible by adhering to a consistent functional paradigm. The user is able to easily mix and match components of pipelines which may be designed for extracting features from different activities without needing to worry about potentially inconsistent output formats from those components.

![An expanded view of the hierarchical and modular organization of mhealthtools. `get_tremor_features` calls both `accelerometer_features` and `gyroscope_features`, which in turn call a sequence of functions that preprocesses (transforms) then extracts features from the inputted raw sensor data. Each of the components above can be internally modified, extended, or omitted altogether, depending on the desired output.](figure_two.png)

In the case of the IMU sensors, the paradigm is as follows:

* Input — raw sensor data, in a standardized format.
* Transform — raw sensor data (by, e.g., tidying [@hadley_2014], computing rates of change, windowing).
* Extract — features by computing statistics upon individual columns, typically grouped on an index such as axis or axis/window.
* Return — features for each group.

The _transform_ and _extract_ steps may be modified relatively easily, allowing functions from external libraries to be included as part of the pipeline without needing to modify the source code of the package. In addition, to handle the processing of large datasets, mhealthtools provides a robust error-handling mechanism in the case of corrupted input data or other data processing issues — guaranteeing a consistent output.

For more information, including how to use custom preprocessing functions as part of the _transform_ step and how to include your own features or machine learning models within the feature extraction pipeline, see the package vignette _Extending mhealthtools_.

# Features

Default feature sets are provided for both time and frequency domains. For a full list of features and their definitions, see the _Feature Definitions_ vignette.

# Comparison with Related Packages
There exist a number of other packages that were built to work with data collected from sensors in a digital health research setting. HeartPy [@paul_van_gent_2019] is a Python package focused on extracting features from PPG data collected through PPG or camera sensors. GaitPy [@czech_2019] is a Python package designed to extract gait features collected by an accelerometer sensor on the lower back. It includes functionality to identify gait bouts (for measurements collected during free-living conditions), implementations of a number of published algorithms for deriving gait features, and a convenience function to visualize gait events. PDkit [@saez-pons_2019] is the package with the most overlap in functionality with mhealthtools. Focused on Parkinson's disease, PDkit aims to provide a broad suite of tools, ranging from convenience functions for loading data from popular Parkinson's digital health datasets, feature extraction on measurements collected during commonly performed tasks in Parkinson's studies, and even functions which attempt to map sensor measurements to Unified Parkinson Disease Rating Scale (UPDRS) Part III scores, a clinically validated metric for measuring Parkinsons's disease progression.

In comparison to these packages, mhealthtools is focused solely on the feature extraction process and tries to offer a transparent and generalizable framework that makes minimal assumptions about how the sensor data was collected and how it should be processed. This is reflected in a function design that caters to both ease of use and extensibility. mhealthtools adopts robust, functional programming practices, such as predictable error handling, function interfaces, and output formats, making the package a good fit for ETL workloads and streaming data. The default features for IMU sensors were chosen to work well in both clinical and free-living contexts, but are otherwise signal agnostic and not chosen with one particular phenotype or activity type in mind (task specific feature extraction functions, such as `get_walk_features`, may make use of default argument values to these otherwise task agnostic feature sets). For these reasons, mhealthtools is useful for both quickly generating feature sets for exploratory analysis as well as coordinating the extraction of features that may be implemented across multiple packages for more advanced data pipelines.

# References