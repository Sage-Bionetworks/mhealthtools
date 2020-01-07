[![Travis-CI Build Status](https://travis-ci.org/Sage-Bionetworks/mhealthtools.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/mhealthtools)

## mhealthtools
An R package for extracting features from mobile and wearable sensor data.

### Description
mhealthtools processes raw data from various mobile and wearable sensors — such as accelerometer, gyroscope, touch screen, and camera — and outputs interpretable feature sets. Included with the package are feature extraction functions designed for activities used in [mHealth applications developed by Sage Bionetworks](http://sagebionetworks.org/digital-health-studies/), as well as general use functions that work with any activity using mobile and wearable sensors.

### Installing

Install the `mhealthtools` package using `devtools`:

```
devtools::install_github("Sage-Bionetworks/mhealthtools")
```

Install the package with the vignettes:
```
devtools::install_github("Sage-Bionetworks/mhealthtools", build_vignettes = TRUE)
```

Alternatively, you can use [this Docker image](https://cloud.docker.com/repository/docker/philsnyder/mhealthtools) — based on `rocker/tidyverse:latest` — which comes with `mhealthtools` preinstalled.

### Known Installation Issues
A common issue on Linux systems when installing the `seewave` dependency is to be missing the system dependencies `libfftw3` and ` libsndfile1`. In a shell, run:

```
$ apt install libfftw3-3 libfftw3-dev libsndfile1 libsndfile1-dev
```

to install these system dependencies, then retry the above `devtools` command.

If you are still having issues installing `seewave`, it may be necessary to also install the `rgl` library.

```
$ apt install r-cran-rgl
```

See the `seewave` [installation page](http://rug.mnhn.fr/seewave/inst.html) for more info.

### Usage

![Modules diagram](paper/figure_one.png)

There are two broad types of modules included with mhealthtools: activity-level and sensor-level modules.

Activity-level modules, like `get_tremor_features` and `get_walk_features`, extract features from every sensor involved in that activity. For example, the two previously mentioned functions both return accelerometer and gyroscope features. These functions are useful for their convenience — we can share parameters since the feature extraction process is nearly identical between accelerometer and gyroscope sensors.

```
get_tremor_features(accelerometer_data, gyroscope_data)
```

Underneath the hood of activity-level modules operate sensor-level modules. These modules are designed to extract features from a single sensor. If you were to design a new activity, you could borrow the already implemented sensor-level modules to extract its features.

```
gyroscope_features(gyroscope_data)
```

Both activity-level and sensor-level modules allow you to include additional steps in the feature extraction pipeline by passing additional arguments to their respective functions. If passing only the input data, a default set of features will be extracted from the raw sensor measurements. But suppose you are working with accelerometer data and would like to detrend, filter frequencies from, and window the axial measurements, then compute measurements for jerk, velocity and displacement — all before extracting features from each measurement.

```
accelerometer_features(
  sensor_data = accelerometer_data,
  detrend = TRUE,
  frequency_filter = c(1, 25),
  window_length = 256, # measured in number of samples
  window_overlap = 0.2,
  derived_kinematics = TRUE)
```

If you omit the `funs` and `models` arguments, a default set of features will be computed. But you can, of course, provide your own feature extraction functions.

```
my_features <- function(x) {
  data.frame(
    "mean" = mean(x),
    "median" = median(x),
    "sd" = sd(x))
}

gyroscope_features(
  sensor_data = gyroscope_data,
  window_length = 256,
  window_overlap = 0.2,
  funs = my_features)
```

At the moment, only accelerometer and gyroscope sensors have this powerful processing pipeline implemented. For sensors such as screen and camera, it is more complicated to provide a useful set of preprocessing and feature extraction functions that generalize well to varied activities. We provide two activity-level functions for these sensors (`get_heartrate` and `get_tapping_features`), but no functionality at the sensor level of granularity — at least not yet.

For more information on how to augment mhealthtools with your own functionality — including not just your own feature extraction functions, but also your own preprocessing/data-cleaning steps — we highly recommend reading the vignettes.

### Contributing
To contribute, [fork](http://help.github.com/fork-a-repo/) the [main repo](https://github.com/Sage-Bionetworks/mHealthTools), branch off a [feature branch](https://www.google.com/search?q=git+feature+branches) from `develop`, [commit](http://git-scm.com/docs/git-commit) and [push](http://git-scm.com/docs/git-push) your changes to your fork and submit a [pull request](http://help.github.com/send-pull-requests/) for `Sage-Bionetworks/mhealthtools:develop`.

### Authors
* Phil Snyder <phil.snyder@sagebase.org> [aut, cre]
* Meghasyam Tummalacherla <meghasyam@sagebase.org> [aut, ctb]
* Thanneer Perumal <thanneer.perumal@sagebase.org> [aut]
* Abhishek Pratap <apratap@sagebase.org> [ctb]
* Elias Chaibub Neto <elias.chaibub.neto@sagebase.org> [ctb]

### License

Apache License
Version 2.0, January 2004
