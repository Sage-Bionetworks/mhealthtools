## mhealthtools
R package for extracting features from mobile sensor data.

### Description
mhealthtools package processes data from various mobile sensors such as accelerometer, gyroscope, pedometer, screen touches, and microphones. Though this package is specifically designed around mHealth applications developed by Sage Bionetwokrs, it works in general with any mobile sensors, provided the data is supplied in a said format.

### Installing

First, install the `mhealthtools` package using `devtools`:

```
devtools::install_github("Sage-Bionetworks/mhealthtools")
```

A common issue when installing the `seewave` dependency is to be missing the system dependencies `libfftw3` and ` libsndfile1`. In a shell, run:

```
$ apt install libfftw3-3 libfftw3-dev libsndfile1 libsndfile1-dev
```

to install these system dependencies, then retry the above `devtools` command.

### Usage


### Next Steps



### Contributing
To contribute, [fork](http://help.github.com/fork-a-repo/) the [main repo](https://github.com/Sage-Bionetworks/mHealthTools), branch off a [feature branch](https://www.google.com/search?q=git+feature+branches) from `master`, make your changes and [commit](http://git-scm.com/docs/git-commit) them, [push](http://git-scm.com/docs/git-push) to your fork and submit a [pull request](http://help.github.com/send-pull-requests/) for `Sage-Bionetworks/mHealthTools:develop`.


### Acknowledgements


### Authors
* Thanneer Perumal <thanneer.perumal@sagebase.org> [aut, cre]
* Phil Snyder <phil.snyder@sagebase.org> [aut, cre]
* Meghasyam Tummalacherla <meghasyam@sagebase.org> [aut, ctb]
* Abhishek Pratap <apratap@sagebase.org> [aut, ctb]
* Elias Chaibub Neto <elias.chaibub.neto@sagebase.org> [ctb]
