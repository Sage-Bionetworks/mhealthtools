## mhealthtools
R package for extracting features from mobile sensor data.

### Description
mhealthtools package processes data from various mobile sensors such as accelerometer, gyroscope, pedometer, screen touches, and microphones. Though this package is specifically designed around mHealth applications developed by Sage Bionetworks, it works in general with any mobile sensors, provided the data is supplied in a said format.

### Installing

Install the `mhealthtools` package using `devtools`:

```
devtools::install_github("Sage-Bionetworks/mhealthtools")
```

### Known Installation Issues
A common issue when installing one of the dependency r-package `seewave`  on Linux systems is to be missing the system dependencies `libfftw3` and ` libsndfile1`. In a shell, run:

```
$ apt install libfftw3-3 libfftw3-dev libsndfile1 libsndfile1-dev
```

to install these system dependencies, then retry the above `devtools` command.

If you are still having issues installing `seewave`, it may be necessary to also install the `rgl` library.

```
$ apt install r-cran-rgl
```

See the `seewave` installation page for more info: http://rug.mnhn.fr/seewave/inst.html

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

### License

