# deg2

An R package for approximate latitudinal semi-corrected areas from raster files in degrees.

## Description

deg2 is an R package intended to calculate areas from pixels meeting certain criteria, considering the latitudinal variation in the relationship degree-distance. Areas are considered semi-corrected because despite the latitudinal correction, the procedure assumes pixels as perfect squares in planar space.

*Note: Currently, this is a BETA version and may contain errors. Use it at your own risk!*

## Getting started

### Dependencies
* RStudio with R (4.0.0 or higher)
* dplyr, raster

### Installing

* Download the devtools package, from the RStudio "Install Packages" menu or using:
```
> install.packages("devtools")
> library(devtools)
```
* Download and install the deg2 package using:
```
> install_github("VR-Daniel/deg2")
> library(deg2)
```

## Commands

* Calculate semi-corrected areas
```
> deg2area
```
* Calculate the equivalence between degrees and arcmin/arcsec, and vice versa
```
> deg2arc
```

## Authors

* Juan D. Vásquez-Restrepo ([@microteiido](https://twitter.com/microteiido))

## Version history
* 1.1.0
    * An option for calculating semi-corrected areas based on squared or trapezoidal "cells" was added (see deg2area function help)
* 1.0.0
    * Initial release

## License

This project is licensed under CC BY-NC-SA 4.0.