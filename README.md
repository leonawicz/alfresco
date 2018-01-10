
<!-- README.md is generated from README.Rmd. Please edit that file -->
alfresco
========

[![Travis build status](https://travis-ci.org/leonawicz/alfresco.svg?branch=master)](https://travis-ci.org/leonawicz/alfresco) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/alfresco?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/alfresco) [![Coverage status](https://codecov.io/gh/leonawicz/alfresco/branch/master/graph/badge.svg)](https://codecov.io/github/leonawicz/alfresco?branch=master)

R is used to manipulate and analyze data that both feed into and come out of the ALFRESCO wildfire model. The `alfresco` R package contains functions for working with ALFRESCO input and output data. It is a core functional package in the [SNAPverse](https://leonawicz.github.io/snapverse/) collection of R packages.

Features
--------

The `alfresco` package includes functions that assist with the following analyses and processing pipelines:

-   Fire events processing immediately following ALFRESCO runs
-   Post-ALFRESCO run, automated Shiny app creation.
-   Hierarchical data extractions from ALFRESCO run output geotiff map sets.
-   Calculations for burn area ratios across multiple ALFRESCO run scenarios.
-   Copying and organizing subsets of ALFRESCO outputs for consecutive model runs and in preparation for other analyses.
-   Generating fire management options ratios geotiffs and related graphics.
-   Generation of flexible bash and slurm scripts for processing catering to unique analysis and data processing needs.

This is a subset of eventual functionality. The package is under active development.

Installation
------------

You can install alfresco from github with:

``` r
# install.packages('devtoools')
devtools::install_github("leonawicz/alfresco")
```

Reference
---------

[Complete package reference and function documentation](https://leonawicz.github.io/alfresco/)
