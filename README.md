Colonial history and global economics distort our understanding of
deep-time biodiversity
================

-   [Description](#description)
-   [Requirements](#requirements)
-   [Setup](#setup)
-   [Scripts](#scripts)
-   [Troubleshooting](#troubleshooting)

[![](https://img.shields.io/badge/doi-10.1038/s41559--021--01608--8-orange.svg)](https://doi.org/10.1038/s41559-021-01608-8)
[![](https://img.shields.io/github/languages/code-size/paleoscientometrics/paleo-imperialism.svg)](https://github.com/paleoscientometrics/paleo-imperialism)

*Nussaïbah B. Raja, Emma M. Dunne, Aviwe Matiwane, Tasnuva Ming Khan,
Paulina S. Nätscher, Aline M. Ghilardi, Devapriya Chattopadhyay*

## Description

This repository contains all the **data** (in `/data`) and **R scripts**
(in `/scripts`) necessary to examine how socio-economic factors and
colonial history influences paleontological research output. The outputs
of the scripts are provided in the `/output` and figures in the `/figs`
folder. Please cite the study as:

Raja, N. B., Dunne, E. M., Matiwane, A., Khan, T. M., Nätscher, P. S.,
Ghilardi, A. M., and Chattopad-hyay, D. (2021). Colonial history and
global economics distort our understanding of deep-time
biodiversity.Nature Ecology &
Evolution.https://doi.org/10.1038/s41559-021-01608-8

## Requirements

This code was developed in `R 4.0.0`. It is therefore recommended to use
the same or any more up-to-date version of R for reproducing the
analyses in this study.

## Setup

You will need to either use the Rstudio project environment or set your
working directory to the root of this folder.

To install all required depdendencies (packages), run:

``` r
source(file.path("inst","dependencies")
```

## Scripts

The `scripts/` folder contains all the code generated for the above
mentioned study.

-   **00-prepare_data.R:** This script prepares the data to be used in
    following analyses.

-   **01-prop_per_aff_country:** This script computes the contribution
    to fossil data per country, including domestic and foreign research.

-   **02-network parachute.R:** This script is used for global network
    analyses and to calculate the parachute index per country.

-   **03-model.R:** This script is used for the path analysis to
    represent the relationship between the socio-economic factors and
    research output.

## Troubleshooting

The issue tracker is the preferred channel for bug reports. You may also
contact me [by email](mailto:nussaibah.raja.schoob@fau.de).
