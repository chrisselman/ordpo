# Simulating trials with different PO models

This repository contains the scripts used in developing a Bayesian adaptive perinatal platform trial.

# Contributors

- Robert Mahar, Melbourne School of Population and Global Health

# Overview

The directory structure follows a template described by Marwick, Boettiger & Mullen (2018) Packaging Data Analytical Work Reproducibly Using R (and Friends), The American Statistician. Note that this template is effectively an R package with an additional `analysis` subdirectory containing an `analysis` subdirectory where all analysis scripts (that are not major `R` functions). 

# Requirements

- Installation of `R`
- Installation of `stan` and `rstan`
- R libraries as specified in `DESCRIPTION`
- Access to the a High Performance Cluster (for efficient simulation) 
- If using a cluster, a user-created `.Renviron` containing the environment variable `PATH_TO_PKG_ON_CLUSTER` which defines the file path to the package on the users cluster: e.g. PATH_TO_PKG_ON_CLUSTER = "/home/[yourdirectory]/platipus"
- Upon opening the `Rproj` file, call `devtools::load_all()` to load the package.

# Stan files

The stan files are adapted from Ben Goodrich's code located at `https://github.com/bgoodri/covid19_Bayesian_RCT`. 

- `po_goodrich` corresponds to `lrmqr`
- `ppo_goodrich` corresponds to `lmrqrcppo` but with the clustering aspect removed by Rob.


The user must run `devtools::run_all()` at the start of each session to load the package and documentation. 

# General style guide

In an effort to maintain some form of consistency, we ask that contributions adhere to Hadley Wickham's Style Guide for `R`. Directories, script names, local objects are in `snake_case`, including acronuyms. Global objects are in `SNAKE_CASE`. Functions should endeavour to 

# Directory Structure

## `README.md`

The information you are currently reading in `markdown` format. Tracked.

## `.gitignore`

A text file specifying files and folders that aren't to be tracked. This is required so that when the project is pushed to an online repository no large files or sensitive data are pushed along with it. Tracked.

## `.Renviron`

A text file containing `R` global environment variables that are loaded upon opening the package (e.g. the location of a data repository on the users local network). Untracked.

## `analysis`

### `input`

Contains non-sensitive `inputs`, other than the raw data. Not tracked.

### `output`

Contains reproducible output from the analysis including intermediate data files, figures, and tables. Not tracked.

### `scripts`

Contains scripts and models to perform the analyses within named subdirectories (e.g. `R`, `pbs`). Tracked.

## `R`

Contains major functions used for the analysis. All functions must be documented using the Roxygen documentation. Tracked.

## `stan`

Contains stan models used for the analysis. Tracked.

# Workflow 

[To be drafted].

# End of document. 
