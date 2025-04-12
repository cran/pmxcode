
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxcode <img src="inst/app/www/logo.png" align="right" alt="" width="120" />

The `pmxcode` Shiny app provides a user interface to create or modify
pharmacometric models for various modeling and simulation software
platforms (currently, NONMEM and mrgsolve).

## Installation

You can install the development version of pmxcode like so:

    # Install the lastest release from the CRAN
    install.packages('pmxcode')

    # Or install the development version from GitHub
    # install.packages('devtools')
    devtools::install_github('sbihorel/pmxcode')

## Starting the Shiny app

You can start the Shiny application by using the `run_app` function.

    pmxcode::run_app()

## Main functionality

The `pmxcode` Shiny app provides 2 modules of functionality:

- A code library for a large variety of pharmacokinetic and/or
  pharmacodynamic models
- An interface for the bulk creation of NONMEM control streams for
  univariate covariate testing by forward selection or backward
  elimination

Each module is accessible from the toolbar at the top on the app and is
described in a dedicated article.
