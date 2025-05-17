
# FAfA: Factor Analysis for All

<!-- badges: start -->
<!-- badges: end -->

Provides a comprehensive Shiny-based graphical user interface for conducting
    a wide range of factor analysis procedures. 'FAfA' (Factor Analysis for All)
    guides users through data uploading, assumption checking (descriptives,
    collinearity, multivariate normality, outliers), data wrangling (variable
    exclusion, data splitting), factor retention analysis (e.g., Parallel Analysis,
    Hull method, EGA), Exploratory Factor Analysis (EFA) with various rotation
    and extraction methods, Confirmatory Factor Analysis (CFA) for model testing,
    Reliability Analysis (e.g., Cronbach's Alpha, McDonald's Omega), Measurement
    Invariance testing across groups, and item weighting techniques. Results are presented in user-friendly tables and plots, with options for
    downloading outputs.


## Installation

You can install the development version of FAfA like so:

``` r
install.packages("devtools")
devtools::install_github("AFarukKILIC/FAfA")
```

## Example


``` r
library(FAfA)
run_app()
```

