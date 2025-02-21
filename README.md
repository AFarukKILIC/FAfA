
# FAfA: Factor Analysis for All

<!-- badges: start -->
<!-- badges: end -->

This Shiny application offers researchers a comprehensive tool for performing factor analysis. 
Users can upload datasets, validate assumptions, manage missing and outlier data, split data for different analyses, and run exploratory and confirmatory factor analyses (EFA and CFA). 
The software also offers reliability analysis, exploratory graph analysis, and item weighting. 
With a user-friendly interface, this tool simplifies the EFA and CFA processes. 
The main features are data submission and simple data inspection. Data manipulation (excluding variables, splitting data, checking for outliers), assumption checking (Tabachnik & Fidell (2012) <ISBN:978-0-205-84957-4> and Field (2009) <ISBN:978-1-84787-906-6>) for factor analysis, exploratory factor analysis (with various factor number determination methods (Lorenzo-Seva & Ferrando (2021) <doi:10.5964/meth.7185>)), confirmatory factor analysis (model definition and modification suggestions (Kline (2011) <ISBN:978-1-60623-877-6>)), reliability analysis (Cronbach's alpha, McDonald's omega, Armor's theta, structural reliability, stratified alpha), item weighting (Kilic & Dogan (2019) <doi:10.21031/epod.516057>).


## Installation

You can install the development version of FAfA like so:

``` r
install.packages("devtools")
devtools::install_github("AFarukKILIC/FAfA")
```

## Example


``` r
library(FAfA)
run_app(lang = "eng")
#you can choose Turkish as
run_app(lang = "tr")
```

