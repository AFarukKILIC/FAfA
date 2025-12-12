# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "FAfA",
  pkg_title = "Factor Analysis for All",
  pkg_description = "Provides a comprehensive Shiny-based graphical user interface for conducting a wide range of factor analysis procedures. 'FAfA' (Factor Analysis for All) guides users through data uploading, assumption checking (descriptives, collinearity, multivariate normality, outliers), data wrangling (variable exclusion, data splitting), factor retention analysis (e.g., Parallel Analysis, Hull method, EGA), Exploratory Factor Analysis (EFA) with various rotation and extraction methods, Confirmatory Factor Analysis (CFA) for model testing, Reliability Analysis (e.g., Cronbach's Alpha, McDonald's Omega), Measurement Invariance testing across groups, and item weighting techniques. The application leverages established R packages such as 'lavaan' and 'psych' to perform these analyses, offering an accessible platform for researchers and students. Results are presented in user-friendly tables and plots, with options for downloading outputs.",

  authors = c(
    person(
      given = "Abdullah Faruk",
      family = "KILIC",
      email = "afarukkilic@trakya.edu.tr",
      role = c("aut", "cre")
    ),
    person(
      given = "Ahmet",
      family = "Caliskan",
      email = "ahmetcaliskan@trakya.edu.tr",
      role = "aut"
    )
  ),

  repo_url = "https://github.com/AFarukKILIC/FAfA/tree/master",
  pkg_version = "0.5",
  set_options = TRUE
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("Golem User") # You can set another license here
golem::use_readme_rmd(open = FALSE)

# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
  name = "origin",
  url = "git@github.com:AFarukKILIC/FAfA.git", overwrite = T
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
