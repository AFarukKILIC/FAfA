% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_app.R
\name{run_app}
\alias{run_app}
\title{Run the Shiny Application}
\usage{
run_app(
  lang = "eng",
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
}
\arguments{
\item{lang}{A character string specifying the language of the application. Options are "eng"
(default) for English and "tr" for Turkish.}

\item{onStart}{A function to execute when the app starts.}

\item{options}{A list of options to be passed to \code{shiny::shinyApp}.}

\item{enableBookmarking}{Enable bookmarking in the app.}

\item{uiPattern}{A pattern to be matched for the app's UI.}

\item{...}{Additional arguments to pass to \code{golem_opts}.}
}
\value{
This function does not return a value. It launches the Shiny application interface.
}
\description{
This function launches the Shiny application for performing comprehensive factor analysis.
The application allows users to upload datasets, check assumptions, manipulate data, perform
exploratory and confirmatory factor analysis, and conduct reliability analysis and item weighting.
}
\details{
The application supports multiple languages. By default, it runs in English. However, it can
be switched to Turkish by setting the \code{lang} parameter to "tr".
}
