% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-fitting.R
\name{fit_global_model_one}
\alias{fit_global_model_one}
\title{Fit a Global Model for a Given Response Variable}
\usage{
fit_global_model_one(response, formula, fit_data)
}
\arguments{
\item{response}{One of \code{"log_effort"} (drift trips per day),
\code{"log_total_cpt"} (total salmon catch per drift trip),
or \code{"logit_chinook_comp"} (Chinook salmon proportion composition in drift harvest trips).}

\item{formula}{Character string containing the right-hand-side of the global model.}

\item{fit_data}{A \code{\link[base:data.frame]{data.frame}} object storing the variables for regression fitting.}
}
\value{
A fitted model object with class \code{\link[stats:lm]{lm}}.
}
\description{
Fit a Global Model for a Given Response Variable
}
\note{
FIXME change to allow_collinearity?
}
