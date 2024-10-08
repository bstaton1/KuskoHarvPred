% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-fitting.R
\name{fit_all_subsets_one}
\alias{fit_all_subsets_one}
\title{Fit All Subsets of a Global Model}
\usage{
fit_all_subsets_one(
  global_model,
  fit_data,
  parallel = FALSE,
  reduce_colinearity = FALSE,
  cwt_retain = 1
)
}
\arguments{
\item{global_model}{A fitted model object with class \code{\link[stats:lm]{lm}}.}

\item{fit_data}{\code{\link[base:data.frame]{data.frame}} storing the variables for regression fitting.}

\item{parallel}{Logical: should model fitting be performed with parallel processing?
Defaults to \code{FALSE}.}

\item{reduce_colinearity}{Logical: Should variables that are known to be colinear be
prevented from being included in the same model?
Defaults to \code{FALSE} (i.e., allows colinear variables).}

\item{cwt_retain}{Numeric value between 0 and 1: what cumulative model weight
should be used to trim the set of models?
Defaults to 1 (i.e., keep models all models).}
}
\value{
A \code{\link[base:list]{list}} of fitted model objects (each with class \code{\link[stats:lm]{lm}}).
}
\description{
Fit All Subsets of a Global Model
}
\note{
If \code{parallel = TRUE}, models will be fitted using \code{\link[MuMIn:pdredge]{MuMIn::pdredge()}}
with the number of parallel cores used set to \code{max(parallel::detectCores() - 1, 1)}.
Otherwise, the models will be fitted using \code{\link[MuMIn:dredge]{MuMIn::dredge()}}.
In either case, the printing of fixed terms (intercept only) is suppressed.
Additionally, regardless of the value of \code{cwt_retain}, if 5 or fewer models
are in the all allowed subsets of the global model, no trimming will be conducted.
Additionally, if the trimmed model set has fewer than 2 models, no trimming will be conducted.
}
