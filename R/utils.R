#' @title Obtain the Name of the Response Variable for a Given Model
#'
#' @param fit A fitted model object with class [`lm`][stats::lm] or `"averaging"` (created via [MuMIn::model.avg()]).
#' @return One of:
#'   * `"effort"` (number of drift trips),
#'   * `"total_cpt"` (total salmon catch per trip), or
#'   * `"chinook_comp"` (Chinook salmon proportion composition)
#' @note Although the models are fitted to response variables
#'   on a transformed scale (`"log_effort"`, `"log_total_cpt"`, `"logit_chinook_comp"`),
#'   the name of the non-transformed variable is returned.

get_response = function(fit) {

  # extract the name of the response variable if fitted model is of class "lm"
  if (class(fit) == "lm") {
    response = as.character(formula(fit$call))[2]
  }

  # extract the name of the response variable if fitted model is of class "averaging"
  if (class(fit) == "averaging") {
    response = as.character(fit$formula)[2]
  }

  # remove the scale portion of the variable
  response = stringr::str_remove(response, "^log_|^logit_")

  # return the name of the response variable
  return(response)
}

#' @title Obtain the Right-Hand-Side of a Given Model
#'
#' @param fit A fitted model object with class [`lm`][stats::lm].
#' @return A character string with the right-hand-side of the
#'   model formula. If the model has the intercept only,
#'   `"Intercept-only"` will be returned rather than `"1"`.

get_formula = function(fit) {

  # extract the formula used to fit the model
  form = formula(fit$call)

  # extract only the right-hand-side as a character string
  rhs = as.character(form)[3]

  # remove the intercept term (" + 1")
  rhs = stringr::str_remove(rhs, "\\s\\+\\s1")

  # if the model is intercept-only, return this
  rhs = ifelse(rhs == "1", "Intercept only", rhs)

  # return the right-hand-side
  return(rhs)
}

#' @title Obtain Function to Return the Inverse of the Scale of the Response Variable
#'
#' @param fit A fitted model object with class [`lm`][stats::lm]
#' @return A function representing the correct inverse function
#'   to back-transform the response variable. E.g., if the response
#'   is either `"effort"` or `"total_cpt`", will return [base::exp()] and
#'   if the response is `"chinook_comp"`, will return [stats::plogis()].

inverse_transform = function(fit) {

  # extract name of response variable
  response = get_response(fit)

  # use the right inverse link function depending on the response variable
  if (response %in% c("effort", "total_cpt")) fn = exp
  if (response %in% c("chinook_comp", "chum_comp", "sockeye_comp")) fn = plogis

  # return the function
  return(fn)
}

#' @title Obtain Model Weights given a vector of AIC(c) Values
#' @param AIC Numeric vector of AIC(c) values from multiple models.
#' @return A numeric vector model weights.

get_wt = function(AIC) {
  delta = AIC - min(AIC)
  exp(-0.5 * delta)/sum(exp(-0.5 * delta))
}

#' @title Count the Number of Parameters in a Model
#' @param fit A fitted model object with class [`lm`][stats::lm].
#' @return The number of coefficients in the fitted model plus 1 for the residual standard error.

count_params = function(fit) {
  length(coef(fit)) + 1
}

#' @title Produce a Concise AICc Table
#' @param fit_list List of fitted model objects of class [`lm`][stats::lm]
#' @param digits Numeric value controlling the number of decimal places to round to.
#'   Passed to [base::round()] for `"delta"` and [KuskoHarvEst::smart_round()] for model weights.
#'   Defaults to 3.
#' @return Data frame with columns:
#'   * `terms`: the right-hand-side of each fitted model formula; from [get_formula()].
#'   * `K`: the number of parameters in each fitted model; from [count_params()].
#'   * `delta`: the difference in AICc scores between each model and the lowest AICc model.
#'   * `wt`: the model weight; from [get_wt()].

AIC_table = function(fit_list, digits = 3) {

  # get AICc from each model
  AICc = sapply(fit_list, MuMIn::AICc)

  # build the table
  tab = data.frame(
    terms = sapply(fit_list, get_formula),
    K = sapply(fit_list, count_params),
    delta = AICc - min(AICc),
    wt = get_wt(AICc)
  )

  # if rounding, do so for relevant variables
  if (!is.null(digits)) {
    tab$delta = round(tab$delta, digits = digits)
    tab$wt = KuskoHarvEst:::smart_round(tab$wt, digits = digits)
  }

  # return the output table
  return(tab)
}

#' Find Predictor Variable Names in a Set of Fitted Models
#'
#' @param fit_list List of fitted model objects of class [`lm`][stats::lm]
#'

find_variables = function(fit_list) {
  vars = unique(unlist(lapply(fit_list, function(m) unlist(strsplit(KuskoHarvPred:::get_formula(m), fixed = TRUE, split = " + ")))))
  if (any(vars == "Intercept only")) vars = vars[-which(vars == "Intercept only")]
  vars
}

#' Start a Shiny Server for the Prediction Tool and Render It
#'
#' @export

run_predictive_tool = function() {
  pkg_dir = system.file(package = "KuskoHarvPred")
  tool_dir = file.path(pkg_dir, "rstudio", "KuskoHarvPred-tool")
  rmarkdown::run(file = file.path(tool_dir, "index.Rmd"), auto_reload = FALSE, render_args = list(quiet = TRUE))
}

#' Extract a Period- and Variable-specific MAPE value
#'
#' @param response Character; one of `"effort"`, `"total_cpt"`, `"chinook_comp"`, `"chum_comp"`, `"sockeye_comp"`, `"chinook_harv"`, `"chum_harv"`, or `"sockeye_harv"`
#' @param period Numeric; one of `1`, `2`, or `3`.
#' @note Queries the `KuskoHarvPred:::loo_output$error_summary` table
#'   for the correct value. The value is on the proportional error scale.

get_mape = function(response, period) {

  # extract LOO errors
  errors = KuskoHarvPred:::loo_output$error_summary

  # extract only the specific MAPE of interest and convert to a proportion
  errors[errors$response == response,paste0("MAPE_", period)]
}
