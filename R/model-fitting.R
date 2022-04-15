#' @title Fit All Subsets for Multiple Response Variables
#' @param global_formulae A named [`list`][base::list] object with elements for `effort`, `totalcpt`, `chinook_comp`, `chum_comp`, and `sockeye_comp` to be passed to the
#'   `formula` argument of [fit_global_model_one()] separately.
#' @param fit_data A [`data.frame`][base::data.frame] object storing the variables for regression fitting.
#' @param ... Optional arguments passed to [fit_all_subsets_one()]
#' @export

fit_all_subsets = function(global_formulae, fit_data, ...) {

  # specify the response variables to fit
  responses = names(global_formulae)

  # add the transformation
  transform = ifelse(responses %in% c("effort", "total_cpt"), "log",
                     ifelse(responses %in% c("chinook_comp", "chum_comp", "sockeye_comp"), "logit", NA))

  if (any(is.na(transform))) {
    stop ("the names of global formulae must be some of 'effort', 'total_cpt', 'chinook_comp', 'chum_comp', or 'sockeye_comp'")
  }

  transformed_responses = paste(transform, responses, sep = "_")

  # count the responses
  nr = length(responses)

  # fit the global models for all response variables
  global_models = lapply(1:nr, function(i) fit_global_model_one(response = transformed_responses[i], formula = global_formulae[[i]], fit_data = fit_data))

  # obtain all subsets, with options; list elements are response variables
  all_subsets = lapply(1:nr, function(i) fit_all_subsets_one(global_models[[i]], fit_data = fit_data, ...))

  # assign the list names
  names(all_subsets) = responses

  #return it
  return(all_subsets)
}

#' @title Fit a Global Model for a Given Response Variable
#' @param response One of `"log_effort"` (drift trips per day),
#'   `"log_total_cpt"` (total salmon catch per drift trip),
#'   or `"logit_chinook_comp"` (Chinook salmon proportion composition in drift harvest trips).
#' @param formula Character string containing the right-hand-side of the global model.
#' @param fit_data A [`data.frame`][base::data.frame] object storing the variables for regression fitting.
#' @return A fitted model object with class [`lm`][stats::lm].
#' @note FIXME change to allow_collinearity?

fit_global_model_one = function(response, formula, fit_data) {

  # build the formula based on supplied arguments
  form = formula(paste0(response, " ~ ", formula))

  # fit the model
  fit = lm(form, data = fit_data, na.action = "na.fail")

  # return the fit
  return(fit)
}

#' @title Fit All Subsets of a Global Model
#' @param global_model A fitted model object with class [`lm`][stats::lm].
#' @param fit_data A [`data.frame`][base::data.frame] object storing the variables for regression fitting.
#' @param parallel Logical: should model fitting be performed with parallel processing?
#'   Defaults to `FALSE`.
#' @param reduce_colinearity Logical: Should variables that are known to be colinear be
#'   prevented from being included in the same model?
#'   Defaults to `FALSE` (i.e., allows colinear variables).
#' @param cwt_retain Numeric value between 0 and 1: what cumulative model weight
#'   should be used to trim the set of models?
#'   Defaults to 0.75 (i.e., keep models that when sorted in decreasing weight make it in the top 75% by model weight).
#' @note If `parallel = TRUE`, models will be fitted using [MuMIn::pdredge()]
#'   with the number of parallel cores used set to `max(parallel::detectCores() - 1, 1)`.
#'   Otherwise, the models will be fitted using [MuMIn::dredge()].
#'   In either case, the printing of fixed terms (intercept only) is suppressed.
#'   Additionally, regardless of the value of `cwt_retain`, if 5 or fewer models
#'   are in the all allowed subsets of the global model, no trimming will be conducted.
#'   Additionally, if the trimmed model set has fewer than 2 models, no trimming will be conducted.
#' @return List of fitted model objects (each with class [`lm`][stats::lm]).

fit_all_subsets_one = function(global_model, fit_data, parallel = FALSE, reduce_colinearity = FALSE, cwt_retain = 0.75) {

  # create the subset matrix
  vars = colnames(model.matrix(global_model))[-1]
  vars = stringr::str_remove(vars, "TRUE")
  n_vars = length(vars)
  M = matrix(TRUE, n_vars, n_vars)
  dimnames(M) = list(vars, vars)
  M[upper.tri(M)] = NA
  diag(M) = NA

  if (reduce_colinearity) {
    # prevent BTF covariates from being in same model as day or day^2
    if ("total_btf_cpue" %in% vars) M["total_btf_cpue","day"] = FALSE
    if ("chinook_btf_comp" %in% vars) M["chinook_btf_comp","day"] = FALSE
    if (all(c("total_btf_cpue", "I(day^2)") %in% vars)) M["total_btf_cpue","I(day^2)"] = FALSE
    if (all(c("chinook_btf_comp", "I(day^2)") %in% vars)) M["chinook_btf_comp","I(day^2)"] = FALSE

    # prevent BTF spp comp and cpue from being in the same model
    if (all(c("chinook_btf_comp", "total_btf_cpue") %in% vars)) M["chinook_btf_comp","total_btf_cpue"] = FALSE

    # prevent mean_temp and mean_relh from being in the same model
    if (all(c("mean_temp", "mean_relh") %in% vars)) M["mean_relh","mean_temp"] = FALSE

    # prevent mean_relh and precip from being in the same model
    if (all(c("mean_relh", "precip") %in% vars)) M["precip","mean_relh"] = FALSE

    # prevent mean_Nwind and mean_wind from being in the same model
    if (all(c("mean_Nwind", "mean_wind") %in% vars)) M["mean_wind","mean_Nwind"] = FALSE

    # prevent mean_Ewind and mean_wind from being in the same model
    if (all(c("mean_Ewind", "mean_wind") %in% vars)) M["mean_wind","mean_Ewind"] = FALSE

    # prevent mean_wind and mean_gust from being in the same model
    if (all(c("mean_wind", "mean_gust") %in% vars)) M["mean_gust","mean_wind"] = FALSE

    # prevent mean_Nwind and mean_gust from being in the same model
    if (all(c("mean_Nwind", "mean_gust") %in% vars)) M["mean_gust","mean_Nwind"] = FALSE

    # prevent mean_Ewind and mean_gust from being in the same model
    if (all(c("mean_Ewind", "mean_gust") %in% vars)) M["mean_gust","mean_Ewind"] = FALSE

    # prevent not_first_day and hours open from being in the same model
    # these are pretty confounded: all <12hrs are first day, all >12 are second day, only 12hrs are mixed
    if (all(c("not_first_day", "hours_open") %in% vars)) M["hours_open","not_first_day"] = FALSE
  }

  # fit all allowable subsets, depending on whether ran in parallel or not
  if (parallel) {
    my_cluster = parallel::makeCluster(getOption("cl.cores", max(parallel::detectCores() - 1, 1)), type = "PSOCK")
    parallel::clusterExport(my_cluster, as.character(global_model$call)[3])
    parallel::clusterEvalQ(my_cluster, {library("parallel"); library("snow")})
    dredge_out = suppressMessages(MuMIn::pdredge(global_model, cluster = my_cluster, subset = M))
    parallel::stopCluster(my_cluster)
  } else {
    dredge_out = suppressMessages(MuMIn::dredge(global_model, subset = M))
  }

  # drop models that have day^2 but not day
  if("I(day^2)" %in% vars) {
    dredge_out = subset(dredge_out, dc("day", "I(day^2)"))
  }

  # trim the model set to only those within the top X% of all model weight
  # but only do this calculation if a subset comprised of at least two models is available.
  # e.g., if there are 3 models assessed and their weights are: 0.85, 0.14, and 0.01
  # keep all three models
  # also, keep all models if the full set is comprised of 5 or fewer models
  if (!any(cumsum(dredge_out$weight) <= cwt_retain) | nrow(dredge_out) <= 5) {
    dredge_out_reduced = dredge_out
  } else {
    dredge_out_reduced = subset(dredge_out, cumsum(weight) <= cwt_retain)
  }

  # refit all models
  fit_list = MuMIn::get.models(dredge_out_reduced, subset = TRUE)

  # assign it attributes
  attributes(fit_list)$subset_params = list(reduce_colinearity = reduce_colinearity, cwt_retain = cwt_retain)

  # return the list of fitted models
  return(fit_list)
}

#' @title Obtain Model-Averaged Predicted Value
#'
#' @param fit_list List of fitted model objects of class [`lm`][stats::lm].
#' @param newdata Data frame storing the variable values at which to obtain model-averaged predictions.
#' @return Numeric vector of length equal to `nrow(newdata)`.
#' @export

predict_model_avg = function(fit_list, newdata) {

  # obtain predicted values at covariate values found in new data from each model
  preds = lapply(fit_list, function(fit) unname(KuskoHarvPred:::inverse_transform(fit)(predict(fit, newdata = newdata))))

  # obtain model weights
  wts = KuskoHarvPred:::get_wt(unlist(lapply(fit_list, MuMIn::AICc)))

  # simpler if only one predicted value per model
  if (nrow(newdata) == 1) {
    # calculate model-averaged predicted value
    out = sum(unlist(preds) * wts)
  } else {
    # combine model-specific predicted vectors as columns in a matrix
    preds = do.call(cbind, preds)

    # calculate model-averaged predicted values
    out = apply(preds, 1, function(i) sum(i * wts))
  }

  # return the model-averaged prediction
  return(out)
}
