#' @title Fit a Global Model for a Given Response Variable
#' @param response One of `"log_effort"` (drift trips per day),
#'   `"log_total_cpt"` (total salmon catch per drift trip),
#'   or `"logit_chinook_comp"` (Chinook salmon proportion composition in drift harvest trips).
#' @param formula Character string containing the right-hand-side of the global model.
#' @param fit_data A [`data.frame`][base::data.frame] object storing the variables for regression fitting.
#'   Defaults to an object called `dat`.
#' @return A fitted model object with class [`lm`][stats::lm].
#' @export

fit_global_model = function(response, formula, fit_data = dat) {

  # build the formula based on supplied arguments
  form = formula(paste0(response, " ~ ", formula))

  # fit the model
  fit = lm(form, data = dat, na.action = "na.fail")

  # return the fit
  return(fit)
}

#' @title Fit All Subsets of a Global Model
#' @param global_model A fitted model object with class [`lm`][stats::lm].
#' @param parallel Logical. Should model fitting be performed with parallel processing?
#'   Defaults to `FALSE`.
#' @param reduce_colinearity Logical. Should variables that are known to be colinear be
#'   prevented from being included in the same model?
#'   Defaults to `FALSE`.
#' @param cwt_retain Numeric value between 0 and 1: what cumulative model weight
#'   should be used to trim the set of models?
#'   Defaults to 1 (i.e., no trimming).
#' @note If `parallel = TRUE`, models will be fitted using [MuMIn::pdredge()]
#'   with the number of parallel cores used set to `max(parallel::detectCores() - 1, 1)`.
#'   Otherwise, the models will be fitted using [MuMIn::dredge()].
#'   In either case, the printing of fixed terms (intercept only) is suppressed.
#'   Additionally, regardless of the value of `cwt_retain`, if 5 or fewer models
#'   are in the all allowed subsets of the global model, no trimming will be conducted.
#'   Additionally, if the trimmed model set has fewer than 2 models, no trimming will be conducted.
#' @return List of fitted model objects (each with class [`lm`][stats::lm]).
#' @export

fit_all_subsets = function(global_model, parallel = FALSE, reduce_colinearity = FALSE, cwt_retain = 1) {

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

  # return the list of fitted models
  return(fit_list)
}
