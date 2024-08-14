#' @title Obtain Leave-one-Out Predictions from a Single Model
#'
#' @param fit A fitted model object of class [`lm`][stats::lm].
#' @return A [`matrix`][base::matrix] object with columns storing values for each record and rows storing the predicted value for
#'   the left out observation and the AICc value (given by [MuMIn::AICc()]) of the model fitted after leaving the data point out.

loo_pred = function(fit) {

  # loop through observations leaving one out at a time
  # fit model to training data, predict value of test data
  sapply(1:nrow(fit$model), function(i) {
    # build training data set (with one observation left out)
    train_dat = as.data.frame(fit$model[-i,]); colnames(train_dat) = colnames(fit$model)

    # build the test data set (with only the left out observation)
    test_dat = as.data.frame(fit$model[i,]); colnames(test_dat) = colnames(fit$model)

    # fit the model to training data set
    loo_fit = lm(formula(fit), data = train_dat)

    # use this model to predict the response value of the test data set
    loo_pred = unname(inverse_transform(fit)(predict(loo_fit, newdata = test_dat)))

    # return the prediction and AICc for this left out sample
    c(loo_pred = loo_pred,
      AICc = MuMIn::AICc(loo_fit)
    )
  })
}

#' @title Obtain Model-Averaged Leave-one-Out Predictions
#'
#' @inheritParams predict_model_avg
#' @return Numeric vector of model-averaged leave-one-out predictions.
#' @note Leave-one-out predictions are made for each model and each record
#'   using [loo_pred()] and model weights are recalculated for each left out record
#'   prior to averaging the predictions from each model.
#' @export

loo_pred_model_avg = function(fit_list) {

  # make loo predictions and calculate AICc for each loo for each model
  loo = lapply(fit_list, loo_pred)

  # extract/format the loo predictions
  loo_preds = do.call(cbind, lapply(loo, function(x) x["loo_pred",]))

  # extract/format the loo predictions
  loo_AICcs = do.call(cbind, lapply(loo, function(x) x["AICc",]))

  # calculate AICc weight for each loo
  loo_wts = t(apply(loo_AICcs, 1, get_wt))

  # calculate model-averaged loo prediction
  loo_pred_model_avg = sapply(1:nrow(loo_preds), function(i) sum(loo_preds[i,] * loo_wts[i,]))

  # return
  return(loo_pred_model_avg)
}

#' @title Conduct the Entire Leave-one-Out Analysis
#' @description A wrapper around model-fitting, model-averaging, leave-one-out calculations, and error summaries
#'   for effort, catch rate, and species composition; also returns predictions and error summaries of harvest implied by the values of the other variables.
#' @inheritParams fit_all_subsets
#' @param var_desc Optional character string describing the variables used in fitting.
#'   If supplied, will become the first column in the `error_summary` element of the output list.
#' @param error_types A character vector specifying which types of error summaries (obtained by [KuskoHarvUtils::get_errors()]) to return.
#'   Options are any combination of `"RHO"`, `"RMSE"`, `"ME"`, `"MAE"`, `"MPE"`, or `"MAPE"`.
#' @param ... Optional arguments passed to [fit_all_subsets()]
#' @details This function conducts several steps in a self-contained wrapper:
#'   1. Fits the global models to each response variable using [fit_global_model_one()].
#'   2. Fits all subsets of these global models using [fit_all_subsets()].
#'   3. Obtains model-averaged leave-one-out predictions for each record using [loo_pred_model_avg()].
#'   4. Summarizes the errors made in step 3 by response variable type and period (see [KuskoHarvUtils::get_errors()] and [KuskoHarvUtils::get_period()]).
#' @return [`list`][base::list] with elements:
#'   * `error_summary`: [`list`][base::list] with elements storing [`data.frame`][base::data.frame] objects, where each data frame stores info about the settings of the run (the values of `reduce_colinearity`, `cwt_retain`, and `var_desc`) and the period- and response variable-specific error summaries. Different list elements store the different error summary statistics supplied to `error_types`.
#'   * `elapsed`: [`data.frame`][base::data.frame] similar to the `error_summary` element, except with a column for minutes elapsed.
#'   * `loo_preds`: [`data.frame`][base::data.frame] with the model-averaged leave-one-out predictions by record and response variable.
#'   * `models`: a [`list`][base::list] with 5 elements, one each for `effort`, `total_cpt`, and `chinook_comp`, `chum_comp`, and `sockeye_comp`. Each element is the [`list`][base::list] of fitted [`lm`][stats::lm] model objects returned by [fit_all_subsets()].
#' @export

whole_loo_analysis = function(global_formulae, fit_data, var_desc = NULL, error_types = c("RHO", "ME", "MAE", "MPE", "MAPE"),  ...) {

  # start a timer
  start = lubridate::now()

  # STEP 1: fit all models and select the top according to the settings
  model_lists = fit_all_subsets(global_formulae = global_formulae, fit_data = fit_data, ...)

  # STEP 2: obtain model-averaged leave-one-out predictions for each response variable
  loo_preds = lapply(model_lists, loo_pred_model_avg)

  # STEP 3: rescale composition variables so they sum to 1
  chinook_comp_new = with(loo_preds, chinook_comp/(chinook_comp + chum_comp + sockeye_comp))
  chum_comp_new = with(loo_preds, chum_comp/(chinook_comp + chum_comp + sockeye_comp))
  sockeye_comp_new = with(loo_preds, sockeye_comp/(chinook_comp + chum_comp + sockeye_comp))
  loo_preds$chinook_comp = chinook_comp_new
  loo_preds$chum_comp = chum_comp_new
  loo_preds$sockeye_comp = sockeye_comp_new

  # STEP 4: obtain model-averaged leave-one-out harvest predictions for all species that had composition models fitted
  spp = c("chinook", "chum", "sockeye")
  harv_loo_preds = lapply(spp, function(x) {
    spp_comp = paste0(x, "_comp")
    loo_preds$effort * loo_preds$total_cpt * loo_preds[[spp_comp]]
  })
  names(harv_loo_preds) = paste0(spp, "_harv")
  loo_preds = append(loo_preds, harv_loo_preds)
  # MAIN = loo_preds

  # STEP 5: summarize the errors
  summarize_errors = function(yhat, response, fit_data, error_types) {
      # build a data.frame with time period, prediction, and observed values
      df = data.frame(period = KuskoHarvUtils::get_period(fit_data$day), yhat = yhat, yobs = fit_data[,response])

      # calculate MPE and MAPE for each period separately
      errors = lapply(unique(df$period), function(p) {
        KuskoHarvUtils::get_errors(yhat = df$yhat[df$period == p], yobs = df$yobs[df$period == p])$summary[error_types]
      })

      # calculate MPE and MAPE for all periods combined, and append to period-specific values
      errors = append(
        errors,
        list(KuskoHarvUtils::get_errors(yhat = df$yhat, yobs = df$yobs)$summary[error_types])
      )

      # convert errors to a data frame: periods as rows
      errors = as.data.frame(do.call(rbind, errors))

      # combine with other info
      cbind(period = c(1,2,3,"all"), response = response, errors)
    }

  error_summary = lapply(names(loo_preds), function(resp) summarize_errors(loo_preds[[resp]], resp, fit_data = fit_data, error_types = error_types))
  error_summary = do.call(rbind, error_summary)

  # add a n_models variable
  n_models = lapply(model_lists, length)
  n_models = data.frame(response = names(n_models), n_models = unname(unlist(n_models)))
  error_summary = merge(error_summary, n_models, by = "response", all = TRUE)

  # make long format
  x = reshape2::melt(error_summary, id.vars = c("period", "response", "n_models"), variable.name = "type", value.name = "value")

  # make wide format
  x = reshape2::dcast(x, response + n_models ~ type + period, value.var = "value")

  # combine with other meta-information about the analysis
  error_summary = cbind(
    reduce_colinearity = ifelse(attr(model_lists[[1]], "subset_params")$reduce_colinearity, "Yes", "No"),
    cwt_retain = attr(model_lists[[1]], "subset_params")$cwt_retain,
    x
  )
  if (!is.null(var_desc)) {
    error_summary = cbind(var_desc = var_desc, error_summary)
  }

  # reorder rows
  error_summary$response = as.character(error_summary$response)
  rownames(error_summary) = error_summary$response
  error_summary = error_summary[c("effort", "total_cpt", "chinook_comp", "chum_comp", "sockeye_comp", "chinook_harv", "chum_harv", "sockeye_harv"),]
  rownames(error_summary) = NULL

  # split up error summaries
  error_summary = lapply(error_types, function(x) {
    id_vars = colnames(error_summary)[!stringr::str_detect(colnames(error_summary), paste(error_types, collapse = "|"))]
    error_summary[,c(id_vars, colnames(error_summary)[stringr::str_detect(colnames(error_summary), x)])]
  }); names(error_summary) = error_types

  # stop the timer
  stop = lubridate::now()

  # calculate elapsed time and format as data frame
  elapsed = data.frame(
    mins_elapsed = lubridate::int_length(lubridate::int_diff(c(start, stop)))/60
  )
  elapsed = cbind(
    reduce_colinearity = ifelse(attr(model_lists[[1]], "subset_params")$reduce_colinearity, "Yes", "No"),
    cwt_retain = attr(model_lists[[1]], "subset_params")$cwt_retain,
    elapsed
  )
  if (!is.null(var_desc)) {
    elapsed = cbind(var_desc = var_desc, elapsed)
  }

  # build the list object to return
  output = list(
    error_summary = error_summary,
    elapsed = elapsed,
    loo_preds = as.data.frame(do.call(cbind, loo_preds)),
    models = model_lists
  )

  # return the output
  return(output)
}
