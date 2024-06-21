# SCRIPT TO PRODUCE OBJECTS NEEDED FOR PREDICTION
# SPECIFY THEM HERE SO THE OBJECTS CAN BE EXPORTED AND LOADED ON THE FLY
# REDUCES RUN TIME LATER

# produce the regression data set
fit_data = KuskoHarvData::prepare_regression_data()

# discard any data collected
fit_data = fit_data[lubridate::month(fit_data$date) %in% c(6,7),]

# perform LOO analysis
loo_output = KuskoHarvPred:::whole_loo_analysis(
  global_formulae = list(
    effort = "day + I(day^2) + hours_open + fished_yesterday + weekend + p_before_noon + total_btf_cpue + chinook_btf_comp",
    total_cpt = "day + I(day^2) + fished_yesterday + total_btf_cpue",
    chinook_comp = "day + chinook_btf_comp",
    chum_comp = "day + chum_btf_comp",
    sockeye_comp = "day + sockeye_btf_comp"
  ),
  fit_data = fit_data,
  cwt_retain = 1
)

# store all component regression models in a separate object
fit_lists = loo_output$models
loo_output = loo_output[-which(names(loo_output) == "models")]

# build bank of miscellaneous variables
misc_bank = list(
  hours_open = seq(6, 24, by = 6),
  p_before_noon = c(0, 0.25, 0.5, 0.75, 1),
  fished_yesterday = c(FALSE, TRUE),
  weekend = c(FALSE, TRUE)
)

# function to return smoothed btf summaries using quantile generalized additive models fitted to btf data
smooth_btf = function(stat, days = seq(min(fit_data$day), max(fit_data$day)), plus_minus = 1, plot = interactive()) {

  # load the BTF data set
  data(btf_master, package = "KuskoHarvData")

  # name the btf stat: this is the name for output
  btf_stat = stringr::str_replace(stat, "_", "_btf_")

  # get the years to include
  yr_range = range(lubridate::year(btf_master$date))
  years = yr_range[1]:yr_range[2]

  btf_summary = lapply(years, function(y) {
    dates = KuskoHarvUtils::from_days_past_may31(days, y)
    out = sapply(dates, KuskoHarvData:::summarize_btf, stat = stat, plus_minus = plus_minus)
    out = data.frame(var = btf_stat, day = days, year = y, value = out)
    return(out)
  }); btf_summary = do.call(rbind, btf_summary)

  ### smooth using quantile generalized additive model ###

  # quantiles to smooth at
  qu_seq = c(0.1, 0.25, 0.5, 0.75, 0.9)

  # set the transformation function: keeps predictions positive and <1 for species composition
  if(stat == "total_cpue") {
    f = log; f_inv = exp
    ylim = c(0, max(btf_summary$value, max(fit_data[,btf_stat]))) * 1.25
  } else {
    f = qlogis; f_inv = plogis
    # qlogis(1) = Inf; don't allow but retain information that it was near 100%
    btf_summary$value[btf_summary$value == 1] = 0.99
    ylim = c(0, 1)
  }

  # qlogis(0) = log(0) = -Inf; don't allow but retain information that was near 0%
  btf_summary$value[btf_summary$value == 0] = 0.01

  # fit the qgam for all elements in qu_seq
  fits = suppressMessages(qgam::mqgam(f(value) ~ s(day), qu = qu_seq, data = btf_summary))

  # create a prediction data set
  pred_data = data.frame(day = with(btf_summary, seq(min(day), max(day))))

  # obtain the fitted curve for each qgam
  qu_preds = lapply(qu_seq, function(qu) {
    tmp = pred_data
    tmp$variable = btf_stat
    tmp$qu = qu
    tmp$pred = f_inv(qgam::qdo(fits, qu = qu, fun = predict, newdata = pred_data))
    tmp
  })
  qu_preds = do.call(rbind, qu_preds)

  if (plot) {
    m = reshape2::dcast(btf_summary, day ~ year, value.var = "value")
    m = m[,-1]
    matplot(x = days, y = m, type = "l", col = scales::alpha("grey30", 0.5), xlab = "day", ylab = btf_stat, ylim = ylim)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.5))
    lines(pred ~ day, data = subset(qu_preds, qu == 0.25), lty = 2)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.75), lty = 2)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.9), lty = 3)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.1), lty = 3)

    points(x = fit_data$day, y = fit_data[,btf_stat], pch = 16)

  }

  out = qu_preds[,c("day", "qu", "pred")]
  colnames(out) = c("day", "cat", btf_stat)
  out$cat = paste0("q", out$cat * 100)
  return(out)
}

# get BTF smoothed summaries
smooth_total_cpue = smooth_btf(stat = "total_cpue")
smooth_chinook_comp = smooth_btf(stat = "chinook_comp")
smooth_chum_comp = smooth_btf(stat = "chum_comp")
smooth_sockeye_comp = smooth_btf(stat = "sockeye_comp")

# combine into one data frame
btf_out = merge(smooth_total_cpue, smooth_chinook_comp) |>
  merge(smooth_chum_comp) |>
  merge(smooth_sockeye_comp)

# function to build a prediction data set given the variables in the model and a range of days
build_pred_data = function(vars, days = min(fit_data$day):max(fit_data$day)) {

  # extract the names of all BTF variables
  btf_vars = vars[stringr::str_detect(vars, "btf")]

  # extract the names of all miscellaneous variables
  misc_vars = vars[!(vars %in% c("day", btf_vars))]

  # remove the quadratic term if it is present
  if (any(misc_vars == "I(day^2)")) misc_vars = misc_vars[-which(misc_vars == "I(day^2)")]

  # build an expanded grid data frame for all desired combos of all variables
  # must be daily because BTF variables vary throughout season
  out = lapply(days, function(day_keep) {
    btf_bank_day = as.list(subset(btf_out[,c("day", btf_vars)], day == day_keep))
    day_bank = append(btf_bank_day, misc_bank[misc_vars])
    day_bank = lapply(day_bank, unique)
    do.call(expand.grid, day_bank)
  })

  # combine days into one data frame
  out = do.call(rbind, out)

  # combine the BTF variable categorical names
  for (btf_var in btf_vars) {
    btf_merge = btf_out[,c("day", "cat", btf_var)]
    colnames(btf_merge)[2] = paste0("CAT_", btf_var)
    out = merge(out, btf_merge, by = c("day", btf_var))
  }

  # return the output data frame
  return(out)
}

# loop through response variables, and build the prediction data set
pred_data = lapply(fit_lists, function(fit_list) build_pred_data(KuskoHarvPred:::find_variables(fit_list)))

# add model averaged predictions
pred_data = lapply(names(fit_lists), function(r) {
  cbind(pred_data[[r]], pred_response = KuskoHarvPred::predict_model_avg(fit_lists[[r]], pred_data[[r]]))
}); names(pred_data) = names(fit_lists)

# export them to proper structure and location
usethis::use_data(fit_lists, pred_data, loo_output, fit_data, internal = TRUE, overwrite = TRUE)
