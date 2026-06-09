# SCRIPT TO PRODUCE OBJECTS NEEDED FOR PREDICTION
# SPECIFY THEM HERE SO THE OBJECTS CAN BE EXPORTED AND LOADED ON THE FLY
# REDUCES RUN TIME LATER

# RUNNING THIS REQUIRES THAT KUSKOHARVPRED IS INSTALLED FIRST

# produce the regression data set
fit_data = KuskoHarvData::prepare_regression_data()

# discard any data collected outside of June or July
fit_data = fit_data[lubridate::month(fit_data$date) %in% c(6,7),]

# perform LOO analysis
starttime = Sys.time()
loo_output = KuskoHarvPred:::whole_loo_analysis(
  global_formulae = list(
    effort = "day + I(day^2) + hours_open + weekend + p_before_noon + total_sonar_count + chinook_sonar_comp",
    total_cpt = "day + I(day^2) + total_sonar_count",
    chinook_comp = "day + I(day^2) + chinook_sonar_comp + I(chinook_sonar_comp^2)",
    chum_comp = "day + I(day^2) + chum_sonar_comp + I(chum_sonar_comp^2)",
    sockeye_comp = "day + I(day^2) + sockeye_sonar_comp + I(sockeye_sonar_comp^2)"
  ),
  fit_data = fit_data,
  lag = 3,
  cwt_retain = 1
)
Sys.time() - starttime

# store all component regression models in a separate object
fit_lists = loo_output$models
loo_output = loo_output[-which(names(loo_output) == "models")]

# build bank of miscellaneous variables
misc_bank = list(
  hours_open = seq(6, 24, by = 6),
  p_before_noon = c(0, 0.25, 0.5, 0.75, 1),
  # fished_yesterday = c(FALSE, TRUE),
  weekend = c(FALSE, TRUE)
)

# stat = "total_count"
# days = seq(min(fit_data$day), max(fit_data$day))
# plus_minus = 0
# plot = TRUE

# function to return smoothed sonar summaries using quantile generalized additive models fitted to sonar data
smooth_sonar = function(stat, days = seq(min(fit_data$day), max(fit_data$day)), plus_minus = 0, plot = interactive()) {

  # load the sonar data set
  data("sonar_data_all", package = "KuskoHarvData")

  # name the sonar stat: this is the name for output
  sonar_stat = stringr::str_replace(stat, "_", "_sonar_")

  # get the years to include
  yr_range = range(lubridate::year(sonar_data_all$date))
  years = yr_range[1]:yr_range[2]

  sonar_summary = lapply(years, function(y) {
    dates = KuskoHarvUtils::from_days_past_may31(days, y)
    out = sapply(dates, KuskoHarvData:::summarize_sonar, stat = stat, plus_minus = plus_minus)
    out = data.frame(var = sonar_stat, day = days, year = y, value = out)
    return(out)
  }); sonar_summary = do.call(rbind, sonar_summary)

  ### smooth using quantile generalized additive model ###

  # quantiles to smooth at
  qu_seq = c(0.1, 0.25, 0.5, 0.75, 0.9)

  # set the transformation function: keeps predictions positive and <1 for species composition
  if(stat == "total_count") {
    f = log; f_inv = exp
    ylim = c(0, max(sonar_summary$value, max(fit_data[,sonar_stat]))) * 1.25
  } else {
    f = qlogis; f_inv = plogis
    # qlogis(1) = Inf; don't allow but retain information that it was near 100%
    sonar_summary$value[sonar_summary$value == 1] = 0.99
    ylim = c(0, 1)
  }

  # qlogis(0) = log(0) = -Inf; don't allow but retain information that was near 0%
  sonar_summary$value[sonar_summary$value == 0] = 0.01

  # fit the qgam for all elements in qu_seq
  junk = capture.output({
    fits = suppressMessages({
      qgam::mqgam(f(value) ~ s(day), qu = qu_seq, data = sonar_summary)})
  })

  # create a prediction data set
  pred_data = data.frame(day = with(sonar_summary, seq(min(day), max(day))))

  # obtain the fitted curve for each qgam
  qu_preds = lapply(qu_seq, function(qu) {
    tmp = pred_data
    tmp$variable = sonar_stat
    tmp$qu = qu
    tmp$pred = f_inv(qgam::qdo(fits, qu = qu, fun = predict, newdata = pred_data))
    tmp
  })
  qu_preds = do.call(rbind, qu_preds)

  if (plot) {
    m = reshape2::dcast(sonar_summary, day ~ year, value.var = "value")
    m = m[,-1]
    matplot(x = days, y = m, type = "l", col = scales::alpha("grey30", 0.5), xlab = "day", ylab = sonar_stat, ylim = ylim)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.5))
    lines(pred ~ day, data = subset(qu_preds, qu == 0.25), lty = 2)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.75), lty = 2)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.9), lty = 3)
    lines(pred ~ day, data = subset(qu_preds, qu == 0.1), lty = 3)

    points(x = fit_data$day, y = fit_data[,sonar_stat], pch = 16)

  }

  out = qu_preds[,c("day", "qu", "pred")]
  colnames(out) = c("day", "cat", sonar_stat)
  out$cat = paste0("q", out$cat * 100)
  return(out)
}

# get sonar smoothed summaries
smooth_total_count = smooth_sonar(stat = "total_count")
smooth_chinook_comp = smooth_sonar(stat = "chinook_comp")
smooth_chum_comp = smooth_sonar(stat = "chum_comp")
smooth_sockeye_comp = smooth_sonar(stat = "sockeye_comp")

# combine into one data frame
sonar_out = merge(smooth_total_count, smooth_chinook_comp) |>
  merge(smooth_chum_comp) |>
  merge(smooth_sockeye_comp)

# vars = KuskoHarvPred:::find_variables(fit_list)
# days = min(fit_data$day):max(fit_data$day)

# function to build a prediction data set given the variables in the model and a range of days
build_pred_data = function(vars, days = min(fit_data$day):max(fit_data$day)) {

  # extract the names of all sonar variables
  sonar_vars = vars[stringr::str_detect(vars, "sonar")]
  sonar_vars = sonar_vars[!stringr::str_detect(sonar_vars, "\\^2")]

  # extract the names of all miscellaneous variables
  misc_vars = vars[!(vars %in% c("day", sonar_vars))]

  # remove the quadratic term if it is present
  if (any(misc_vars == "I(day^2)")) misc_vars = misc_vars[-which(misc_vars == "I(day^2)")]
  if (any(misc_vars == "I(chinook_sonar_comp^2)")) misc_vars = misc_vars[-which(misc_vars == "I(chinook_sonar_comp^2)")]
  if (any(misc_vars == "I(chum_sonar_comp^2)")) misc_vars = misc_vars[-which(misc_vars == "I(chum_sonar_comp^2)")]
  if (any(misc_vars == "I(sockeye_sonar_comp^2)")) misc_vars = misc_vars[-which(misc_vars == "I(sockeye_sonar_comp^2)")]

  # build an expanded grid data frame for all desired combos of all variables
  # must be daily because sonar variables vary throughout season
  out = lapply(days, function(day_keep) {
    sonar_bank_day = as.list(subset(sonar_out[,c("day", sonar_vars)], day == day_keep))
    day_bank = append(sonar_bank_day, misc_bank[misc_vars])
    day_bank = lapply(day_bank, unique)
    do.call(expand.grid, day_bank)
  })

  # combine days into one data frame
  out = do.call(rbind, out)

  # combine the sonar variable categorical names
  for (sonar_var in sonar_vars) {
    sonar_merge = sonar_out[,c("day", "cat", sonar_var)]
    colnames(sonar_merge)[2] = paste0("CAT_", sonar_var)
    out = merge(out, sonar_merge, by = c("day", sonar_var))
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
