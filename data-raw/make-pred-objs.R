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

# build the prediction data set for each response variable
pred_data = lapply(fit_lists, function(fit_list) build_pred_data(KuskoHarvPred:::find_variables(fit_list)))

# add model averaged predictions
pred_data = lapply(names(fit_lists), function(r) {
  cbind(pred_data[[r]], pred_response = KuskoHarvPred::predict_model_avg(fit_lists[[r]], pred_data[[r]]))
}); names(pred_data) = names(fit_lists)

# export them to proper structure and location
usethis::use_data(fit_lists, pred_data, loo_output, fit_data, internal = TRUE, overwrite = TRUE)
