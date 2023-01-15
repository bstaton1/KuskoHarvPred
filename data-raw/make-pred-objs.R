# SCRIPT TO PRODUCE OBJECTS NEEDED FOR PREDICTION
# SPECIFY THEM HERE SO THE OBJECTS CAN BE EXPORTED AND LOADED ON THE FLY
# REDUCES RUN TIME LATER

# produce the regression data set
dat = KuskoHarvData::prepare_regression_data()

# perform LOO analysis
loo_output = KuskoHarvPred:::whole_loo_analysis(
  global_formulae = list(
    effort = "day + hours_open + not_first_day + weekend + p_before_noon + total_btf_cpue + chinook_btf_comp + mean_Nwind + mean_Ewind",
    total_cpt = "day + I(day^2) + hours_open + not_first_day + p_before_noon + total_btf_cpue + mean_Nwind + mean_Ewind",
    chinook_comp = "day + chinook_btf_comp",
    chum_comp = "day + chum_btf_comp",
    sockeye_comp = "day + sockeye_btf_comp"
  ),
  fit_data = dat
)

# store all component regression models in a separate object
fit_lists = loo_output$models
loo_output = loo_output[-which(names(loo_output) == "models")]

# build objects needed for constructing predictive data set
# years
year_range = sort(unique(lubridate::year(dat$date)))

# build bank of day variable
day = min(dat$day):max(dat$day)

# build bank of miscellaneous variables
misc_bank = list(
  hours_open = seq(6, 24, by = 6),
  p_before_noon = c(0, 0.25, 0.5, 0.75, 1),
  not_first_day = c(FALSE, TRUE),
  weekend = c(FALSE, TRUE)
)

# extract BTF total CPUE summaries for each day of each year
total_btf_cpue = lapply(year_range, function(y) {
  dates = KuskoHarvData:::from_days_past_may31(day, y)
  out = sapply(dates, KuskoHarvData:::summarize_btf, stat = "total_cpue", plus_minus = 1)
  out = data.frame(var = "total_btf_cpue", day = day, year = y, value = out)
  return(out)
}); total_btf_cpue = do.call(rbind, total_btf_cpue)

# extract BTF Chinook composition summaries for each day of each year
chinook_btf_comp = lapply(year_range, function(y) {
  dates = KuskoHarvData:::from_days_past_may31(day, y)
  out = sapply(dates, KuskoHarvData:::summarize_btf, stat = "chinook_comp", plus_minus = 1)
  out = data.frame(var = "chinook_btf_comp", day = day, year = y, value = out)
  return(out)
}); chinook_btf_comp = do.call(rbind, chinook_btf_comp)

# extract BTF chum composition summaries for each day of each year
chum_btf_comp = lapply(year_range, function(y) {
  dates = KuskoHarvData:::from_days_past_may31(day, y)
  out = sapply(dates, KuskoHarvData:::summarize_btf, stat = "chum_comp", plus_minus = 1)
  out = data.frame(var = "chum_btf_comp", day = day, year = y, value = out)
  return(out)
}); chum_btf_comp = do.call(rbind, chum_btf_comp)

# extract BTF sockeye composition summaries for each day of each year
sockeye_btf_comp = lapply(year_range, function(y) {
  dates = KuskoHarvData:::from_days_past_may31(day, y)
  out = sapply(dates, KuskoHarvData:::summarize_btf, stat = "sockeye_comp", plus_minus = 1)
  out = data.frame(var = "sockeye_btf_comp", day = day, year = y, value = out)
  return(out)
}); sockeye_btf_comp = do.call(rbind, sockeye_btf_comp)

# combine into one large data frame
btf = rbind(total_btf_cpue, chinook_btf_comp, chum_btf_comp, sockeye_btf_comp)

# calculate daily means, mins, and maxs for each variable across years
btf_means = aggregate(value ~ var + day, data = btf, FUN = mean); btf_means = cbind(btf_means, cat = "mean")
btf_mins = aggregate(value ~ var + day, data = btf, FUN = min); btf_mins = cbind(btf_mins, cat = "min")
btf_maxs = aggregate(value ~ var + day, data = btf, FUN = max); btf_maxs = cbind(btf_maxs, cat = "max")

# convert each to wide format
btf_means = reshape2::dcast(btf_means, day + cat ~ var, value.var = "value")
btf_mins = reshape2::dcast(btf_mins, day + cat ~ var, value.var = "value")
btf_maxs = reshape2::dcast(btf_maxs, day + cat ~ var, value.var = "value")

# combine summary statistics
btf_out = rbind(btf_means, btf_mins, btf_maxs)
btf_out = btf_out[order(btf_out$day),]
rownames(btf_out) = NULL

# build bank of weather variables
weather_bank = list(
  mean_Nwind = c(-10, 0, 10),
  mean_Ewind = c(-10, 0, 10)
)

weather_key = data.frame(
  mean_Nwind = c(-10, 0, 10),
  mean_Ewind = c(-10, 0, 10),
  CAT_mean_Nwind = c("strong_southerly", "none", "strong_northerly"),
  CAT_mean_Ewind = c("strong_westerly", "none", "strong_easterly")
)

build_pred_data = function(vars, days = min(dat$day):max(dat$day)) {

  # extract the names of all BTF variables
  btf_vars = vars[stringr::str_detect(vars, "btf")]

  # extract the names of all weather variables
  weather_vars = vars[stringr::str_detect(vars, "wind")]

  # extract the names of all miscellaneous variables
  misc_vars = vars[!(vars %in% c("day", btf_vars, weather_vars))]

  # remove the quadratic term if it is present
  if (any(misc_vars == "I(day^2)")) misc_vars = misc_vars[-which(misc_vars == "I(day^2)")]

  # build an expanded grid data frame for all desired combos of all variables
  # must be daily because BTF variables vary throughout season
  out = lapply(days, function(day_keep) {
    btf_bank_day = as.list(subset(btf_out[,c("day", btf_vars)], day == day_keep))
    day_bank = append(append(btf_bank_day, misc_bank[misc_vars]), weather_bank[weather_vars])
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

  # combine the weather variable categorical names
  if (any(weather_vars %in% colnames(weather_key))) {
    for (weather_var in weather_vars) {
      weather_merge = weather_key[stringr::str_detect(colnames(weather_key),weather_var)]
      out = merge(out, weather_merge, by = weather_var)
    }
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
usethis::use_data(fit_lists, pred_data, loo_output, internal = TRUE, overwrite = TRUE)
