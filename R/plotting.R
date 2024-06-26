#' Subset Pre-Processed Model Predictions
#'
#' @param response Character; one of `"effort"`, `"total_cpt"`, `"chinook_comp"`, `"chum_comp"`, or `"sockeye_comp"`
#' @param settings List specifying which covariate settings to subset the predicted values for. See details.
#' @details The `settings` argument must be a list, and if it is empty (the default), the acceptable elements will be populated with preset values.
#'   The following elements are acceptable:
#'     * `settings$day`: numeric; defaults to `12:46`
#'     * `settings$hours_open`: numeric; must contain any combination of 6, 12, 18, 24, defaults to 12
#'     * `settings$p_before_noon`: numeric; must contain any combination of 0.25, 0.5, 0.75, 1; defaults to 0.5
#'     * `settings$fished_yesterday`: logical; defaults to `FALSE`
#'     * `settings$weekend`: logical; defaults to `FALSE`
#'     * `settings$CAT_total_btf_cpue`: character, must contain any combination of `"q10"`, `"q25"`, `"q50"`, `"q75"`, `"q90"`; defaults to `"q50"`
#'     * `settings$CAT_chinook_btf_comp`: same as `settings$CAT_total_btf_cpue`
#'     * `settings$CAT_chum_btf_comp`: same as `settings$CAT_total_btf_cpue`
#'     * `settings$CAT_sockeye_btf_comp`: same as `settings$CAT_total_btf_cpue`
#'     * `settings$CAT_mean_Nwind`: character, must contain any combination of `"strong_northerly"`, `"none"`, `"strong_southerly"`; defaults to `"none"`
#'     * `settings$CAT_mean_Ewind`: character, must contain any combination of `"strong_easterly"`, `"none"`, `"strong_westerly"`; defaults to `"none"`
#'  @note Because not every predictor variable is used for all responses, it is possible to change the `settings` argument and receive the same output.
#'

subset_pred_data = function(response, settings = list()) {

  # set default settings if not supplied
  if (is.null(settings$day)) settings$day = 12:46
  if (is.null(settings$hours_open)) settings$hours_open = 12
  if (is.null(settings$p_before_noon)) settings$p_before_noon = 0.5
  if (is.null(settings$fished_yesterday)) settings$fished_yesterday = FALSE
  if (is.null(settings$weekend)) settings$weekend = FALSE
  if (is.null(settings$CAT_total_btf_cpue)) settings$CAT_total_btf_cpue = "q50"
  if (is.null(settings$CAT_chinook_btf_comp)) settings$CAT_chinook_btf_comp = "q50"
  if (is.null(settings$CAT_chum_btf_comp)) settings$CAT_chum_btf_comp = "q50"
  if (is.null(settings$CAT_sockeye_btf_comp)) settings$CAT_sockeye_btf_comp = "q50"
  if (is.null(settings$CAT_mean_Nwind)) settings$CAT_mean_Nwind = "none"
  if (is.null(settings$CAT_mean_Ewind)) settings$CAT_mean_Ewind = "none"

  # trim settings down to only those found in prediction data set
  settings = settings[which(names(settings) %in% names(pred_data[[response]]))]

  # extract only the columns included in the settings list
  pred_data_sub = pred_data[[response]][,names(settings)]
  if (!is.data.frame(pred_data_sub)) {
    pred_data_sub = data.frame(pred_data_sub)
    colnames(pred_data_sub) = names(settings)
  }

  # for each variable, find the rows that match the settings value for that variable
  sub_true = do.call(cbind, lapply(names(settings), function(i) pred_data_sub[,i] %in% settings[[i]]))

  # extract only rows that have TRUE for all variables specified in settings
  out = pred_data[[response]][apply(sub_true, 1, all),]
  out = out[order(out$day),]
  rownames(out) = NULL

  # return the output
  return(out)
}

#' Plot the Relationship of a Variable with Day of the Season
#'
#' @param response Character; one of `"effort"`, `"total_cpt"`, `"chinook_comp"`, `"chum_comp"`, or `"sockeye_comp"`
#' @param settings List specifying which covariate settings to subset the predicted values for. Passed to [subset_pred_data()].
#' @param dat Data frame; the input regression data set, defaults to `KuskoHarvPred:::fit_data`, which is equivalent to [KuskoHarvData::prepare_regression_data()].
#' @param separate_day_types Logical; if the variable passed to `response` used the predictor variable `fished_yesterday`, should two relationships be drawn?
#' @param pred_day Numeric; the day corresponding to a hypothetical prediction (`pred_response`). Defaults to `NULL` in which case this is not drawn.
#' @param pred_response Numeric; the predicted response corresponding to a hypothetical day (`pred_day`). Defaults to `NULL` in which case this is not drawn.
#' @param draw_make_range Logical; should a shaded region around model prediction that shows +/- 1MAPE be shown?
#' @export

relationship_plot = function(response, settings = list(), dat = KuskoHarvPred:::fit_data, separate_day_types = TRUE, pred_day = NULL, pred_response = NULL, draw_mape_range = FALSE) {

  # aesthetic settings here
  pt_cex = 1.5
  pt_col = scales::alpha("royalblue", 0.5)
  pt_bg = scales::alpha("skyblue", 0.5)
  line_col = "salmon"
  poly_col = scales::alpha(line_col, 0.25)

  # create the y-axis label
  ylab = KuskoHarvUtils::get_var_name(response)

  # create the plot with two lines: one for fished_yesterday and one for !fished_yesterday
  if ("fished_yesterday" %in% colnames(pred_data[[response]]) & separate_day_types) {

    # extract only the data for this response variable and specific covariate settings
    settings$fished_yesterday = c(TRUE, FALSE)
    sub_pred_data = subset_pred_data(response, settings = settings)

    # set the y-axis limits
    ylim = c(0, max(sub_pred_data$pred_response, max(dat[,response]))) * 1.05

    # scatter plot with correct dimensions, labels, etc.
    plot(dat[,response] ~ day, data = dat, type = "n", ylim = ylim, xaxt = "n", yaxt = "n",
         ylab = ylab, xlab = "Date", axes = FALSE)

    # draw uncertainty if instructed
    if (draw_mape_range) {
      # for not fished yesterday
      x = subset(sub_pred_data, !fished_yesterday)
      period = KuskoHarvUtils::get_period(x$day)
      mape = sapply(period, function(p) get_mape(response, p))
      lwr = x$pred_response - x$pred_response * mape
      upr = x$pred_response + x$pred_response * mape
      lwr = ifelse(lwr < 0, 0, lwr)
      polygon(x = c(x$day, rev(x$day)), y = c(lwr, rev(upr)), col = poly_col, border = FALSE)
      lines(lwr ~ x$day, col = line_col)
      lines(upr ~ x$day, col = line_col)

      # for fished yesterday
      x = subset(sub_pred_data, fished_yesterday)
      period = KuskoHarvUtils::get_period(x$day)
      mape = sapply(period, function(p) get_mape(response, p))
      lwr = x$pred_response - x$pred_response * mape
      upr = x$pred_response + x$pred_response * mape
      lwr = ifelse(lwr < 0, 0, lwr)
      polygon(x = c(x$day, rev(x$day)), y = c(lwr, rev(upr)), col = poly_col, border = FALSE)
      lines(lwr ~ x$day, col = line_col)
      lines(upr ~ x$day, col = line_col)
    }

    # draw the fitted curves
    lines(pred_response ~ day, data = subset(sub_pred_data, !fished_yesterday), col = line_col, lwd = 2)
    lines(pred_response ~ day, data = subset(sub_pred_data, fished_yesterday), lty = 2, col = line_col, lwd = 2)

    # draw points
    points(dat[,response] ~ day, data = dat, pch = ifelse(fished_yesterday, 24, 21),
           bg = pt_bg, col = pt_col, cex = pt_cex)

    # add a legend
    legend_location = ifelse(response == "total_cpt", "topleft", "topright")
    legend(legend_location, title = "Fished Yesterday?", legend = c("No", "Yes"),
           lty = c(1, 2), col = line_col, lwd = 2, bty = "n", text.col = "white",
           x.intersp = 2.5, seg.len = 3.5, cex = 0.8)
    legend(legend_location, title = "Fished Yesterday?", legend = c("No", "Yes"),
           pch = c(21, 24), col = pt_col, pt.bg = pt_bg, pt.cex = pt_cex, bty = "n",
           x.intersp = 2.5, text.col = par("col.axis"), cex = 0.8)

  } else {

    # extract only the data for this response variable and specific covariate settings
    sub_pred_data = subset_pred_data(response, settings = settings)

    # set the y-axis limits
    if (stringr::str_detect(response, "comp")) {
      ylim = c(0,1)
    } else {
      ylim = c(0, max(sub_pred_data$pred_response, max(dat[,response])))
    }

    # scatter plot with correct dimensions, labels, etc.
    plot(dat[,response] ~ day, data = dat, type = "n", ylim = ylim, xaxt = "n", yaxt = "n",
         ylab = ylab, xlab = "Date", axes = FALSE)

    # draw uncertainty if instructed
    if (draw_mape_range) {
      x = sub_pred_data
      period = KuskoHarvUtils::get_period(x$day)
      mape = sapply(period, function(p) get_mape(response, p))
      lwr = x$pred_response - x$pred_response * mape
      upr = x$pred_response + x$pred_response * mape
      lwr = ifelse(lwr < 0, 0, lwr)
      polygon(x = c(x$day, rev(x$day)), y = c(lwr, rev(upr)), col = poly_col, border = FALSE)
      lines(lwr ~ x$day, col = line_col)
      lines(upr ~ x$day, col = line_col)
    }

    # draw points
    points(dat[,response] ~ day, data = dat, pch = 21, col = pt_col, bg = pt_bg, cex = pt_cex)

    # draw the fitted curve
    lines(pred_response ~ day, data = sub_pred_data, col = line_col, lwd = 3)
  }

  usr = par("usr")

  # if specific predicted day and response values are provided, draw them
  if (!is.null(pred_day) & !is.null(pred_response)) {
    segments(pred_day, usr[3], pred_day, pred_response, lwd = 2, lty = 3, col = "grey")
    segments(usr[1], pred_response, pred_day, pred_response, lwd = 2, lty = 3, col = "grey")
    points(x = pred_day, y = pred_response, pch = 16, cex = pt_cex)
  }

  # add x-axis
  draw_day_axis(min(dat$day), max(dat$day), by = 5, side = 1)

  # add y-axis
  if (stringr::str_detect(response, "comp")) {
    draw_percent_axis(side = 2)
  } else {
    axis(side = 2, col = "white")
    draw_axis_line(side = 2)
  }
}

#' Draw an Axis showing Dates
#'
#' @param fday The first day (1 = June 1)
#' @param lday The last day (1 = June 1)
#' @param by The interval to draw axis ticks at
#' @param side The side of the plot to draw the axis on,
#'  1 = x-axis, 2 = y-axis
#' @param ... Optional arguments to be passed to [graphics::axis()]

draw_day_axis = function(fday, lday, by, side = 1, col = "white", ...) {
  at = seq(fday, lday, by = by)
  date = KuskoHarvUtils::from_days_past_may31(at)
  month = lubridate::month(date)
  day = lubridate::day(date)
  lab = paste(month, day, sep = "/")
  axis(side = side, at = at, labels = lab, col = col, ...)
  draw_axis_line(side = side)
}

#' Draw an Axis Showing Percentages
#'
#' @inheritParams draw_day_axis
#'

draw_percent_axis = function(side, col = "white", ...) {
  usr = par("usr")
  if (side == 1) i = c(1,2) else i = c(3,4)
  at = axisTicks(usr[i], log = FALSE)
  axis(side = side, at = at, labels = paste0(at * 100, "%"), col = col, ...)
  draw_axis_line(side = side)
}

#' Draw an Axis Showing Percentages
#'
#' @inheritParams draw_day_axis
#'

draw_yn_axis = function(side, col = "white", ...) {
  axis(side = side, at = c(0,1), labels = c("No", "Yes"), col = col, ...)
  draw_axis_line(side = side)
}

#' Draw an Axis Line with No Ticks or Labels
#'
#' @inheritParams draw_day_axis
#' @param col Color, defaults to `par("col.axis")`

draw_axis_line = function(side, col = par("col.axis")) {
  usr = par("usr")
  if (side == 1) segments(usr[1], usr[3], usr[2], usr[3], col = col, xpd = TRUE)
  if (side == 2) segments(usr[1], usr[3], usr[1], usr[4], col = col, xpd = TRUE)
  if (side == 3) segments(usr[1], usr[4], usr[2], usr[4], col = col, xpd = TRUE)
  if (side == 4) segments(usr[2], usr[3], usr[2], usr[4], col = col, xpd = TRUE)
}

#' Create a Scatterplot of Two Variables
#'
#' For investigating basic co-linearity in predictors,
#' relationships among variables not available
#' from [relationship_plot()].
#'
#' @param xvar Character; name of variable to show on the x-axis, defaults to `"total_btf_cpue"`
#' @param yvar Character; name of variable to show on the y-axis, defaults to `"total_cpt"`
#' @param color_periods Logical; should points be color-coded to show which time period of the season the data came from?
#'   See [KuskoHarvUtils::get_period()]. Defaults to `FALSE`.
#' @param label_years Logical; should points be labeled to show which year the data came from?
#' @param dat Data frame; the input regression data set, defaults to `KuskoHarvPred:::fit_data`, which is equivalent to [KuskoHarvData::prepare_regression_data()].
#' @export

vars_biplot = function(xvar = "total_btf_cpue", yvar = "total_cpt", color_periods = FALSE, label_years = FALSE, dat = KuskoHarvPred:::fit_data) {

  # error handle to ensure variables are found
  if (!xvar %in% colnames(dat)) stop ("Value of xvar ('", xvar, "') not in dat")
  if (!yvar %in% colnames(dat)) stop ("Value of yvar ('", yvar, "') not in dat")

  # make colors
  if (color_periods) {
    base_col = ifelse(dat$period == 1, "skyblue", ifelse(dat$period == 2, "orange", ifelse(dat$period == 3, "salmon", "grey20")))
  } else {
    base_col = "grey50"
  }
  alpha = c(bg = 0.5, col = 0.75)

  # extract the values of each variable
  x = dat[,xvar]; y = dat[,yvar]

  # determine if each variable is a percentage
  x_is_comp = stringr::str_detect(xvar, "comp") | xvar == "p_before_noon"
  y_is_comp = stringr::str_detect(yvar, "comp") | yvar == "p_before_noon"

  # determine if each variable is a Yes/No variable
  x_is_yn = xvar %in% c("fished_yesterday", "weekend")
  y_is_yn = yvar %in% c("fished_yesterday", "weekend")

  # add jitter to points if Yes/No
  if (x_is_yn) x = x + runif(length(x), -0.25, 0.25)
  if (y_is_yn) y = y + runif(length(y), -0.25, 0.25)

  # widen axis limits if Yes/No variable
  if (x_is_yn) xoff = c(-0.3, 0.3) else xoff = c(0,0)
  if (y_is_yn) yoff = c(-0.3, 0.3) else yoff = c(0,0)
  if (color_periods) ymult = c(1,1.2) else ymult = c(1,1)

  # make an empty plot with proper labels and dimensions
  plot(x = 1, y = 1, type = "n",xlab = KuskoHarvUtils::get_var_name(xvar), ylab = KuskoHarvUtils::get_var_name(yvar),
       xlim = range(dat[,xvar]) + xoff, ylim = (range(dat[,yvar]) + yoff) * ymult, axes = FALSE)

  # draw the points
  points(x = x, y = y, pch = 21, cex = 1.75,
         bg = scales::alpha(base_col, alpha["bg"]),
         col = scales::alpha(base_col, alpha["col"]))

  # label the years if requested
  if (label_years) {
    text(x = x, y = y, labels = substr(dat$year, 3, 4), cex = 0.8)
  }

  # draw the appropriate X-axis ticks and labels
  if (x_is_comp) {
    draw_percent_axis(side = 1)
  } else {
    if (xvar == "day") {
      draw_day_axis(min(dat$day), max(dat$day), by = 5, side = 1)
    } else {
      if (x_is_yn) {
        draw_yn_axis(side = 1)
      } else {
        axis(side = 1, col = "white")
        draw_axis_line(side = 1)
      }
    }
  }

  # draw the appropriate Y-axis ticks and labels
  if (y_is_comp) {
    draw_percent_axis(side = 2)
  } else {
    if (yvar == "day") {
      draw_day_axis(min(dat$day), max(dat$day), by = 5, side = 2)
    } else {
      if (y_is_yn) {
        draw_yn_axis(side = 2)
      } else {
        axis(side = 2, col = "white")
        draw_axis_line(side = 2)
      }
    }
  }

  # include a legend to differentiate time periods
  if (color_periods) {
    legend ("top", horiz = TRUE, title = "Period", legend = c("6/12-6/19", "6/20-6/30", ">= 7/1"), pch = 21,
            col = scales::alpha(c("skyblue", "orange", "salmon"), alpha["col"]),
            pt.bg = scales::alpha(c("skyblue", "orange", "salmon"), alpha["bg"]),
            pt.cex = 2, cex = 0.9, text.col = par("col.axis"),
            bty = "n"
    )
  }
}
