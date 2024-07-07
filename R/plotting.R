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
  pt_col = scales::alpha("royalblue", 0.75)
  pt_bg = scales::alpha("royalblue", 0.5)
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
    if (draw_mape_range) {
      x = subset(sub_pred_data, !fished_yesterday)
      period = KuskoHarvUtils::get_period(x$day)
      mape = sapply(period, function(p) get_mape(response, p))
      lwr = x$pred_response - x$pred_response * mape
      upr = x$pred_response + x$pred_response * mape
      ylim = c(0, max(sub_pred_data$pred_response, dat[,response], upr)) * 1.05
    } else {
      ylim = c(0, max(sub_pred_data$pred_response, dat[,response])) * 1.05
    }

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
      if (draw_mape_range) {

        ylim = c(0, max(sub_pred_data$pred_response, dat[,response]))
      } else {
        x = sub_pred_data
        period = KuskoHarvUtils::get_period(x$day)
        mape = sapply(period, function(p) get_mape(response, p))
        lwr = x$pred_response - x$pred_response * mape
        upr = x$pred_response + x$pred_response * mape
        lwr = ifelse(lwr < 0, 0, lwr)
        ylim = c(0, max(sub_pred_data$pred_response, dat[,response], upr))
      }

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
    axis(side = 2, col = "white", col.ticks = par("col.axis"))
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

draw_day_axis = function(fday, lday, by, side = 1, col = par("col.axis"), ...) {
  at = seq(fday, lday, by = by)
  date = KuskoHarvUtils::from_days_past_may31(at)
  month = lubridate::month(date)
  day = lubridate::day(date)
  lab = paste(month, day, sep = "/")
  axis(side = side, at = at, labels = lab, col = "white", col.ticks = col, ...)
  draw_axis_line(side = side)
}

#' Draw an Axis Showing Percentages
#'
#' @inheritParams draw_day_axis
#'

draw_percent_axis = function(side, col = par("col.axis"), ...) {
  usr = par("usr")
  if (side == 1) i = c(1,2) else i = c(3,4)
  at = axisTicks(usr[i], log = FALSE)
  axis(side = side, at = at, labels = paste0(at * 100, "%"), col = "white", col.ticks = col, ...)
  draw_axis_line(side = side)
}

#' Draw an Axis Showing Percentages
#'
#' @inheritParams draw_day_axis
#'

draw_yn_axis = function(side, col = par("col.axis"), ...) {
  axis(side = side, at = c(0,1), labels = c("No", "Yes"), col = "white", col.ticks = col, ...)
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

#' Auto-select the Type of Axis to Draw
#'
#' For use in generalized plotting functions where
#' the type of variable (and thus axis) needs to change
#'
#' @param var Variable name. Must be a character vector of length 1.
#' @return Character vector of length 1; one of:
#'   * `"yn"` -- if `var %in% c("fished_yesterday", "weekend")`
#'   * `"day"` -- if `var == "day"`
#'   * `"percent"` -- e.g., if `var %in% c("chinook_comp", "btf_chinook_comp")`
#'   * `"regular"` -- otherwise

choose_axis_type = function(var) {

  # error handle to make sure var is a single element vector
  if (length(var) > 1) stop ("var must be a vector of length 1")

  # error handle to make sure var is a character
  if (!is.character(var)) stop ("var must be a character vector")

  # set the variable names that use each specialty axis type
  yn_vars = c("fished_yesterday", "weekend")
  day_vars = c("day")
  percent_vars = c(
    "chinook_comp", "chum_comp", "sockeye_comp",
    "chinook_btf_comp", "chum_btf_comp", "sockeye_btf_comp",
    "p_before_noon"
  )

  # initialize an empty type
  type = NA
  if (is.na(type) & var %in% yn_vars) type = "yn"
  if (is.na(type) & var %in% day_vars) type = "day"
  if (is.na(type) & var %in% percent_vars) type = "percent"
  if (is.na(type)) type = "regular"

  return(type)
}

#' Create a Scatterplot of Two Variables
#'
#' For investigating basic relationships not available
#' from [relationship_plot()], which only allows day on the _x_-axis.
#'
#' @param x Numeric vector; variable to show on the x-axis
#' @param y Numeric vector; variable to show on the y-axis
#' @param x_axis_type Character vector of length 1, output of [choose_axis_type()] applicable to `x`.
#' @param y_axis_type Character vector of length 1, output of [choose_axis_type()] applicable to `y`.
#' @param period Optional numeric vector (values 1, 2, or 3) indicating the period each element of `x` and `y` belong to.
#'   See [KuskoHarvUtils::get_period()]. Defaults to `NULL`, in which case points are grey; otherwise points are color-coded.
#' @param year Optional numeric vector (values 1, 2, or 3) indicating the year each element of `x` and `y` belong to.
#'   Defaults to `NULL`, in which case points are not labeled; otherwise, points are labeled with the last two digits of the year.
#' @param legend Optional character vector of length 1 indicating where to place the legend if `!is.null(period)`.
#'   Defaults to `NULL`, in which case no legend is drawn; otherwise supply a value such as `"top"` or `"bottomright"`.
#' @param xlab,ylab,xlim,ylim Supplied to [graphics::plot()].
#' @export

vars_biplot = function(x, y, x_axis_type, y_axis_type, period = NULL, year = NULL, legend = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL) {

  # error handle to ensure axis type is accepted
  accepted_axis_types = c("percent", "yn", "day", "regular")
  if (!x_axis_type %in% accepted_axis_types) {
    stop ("x_axis_type must be one of: ", knitr::combine_words(accepted_axis_types, before = "'", and = "or"))
  }
  if (!y_axis_type %in% accepted_axis_types) {
    stop ("y_axis_type must be one of: ", knitr::combine_words(accepted_axis_types, before = "'", and = "or"))
  }

  # check to make sure lengths of variables are good
  if (length(x) != length(y)) stop ("x and y must be the same length")
  if (!is.null(period)) {if (length(period) != length(x)) stop ("period and x must be the same length")}
  if (!is.null(year)) {if (length(year) != length(x)) stop ("year and x must be the same length")}

  # set colors
  if (!is.null(period)) {
    base_col = ifelse(period == 1, "royalblue", ifelse(period == 2, "orange", ifelse(period == 3, "salmon", "grey20")))
  } else {
    base_col = "grey50"
  }
  alpha = c(bg = 0.5, col = 0.75)

  # determine if each variable is a percentage
  x_is_percent = x_axis_type == "percent"
  y_is_percent = y_axis_type == "percent"

  # determine if each variable is a Yes/No variable
  x_is_yn = x_axis_type == "yn"
  y_is_yn = y_axis_type == "yn"

  # determine if each variable is a day variable
  x_is_day = x_axis_type == "day"
  y_is_day = y_axis_type == "day"

  # add jitter to points if Yes/No
  if (x_is_yn) x = x + runif(length(x), -0.25, 0.25)
  if (y_is_yn) y = y + runif(length(y), -0.25, 0.25)

  # widen axis limits if Yes/No variable
  if (x_is_yn) xoff = c(-0.3, 0.3) else xoff = c(0,0)
  if (y_is_yn) yoff = c(-0.3, 0.3) else yoff = c(0,0)

  # set x and y limits if not already supplied
  if (is.null(xlim)) xlim = range(x)
  if (is.null(ylim)) ylim = range(y)
  if (!is.null(legend) & !is.null(period)) {if (legend == "top") y_mult = c(1,1.2) else y_mult = c(1,1.05)} else y_mult = c(1,1.05)

  # make an empty plot with proper labels and dimensions
  plot(x = 1, y = 1, type = "n", xlab = xlab, ylab = ylab,
       xlim = xlim + xoff, ylim = (ylim + yoff) * y_mult, axes = FALSE)

  # draw the points
  points(
    x = x, y = y, pch = 21, cex = 1.75,
    bg = scales::alpha(base_col, alpha["bg"]),
    col = scales::alpha(base_col, alpha["col"])
  )

  # label the years if requested
  if (!is.null(year)) {
    text(x = x, y = y, labels = substr(year, 3, 4), cex = 0.8)
  }

  # draw the appropriate X-axis ticks and labels
  if (x_is_percent) {
    draw_percent_axis(side = 1)
  } else {
    if (x_is_day) {
      draw_day_axis(min(x), max(x), by = 5, side = 1)
    } else {
      if (x_is_yn) {
        draw_yn_axis(side = 1)
      } else {
        axis(side = 1, col = "white", col.ticks = par("col.axis"))
        draw_axis_line(side = 1)
      }
    }
  }

  # draw the appropriate Y-axis ticks and labels
  if (y_is_percent) {
    draw_percent_axis(side = 2)
  } else {
    if (y_is_day) {
      draw_day_axis(min(y), max(y), by = 5, side = 2)
    } else {
      if (y_is_yn) {
        draw_yn_axis(side = 2)
      } else {
        axis(side = 2, col = "white", col.ticks = par("col.axis"))
        draw_axis_line(side = 2)
      }
    }
  }

  # include a legend to differentiate time periods
  if (!is.null(period) & !is.null(legend)) {
    legend (
      legend, horiz = ifelse(legend == "top", TRUE, FALSE), title = "Period",
      legend = c("6/12-6/19", "6/20-6/30", expression("">="7/1")), pch = 21,
      col = scales::alpha(c("royalblue", "orange", "salmon"), alpha["col"]),
      pt.bg = scales::alpha(c("royalblue", "orange", "salmon"), alpha["bg"]),
      pt.cex = 2, cex = 0.8, text.col = par("col.axis"),
      bty = "n"
    )
  }
}
