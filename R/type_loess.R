#' Local polynomial regression plot type
#'
#' @description Type function for plotting a LOESS (LOcal regrESSion) fit.
#' Arguments are passed to \code{\link[stats]{loess}}.
#'
#' @inheritParams type_glm
#' @inheritParams stats::loess
#' @param se logical. If `TRUE` (the default), confidence intervals are drawn.
#' @param level the confidence level required if `se = TRUE`. Default is 0.95.
#' @param n integer. Number of equally-spaced points at which the fitted model
#' is evaluated for drawing, per group. Default is 100. Note that this affects
#' only where the fitted curve is sampled, not the fit itself, which always
#' uses every observation. Larger values trace a wiggly fit more faithfully, at
#' proportionally greater cost; consider raising it if you combine a small
#' `span` with a large dataset.
#' @section Plotting large data:
#' \code{\link[stats]{loess}} can be a time-consuming algorithm. From a
#' plotting perspective, though, the expensive half is usually not the model fit
#' but the _prediction_ step (`y` values corresponding to each `x`).
#' `type_loess()` mitigates this in two ways.
#' 
#' First, the fitted model is evaluated on an equally-spaced grid of `n` points
#' spanning the data range of each group, rather than at every observation. On
#' large data this skips a great deal of redundant work---dozens of observations
#' can crowd into a single pixel, so evaluating the curve at each of them costs
#' time without revealing anything---while `n` points remain ample to trace a
#' smooth curve. The model is still fitted on the full dataset, so the estimates
#' are unchanged. It's only the points along `x` at which they are read off that
#' differ. (If the x-axis is logarithmic, the grid is spaced evenly in `log(x)`,
#' so that the curve is traced evenly across the plot.)
#'
#' Second, once prediction is cheap the fit itself becomes the bottleneck, and
#' here \code{\link[stats]{lowess}} is substituted for `loess()` wherever the
#' two are equivalent: unweighted local linear fits, without standard errors, on
#' a non-logarithmic x-axis. That path smooths millions of observations in
#' seconds. Standard errors, quadratic fits and weighted fits all fall back to
#' `loess()`, as does a logarithmic x-axis, where `lowess()` interpolates too
#' coarsely through the compressed decades.
#'
#' Note that `se = TRUE` carries a hard ceiling of its own:
#' \code{\link[stats]{predict.loess}}'s workspace grows quadratically in the
#' number of observations---regardless of how many points you predict at---and
#' fails outright at around 38,000. Confidence intervals are thus unavailable at
#' the sizes where the `lowess()` path matters most, and `se = FALSE` is
#' required to draw a smooth over very large data.
#' @importFrom stats approx loess loess.control lowess predict
#' @examples
#' # "loess" type convenience string
#' tinyplot(dist ~ speed, data = cars, type = "loess")
#'
#' # Use `type_loess()` to pass extra arguments for customization
#' tinyplot(dist ~ speed, data = cars, type = type_loess(span = 0.5, degree = 1))
#'
#' # The fast `lowess` path is used automatically where it is equivalent, which
#' # makes short work of larger datasets
#' tinyplot(sunspots, type = "l", col = "grey")
#' tinyplot_add(type = type_loess(span = 0.05, degree = 1, se = FALSE), lwd = 2)
#'
#' # Raise `n` if a small `span` needs more than 100 points to trace faithfully
#' tinyplot(sunspots, type = "l", col = "grey")
#' tinyplot_add(type = type_loess(span = 0.05, degree = 1, se = FALSE, n = 500), lwd = 2)
#' @export
type_loess = function(
    span = 0.75,
    degree = 2,
    family = "gaussian",
    control = loess.control(),
    se = TRUE,
    level = 0.95,
    n = 100,
    weights = NULL) {
    assert_integerish(n, len = 1, lower = 2, name = "n")
    out = list(
        draw = draw_ribbon(),
        data = data_loess(span = span, degree = degree, family = family, control = control, se = se, level = level, n = n, weights = weights),
        name = if (isTRUE(se)) "ribbon" else "l"
    )
    class(out) = "tinyplot_type"
    return(out)
}


data_loess = function(span, degree, family, control, se, level, n = 100, weights = NULL, ...) {
    fun = function(settings, ...) {
        env2env(settings, environment(), "datapoints")
        # top-level `weights` (carried on datapoints via NSE) take precedence
        # over the constructor-level `weights` argument
        if (is.null(datapoints[["weights"]]) && !is.null(weights)) {
            datapoints[["weights"]] = weights
        }
        has_weights = !is.null(datapoints[["weights"]])
        if (has_weights) settings$weights_used = TRUE
        # lowess() defaults to robustness iterations where loess() does not, so
        # the two only agree if we set `iter` from the requested loess family.
        # Note that the counts differ by one: loess's `iterations` includes the
        # initial fit, whereas lowess's `iter` counts only the reweightings.
        robust = identical(match.arg(family, c("gaussian", "symmetric")), "symmetric")
        iter = if (robust) max(0L, (control[["iterations"]] %||% 4L) - 1L) else 0L
        # lowess() interpolates between fit points spaced `delta` apart in raw x.
        # That is invisible on a linear axis, even for badly skewed x, since the
        # dense region is compressed into a narrow strip. A log axis stretches
        # that region across the plot, where the interpolation dominates (we
        # measured errors up to 37% of the y-range), so the shortcut is off there.
        xlog = grepl("x", settings$log %||% "", fixed = TRUE)
        datapoints = split(datapoints, list(datapoints$facet, datapoints$by))
        datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        datapoints = lapply(datapoints, function(dat) {
            # .w is NULL when no weights column is present, which loess() treats
            # the same as omitting the argument
            .w = dat[["weights"]]
            # the fit below still uses every observation in `dat`; the grid
            # only governs where we then read the fitted curve off, which is
            # the expensive half on large data (and yields a smoother curve)
            nd = data.frame(x = model_grid(dat$x, n, settings$log))
            nd$by = dat$by[1]
            nd$facet = dat$facet[1]
            # fast path: lowess() is only equivalent to loess() for unweighted,
            # local linear fits on a linear axis, and cannot return standard errors
            if (!isTRUE(se) && degree == 1 && is.null(.w) && !xlog) {
                lw = lowess(dat$x, dat$y, f = span, iter = iter)
                # lowess() returns sorted x, with a single fitted value per tied
                # x, so `ties = "ordered"` skips a redundant (and noisy) regularize
                nd$y = approx(lw$x, lw$y, xout = nd$x, ties = "ordered")$y
                return(nd)
            }
            fit = loess(y ~ x, data = dat, span = span, degree = degree, family = family, control = control, weights = .w)
            if (se == TRUE) {
                p = predict(fit, newdata = nd, se = TRUE)
                p = ci(p$fit, p$se.fit, conf.level = level, p$df)
                nd$y = p$estimate
                nd$ymax = p$conf.high
                nd$ymin = p$conf.low
            } else {
                nd$y = predict(fit, nd)
            }
            nd
        })
        datapoints = do.call(rbind, datapoints)
        datapoints = datapoints[order(datapoints$facet, datapoints$by, datapoints$x), ]
        
        # legend customizations - same as ribbon but add line through square
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["pt.lwd"]] = settings$legend_args[["pt.lwd"]] %||% 0
        settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% par("lty")
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25
        
        env2env(environment(), settings, "datapoints")
    }
    return(fun)
}
