#' Plot mean and standard error of `y` at unique values of `x`
#'
#' @md
#' @description
#' Plots the mean + standard error of a numeric variable by a grouping variable.

#' @param conf.int confidence error to plot the standard error. Defaults to .95
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#' values should be stripped before the computation proceeds Defaults to TRUE.
#' @param errorbar a logical evaluating to TRUE or FALSE indicating whether 
#' errorbars should be drawn. If FALSE, it will draw a pointrange. Defaults to
#' TRUE
#' @param ... Additional arguments are passed to the `draw_pointrange()`
#' or `draw_errorbar()` function, ex: `col="pink"`.
#' @examples
#' # Plot the mean and standard error of miles per gallon by cylinders
#' tinyplot(mpg ~ cyl, data = mtcars, data = mtcars, type = "mean_se")
#'

#'
#' # Use 99% confidence intervals
#' tinyplot(mpg ~ cyl, data = mtcars, type = type_mean_se(conf.int = .99))
#'
#' # Works with groups and/or facets too
#' tinyplot(mpg ~ cyl | gear, facet = "by", data = mtcars, type = "mean_se")
#'
#'
#' @export
type_mean_se = function(conf.int = .95, na.rm = TRUE, errorbar = TRUE, 
                        dodge = 0, fixed.dodge = FALSE, ...) {
  pointrange_args = list(...)

  # function to get mean_se out of two vectors
  mean_se_internal <- function(var, group, na.rm = TRUE, conf.int = .95) {
    # just one group

    mean_se_basic <- function(z, na.rm = na.rm, conf.int = conf.int) {
      m <- mean(z, na.rm = na.rm)
      se <- sd(z, na.rm = na.rm) / sqrt(length(z))

      mult <- qnorm(1 - ((1 - conf.int) / 2))

      conf.low <- m - se * mult
      conf.high <- m + se * mult

      out <- data.frame(m = m, ymin = conf.low, ymax = conf.high)

      # pending - change colnames

      return(out)
    }

    if (missing(group)) {
      mean_se_basic(z = var, na.rm = na.rm, conf.int = conf.int)
    } else {
      out <- tapply(var, group, \(x) {
        mean_se_basic(x, na.rm = na.rm, conf.int = conf.int)
      })

      out <- do.call("rbind", out)

      out$group <- row.names(out)
      row.names(out) <- NULL

      out <- out[, c(4, 1, 2, 3)]

      return(out)
    }
  }

  data_mean_se = function(fun = mean_se_internal) {
    funky = function(settings, ...) {
      env2env(settings, environment(), c("datapoints", "by", "facet"))

      datapoints = split(
        datapoints,
        list(datapoints$facet, datapoints$by),
        drop = TRUE
      )
      datapoints = lapply(datapoints, function(dat) {
        ms <- mean_se_internal(dat$y, dat$x)
        colnames(ms) <- c("x", "y", "ymin", "ymax")
        ms$xmin <- ms$x
        ms$xmax <- ms$x
        ms$by <- dat$by[1]
        ms$facet <- dat$facet[1]
        ms = ms[order(ms$x), ]
        return(ms)
      })
      datapoints = do.call(rbind, datapoints)
      
      
      if (is.character(datapoints$x)) {
        datapoints$x = as.factor(datapoints$x)
      }
      if (is.factor(datapoints$x)) {
        ## original data (i.e., no new sorting by factor)
        xlvls = unique(datapoints$x)
        datapoints$x = factor(datapoints$x, levels = xlvls)
        xlabs = seq_along(xlvls)
        names(xlabs) = xlvls
        datapoints$x = as.integer(datapoints$x)
      }
      datapoints$xmin = datapoints$x
      datapoints$xmax = datapoints$x
      
      if (is.character(datapoints$x)) {
        datapoints$x = as.factor(datapoints$x)
      }
      if (is.factor(datapoints$x)) {
        ## original data (i.e., no new sorting by factor)
        xlvls = unique(datapoints$x)
        datapoints$x = factor(datapoints$x, levels = xlvls)
        xlabs = seq_along(xlvls)
        names(xlabs) = xlvls
        datapoints$x = as.integer(datapoints$x)
      }
      datapoints$xmin = datapoints$x
      datapoints$xmax = datapoints$x
      
      # dodge
      if (dodge != 0) {
        datapoints = dodge_positions(datapoints, dodge, fixed.dodge)
      }

      
      x = datapoints$x
      

      env2env(environment(), settings, c(
        "x",
        "xlabs",
        "datapoints"
      ))
    }
      
    
    return(funky)
  }
  if (errorbar) {
    out = list(
      draw = draw_errorbar(...),
      data = data_mean_se(fun = fun),
      name = "p"
    )
  }
  else {
    out = list(
      draw = draw_pointrange(...),
      data = data_mean_se(fun = fun),
      name = "p"
    )
  }

  class(out) = "tinyplot_type"
  return(out)
}
