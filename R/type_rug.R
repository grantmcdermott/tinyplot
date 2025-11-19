#' Add a rug to a plot
#' 
#' @description
#' Adds a rug representation (1-d plot) of the data to the plot.
#' 
#' @details
#' This function should only be used as part of [`tinyplot_add()`], i.e. adding
#' to an existing plot.
#' 
#' In most cases, determining which variable receives the rug representation
#' will be based on the `side` argument (i.e., x-variable if side is 1 or 3, and
#' y-variable if side is 2 or 4). An exception is if the preceding plot type was
#' either `"density"` or `"histogram"`; for these latter cases, the x-variable
#' will always be used. See Examples.
#' 
#' @inheritParams graphics::rug
#' @param jitter Logical. Add jittering to separate ties? Default is `FALSE`.
#' @param amount Numeric. Amount of jittering (see \code{\link[base]{jitter}}).
#'   Only used if `jitter` is `TRUE`.
#' @examples
#' tinyplot(~wt | am, data = mtcars, type = "density", facet = "by", fill = "by")
#' tinyplot_add(type = "rug")
#' # use type_rug() to pass extra options
#' tinyplot_add(type = type_rug(side = 3, ticksize = 0.05))
#' 
#' # For ties, use jittering
#' tinyplot(eruptions ~ waiting, data = faithful, type = "lm")
#' tinyplot_add(type = type_rug(jitter = TRUE, amount = 0.3))
#' tinyplot_add(type = type_rug(jitter = TRUE, amount = 0.1, side = 2))
#' # Add original points just for reference
#' tinyplot_add(type = "p")
#' 
#' @importFrom graphics rug
#' @export
type_rug = function(ticksize = 0.03, side = 1, quiet = getOption("warn") < 0, jitter = FALSE, amount = NULL) {
  data_rug = function(settings, ...) {
    list2env(settings["datapoints"], envir = environment())
    if (nrow(datapoints) == 0) {
      msg = "`type_rug() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }

    out = update_settings(settings, datapoints = datapoints)
    return(out)
  }
  draw_rug = function(.ticksize = ticksize, .side = side, .quiet = quiet, .jitter = jitter, .amount = amount) {
      fun = function(ix, iy, icol, ilwd, ...) {
        lc = get_environment_variable(".last_call")
        swapy = !is.null(lc$type) && lc$type %in% c("density", "hist", "histogram")
        rugx = if (swapy) iy else if (side %in% c(1, 3)) ix else iy
        if (isTRUE(jitter)) rugx = jitter(rugx, amount = .amount)
        rug(
          x = rugx,
          col = icol,
          lwd = ilwd,
          ticksize = .ticksize,
          side = .side,
          quiet = .quiet
        )
      }
      return(fun)
  }

  out = list(
    draw = draw_rug(),
    data = data_rug,
    name = "rug"
  )
  class(out) = "tinyplot_type"
  return(out)
}
