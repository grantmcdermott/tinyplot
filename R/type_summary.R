#' Plot summary values of `y` at unique values of `x`
#'
#' @md
#' @details
#' Applies a summary function to `y` along unique values of `x`. This is useful,
#' say, for quickly plotting mean values of your dataset. Internally,
#' `type_summary()` applies a thin wrapper around \code{\link[base]{ave}} and
#' then passes the result to [`type_lines`] for drawing.
#'
#' @param fun summarizing function. Should be compatible with
#'   \code{\link[ave]{ave}}. Defaults to \code{\link[stats]{mean}}.
#' @param ... Additional arguments are passed to the `lines()` function,
#' ex: `type="p"`, `col="pink"`.
#' @seealso [`ave`] which performs the summarizing (averaging) behind the
#'   scenes.
#' @examples
#' # Plot the mean chick weight over time
#' tinyplot(weight ~ Time, data = ChickWeight, type = type_summary())
#' 
#' # mean is the default function, so the above is equivalent to
#' tinyplot(weight ~ Time, data = ChickWeight, type = type_summary(mean))
#' 
#' # Plot the median instead
#' tinyplot(weight ~ Time, data = ChickWeight, type = type_summary(median))
#' 
#' # Custom function example
#' tinyplot(
#'   weight ~ Time, data = ChickWeight,
#'   type = type_summary(function(y) quantile(y, probs = 0.9))
#' )
#' 
#' # Works with groups and/or facets too
#' tinyplot(weight ~ Time | Diet, facet = "by", data = ChickWeight, type = type_summary())
#'
#' @export
type_summary = function(fun = mean, ...) {
  assert_function(fun)
  lines_args = list(...)
  data_summary = function(fun) {
    funky = function(datapoints, ...) {
      datapoints = split(datapoints, list(datapoints$facet, datapoints$by), drop = TRUE)
      datapoints = lapply(datapoints, function(dat) {
        newy = ave(dat$y, dat$x, FUN = fun)
        dat$y = newy
        dat = dat[order(dat$x), ]
        return(dat)
      })
      datapoints = do.call(rbind, datapoints)
      out = list(datapoints = datapoints)
      return(out)
    }
    return(funky)
  }
  out = list(
    draw = draw_lines(...),
    data = data_summary(fun = fun),
    name = "l"
  )
  class(out) = "tinyplot_type"
  return(out)
}
