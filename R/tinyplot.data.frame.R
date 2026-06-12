#' tinyplot Method for Plotting Data Frames
#'
#' @description Convenience interface for visualizing data.frame objects
#'   with tinyplot.
#'
#' @details This is a convenience function for plotting data frames with
#'   or without a formula. The case with the formula mainly facilitates
#'   using `tinyplot()` in combination with pipes. The case without
#'   formula provides a quick way of plotting the variables in data frames,
#'   either only one variable or a pair of variables. In the future, the
#'   latter might be extended to a pairs display for data frames with
#'   more than two variables but this is not implemented, yet. See the
#'   examples for illustrations.
#'
#' @param x an object of class `"data.frame"`.
#' @param formula a \code{\link[stats]{formula}} that is passed on to
#'   \code{\link{tinyplot.formula}}. If `formula` is `NULL` a formula of
#'   type `y ~ 1` or `y ~ x` is set up for 1- and 2-dimensional data frames,
#'   respectively. For data frames with more than 2 variables the `facet = NULL`
#'   case is not supported, yet, and currently leads to an error.
#' @param ... further arguments passed to `tinyplot`. 
#'
#' @examples
#' tinytheme("clean2")
#' 
#' ## using tinyplot() with data frames and pipes
#' cars |> tinyplot()
#' iris |> tinyplot(Sepal.Length ~ Petal.Width | Species)
#'
#' ## tinyplot(df) only works for data frames with 1 or 2 variables
#' ## in the future we might add a pairs-style display as follows
#' ## but this would require better handling of axes and their labels
#' par(mfrow = c(5, 5))
#' for (i in names(iris)) for(j in names(iris)) tinyplot(iris[, unique(c(j, i)), drop = FALSE])
#'
#' tinytheme() ## reset
#'
#' @export
tinyplot.data.frame = function (x, formula = NULL, ...) {
  ## original call
  cl = match.call()

  ## default formula
  ## 1d: y ~ 1
  ## 2d: y ~ x
  ## 3d: pairs? (not supported, yet)
  if (is.null(formula)) {
    if (ncol(x) > 2L) stop("'formula' is missing with no default for more than 2 columns")
    f = names(x)
    if (length(f) < 2L) f = c("1", f)
    cl$formula = as.formula(paste(f[2L:1L], collapse = " ~ "))
  }

  ## evaluate updated call
  names(cl)[names(cl) == "x"] = "data"
  cl[[1L]] = quote(tinyplot::tinyplot)
  eval.parent(cl)
}
