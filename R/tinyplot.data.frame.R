#' tinyplot Method for Plotting Data Frames
#'
#' @description Convenience interface for visualizing data.frame objects
#'   with tinyplot.
#'
#' @details This is a convenience function for plotting data frames with
#'   or without a formula. The case with the formula mainly facilitates
#'   using `tinyplot()` in combination with pipes. The case without
#'   formula provides a quick way of plotting the variables in data frames,
#'   either only one variable or a pair of variables, or all possible pairs
#'   of variables.
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
#' ## using tinyplot() with data frames
#' tinyplot(cars)
#' tinyplot(iris, Sepal.Length ~ Petal.Width | Species)
#'
#' ## note that this also enables usage with pipes (in R >= 4.1.0) such as
#' ## cars |> tinyplot()
#' ## iris |> tinyplot(Sepal.Length ~ Petal.Width | Species)
#'
#' ## pairs-style display for more than 2 variables
#' ## (handling of axes and their labels will be improved in future versions)
#' tinyplot(iris)
#'
#' tinytheme() ## reset
#'
#' @importFrom stats reformulate
#' @export
tinyplot.data.frame = function (x, formula = NULL, ...) {
  ## original call
  cl = match.call()

  ## update call for formula interface
  names(cl)[names(cl) == "x"] = "data"
  cl[[1L]] = quote(tinyplot::tinyplot)
  if (is.null(formula)) cl$formula = . ~ .
  id = which(names(cl) == "formula")
  cl = as.call(as.list(cl)[c(1L, id, setdiff(2L:length(cl), id))])
  
  ## variables
  nm = names(x)
  n = length(nm)

  if (is.null(formula) & n > 2L) {

    ## 3d: pairs-esque  
    op = par(mfrow = c(n, n))
    for (j in 1L:n) {
      for (i in 1L:n) {
        cl_ij = cl
        if (i == j) {
          cl_ij$formula = reformulate("1", nm[i])
          cl_ij$xlab = ""
          cl_ij$ylab = ""
          cl_ij$main = nm[i]
        } else {
          cl_ij$formula = reformulate(nm[i], nm[j])
          if (i > 1L) cl_ij$ylab = ""
          if (j < n)  cl_ij$xlab = ""
        }
        eval.parent(cl_ij)
      }
    }
    par(op)

  } else {

    ## default formula
    ## 1d: y ~ 1
    ## 2d: y ~ x
    if (is.null(formula)) {
      cl$formula = if (length(nm) < 2L) reformulate("1", nm) else reformulate(nm[1L], nm[2L])
    }

    ## evaluate updated call
    eval.parent(cl)
  }
}
