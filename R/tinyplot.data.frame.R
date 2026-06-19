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
    on.exit(par(op))

    ## We'll (manually) scale cex elements similar to faceted plots.
    ## Safest way is to capture the existing theme and then inject temporary
    ## overrides via an ephemeral theme. The theme is built as a language object
    ## because `cl` is a matched call, so `cl[["theme"]]` is unevaluated (e.g.
    ## the call `list("dark")`, not a list).
    cex_fct_adj = 0.66 # use same scaling as with faceted plots.
    active_theme = get_tpar("tinytheme", default = "default")
    theme_arg = cl[["theme"]]
    if (is.null(theme_arg)) {
      theme_ij = bquote(list(.(active_theme), cex = .(cex_fct_adj)))
    } else if (is.call(theme_arg) && identical(theme_arg[[1L]], as.name("list"))) {
      theme_arg[["cex"]] = cex_fct_adj
      theme_ij = theme_arg
    } else {
      theme_ij = bquote(list(.(theme_arg), cex = .(cex_fct_adj)))
    }

    ## Ephemeral themes revert to the *default* theme on exit, so after the loop
    ## we must re-assert any persistent theme that was active beforehand.
    if (!identical(active_theme, "default")) {
      on.exit(tinytheme(active_theme), add = TRUE)
    }

    for (j in 1L:n) {
      for (i in 1L:n) {
        cl_ij = cl
        cl_ij[["theme"]] = theme_ij
        if (i == j) {
          cl_ij$formula = reformulate("1", nm[i])
          # cl_ij$xlab = NA
          # cl_ij$ylab = NA
          cl_ij$main = nm[i]
        } else {
          cl_ij$formula = reformulate(nm[i], nm[j])
          # if (i > 1L) cl_ij$ylab = NA
          # if (j < n)  cl_ij$xlab = NA
        }
        # GM: drop all axes labs (like pairs)
        cl_ij$ylab = NA
        cl_ij$xlab = NA
        eval.parent(cl_ij)
        # GM: add box around each plot
        box("figure", lwd = 0.3)
      }
    }

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
