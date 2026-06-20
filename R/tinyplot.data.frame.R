#' tinyplot Method for Plotting Data Frames
#'
#' @description Convenience interface for visualizing
#'   \code{\link[base]{data.frames}} with tinyplot.
#'
#' @details This is a convenience function for plotting data frames with
#'   or without a formula. The case with the formula mainly facilitates
#'   using `tinyplot()` in combination with pipes. The case without
#'   formula provides a quick way of plotting the variables in a data frame:
#'   a single variable, a pair of variables, or a
#'   \code{\link[graphics]{pairs}}-style grid of all variable combinations for
#'   3 or more variables.
#'
#' @param x an object of class `"data.frame"`.
#' @param formula a \code{\link[stats]{formula}} that is passed on to
#'   \code{\link{tinyplot.formula}}. If `formula` is `NULL` a formula of
#'   type `y ~ 1` or `y ~ x` is set up for 1- and 2-variable data frames,
#'   respectively. For data frames with 3+ variables, a
#'   \code{\link[graphics]{pairs}}-style grid of all variable combinations is
#'   drawn instead.
#' @param by (3+ case only) optional grouping variable. Either 1) a character
#'   string giving a column name in `x`, or 2) a vector of equal length to the
#'   main data.frame (_caveat emptor_: this is not checked). The variable is
#'   spliced into each sub-plot's formula as `y ~ x | by` so groups are
#'   distinguished (e.g. by colour). Note that the legend is deliberately
#'   suppressed.
#' @param labs (3+ case only) logical indicating whether the axes labels
#'   (titles) for each sub-plot in the pairs-style case should be shown. Default
#'   is `FALSE`.
#' @param frames (3+ case only) logical indicating whether each sub-plot should
#'   be framed by a box. Default is `FALSE`. Note the trailing "s" plural case
#'   to disambiguate from `frame.plot`.
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
#' # cars |> tinyplot()
#' # iris |> tinyplot(Sepal.Length ~ Petal.Width | Species)
#'
#' ## pairs-style display for data frames with 3 or more variables
#' tinyplot(iris)
#'
#' ## pass `by` arg to group the pairs display (legend is suppressed)
#' ## here, we also add optional frames around the individual sub-plots
#' tinyplot(iris, by = "Species", frames = TRUE)
#' 
#' ## another option (but assumes objects of equal length)
#' tinyplot(iris[, 1:4], by = iris$Species, frames = TRUE)
#'
#' tinytheme() ## reset theme
#'
#' @importFrom stats reformulate
#' @export
tinyplot.data.frame = function (x, formula = NULL, by = NULL, labs = FALSE, frames = FALSE, ...) {
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

  ## backtick-protect names so reformulate() parses non-syntactic names (e.g.
  ## "GDP (2020)") as single symbols rather than as code, matching pairs().
  bt = function(x) paste0("`", x, "`")

  if (is.null(formula) && n > 2L) {

    ## 3d: pairs-esque
    op = par(mfrow = c(n, n))
    on.exit(par(op))

    assert_logical(labs)
    assert_logical(frames)
    if (!is.null(by)) {
      if (inherits(by, "character")) {
        ## a column name already in `x`
        assert_choice(by, nm)
      } else {
        ## a standalone vector: splice it into `x` under a unique name so the
        ## formula method can reference it. make.unique() guards against an
        ## existing "__by__" column. The name is added after `n` is computed,
        ## so it is used for grouping only and not drawn as its own row/column.
        by_nm = make.unique(c(nm, "__by__"))[n + 1L]
        x[[by_nm]] = by
        by = by_nm
        cl[["data"]] = x
      }
      ## `by` is spliced directly into each cell formula below, so drop it from
      ## the call. Suppress the legend to avoid repeating it across every cell.
      if ("by" %in% names(cl)) cl[["by"]] = NULL
      cl[["legend"]] = FALSE
    }

    ## To scale cex like faceted plots, we capture the existing theme and apply
    ## temporary cex overrides (restoring on exit). The theme is built as a
    ## language object because `cl` is a matched call, so `cl[["theme"]]` is
    ## unevaluated (e.g. the call `list("dark")`, not a list).
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
    ## drop any `theme` from the cell call so it isn't re-applied per cell (see
    ## above). NULL-assignment on a call errors if the element is absent, so
    ## guard with a membership check.
    if ("theme" %in% names(cl)) cl[["theme"]] = NULL

    ## apply temp theme overrides and restore orig theme on exit
    do.call(tinytheme, eval(theme_ij))
    on.exit(if (identical(active_theme, "default")) tinytheme() else tinytheme(active_theme), add = TRUE)

    for (j in 1L:n) {
      for (i in 1L:n) {
        cl_ij = cl
        grp = if (is.null(by)) "" else paste(" |", bt(by))
        if (i == j) {
          cl_ij$formula = reformulate(paste0("1", grp), bt(nm[i]))
          cl_ij$main = nm[i]
        } else {
          cl_ij$formula = reformulate(paste0(bt(nm[i]), grp), bt(nm[j]))
        }
        if (!labs) {
          cl_ij$ylab = NA
          cl_ij$xlab = NA
        }
        eval.parent(cl_ij)
        if (frames) box("figure", lwd = 0.3)
      }
    }

  } else {

    ## `by` only applies to the >=3 (pairs-style) case; drop it so it isn't
    ## passed through to tinyplot() here.
    if ("by" %in% names(cl)) cl[["by"]] = NULL

    ## default formula
    ## 1d: y ~ 1
    ## 2d: y ~ x
    if (is.null(formula)) {
      cl$formula = if (length(nm) < 2L) reformulate("1", bt(nm)) else reformulate(bt(nm[1L]), bt(nm[2L]))
    }

    ## evaluate updated call
    eval.parent(cl)
  }
}
