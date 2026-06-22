#' Add new elements to the current `tinyplot`
#'
#' @description
#' This convenience function grabs the preceding `tinyplot` call and updates it
#' with any new arguments that have been explicitly provided by the user. It
#' then injects `add=TRUE` and evaluates the updated call, thereby drawing a new
#' layer on top of the existing plot. `plt_add()` is a shorthand alias for
#' `tinyplot_add()`.
#'
#' @section Limitations:
#' - `tinyplot_add()` works reliably only when adding to a plot originally
#'   created using the [`tinyplot.formula`] method with a valid `data` argument.
#'   We cannot guarantee correct behavior if the original plot was created with
#'   the atomic [`tinyplot.default`] method, due to potential environment
#'   mismatches. (An exception is when the original plot arguments---`x`, `y`,
#'   etc.---are located in the global environment.)
#'
#' - Automatic legends for the added elements will be turned off.
#'
#' @param ... All named arguments override arguments from the previous calls.
#' Arguments not supplied to [tinyplot_add] remain unchanged from the previous
#' call. Arguments are captured unevaluated and spliced into the updated call,
#' so those that rely on non-standard evaluation against `data`---e.g.
#' `subset = cyl == 4` or `weights = wt`---behave the same as in a direct
#' [`tinyplot`] call.
#'
#' @examples
#' tinyplot(Sepal.Width ~ Sepal.Length | Species,
#'   facet = ~Species,
#'   data = iris)
#'
#' tinyplot_add(type = "lm") ## or : plt_add(type = "lm")
#'
#' ## Note: the previous function is equivalent to (but much more convenient
#' ## than) re-writing the full call with the new type and `add=TRUE`:
#'
#' # tinyplot(Sepal.Width ~ Sepal.Length | Species,
#' #          facet = ~Species,
#' #          data = iris,
#' #          type = "lm",
#' #          add = TRUE)
#'
#' ## Arguments relying on non-standard evaluation (e.g. `subset`) work too:
#' tinyplot(mpg ~ wt, data = mtcars)
#' tinyplot_add(subset = cyl == 4, col = "red", pch = 16)
#'
#' @returns No return value, called for side effect of producing a plot.
#'
#' @export
tinyplot_add = function(...) {
  cal = get_environment_variable(".last_call")

  if (is.null(cal)) {
    stop("No previous tinyplot call found.")
  }

  # Capture the dots as *unevaluated* expressions rather than evaluated values.
  # This way, arguments that rely on non-standard evaluation against `data`
  # (e.g. `subset = cyl == 4`, `weights = wt`, or a bare-name formula) are
  # spliced into the rebuilt call and only resolved when it is finally
  # evaluated through the formula method's `model.frame`. Using `list(...)`
  # here would instead force evaluation in this frame, where data-scoped
  # variables are not visible. (#630)
  args = match.call(expand.dots = FALSE)[["..."]]
  nms = names(args)
  for (i in seq_along(args)) {
    n = if (is.null(nms)) "" else nms[[i]]
    if (!is.null(n) && n != "") {
      cal[[n]] = args[[i]]
    }
  }

  # allow first argument in tinyplot_add() to be unnamed
  if (length(args) >= 1L && (is.null(nms) || nms[[1L]] == "")) {
    cal[[2]] = args[[1L]]
  }

  cal[["add"]] = TRUE
  eval(cal, envir = parent.frame())
}



#' @export
#' @name plt_add
#' @rdname tinyplot_add
plt_add = tinyplot_add
