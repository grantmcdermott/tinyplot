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
#' call.
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
#' @returns No return value, called for side effect of producing a plot.
#'
#' @export
tinyplot_add = function(...) {
  cal = getOption("tinyplot_last_call", default = NULL)

  ## TODO: remove the global option above and move to this when density is refactored
  # cal = get(".last_call", envir = get(".tinyplot_env", envir = parent.env(environment())))

  if (is.null(cal)) {
    stop("No previous tinyplot call found.")
  }

  args = list(...)
  for (n in names(args)) {
    if (n != "") {
      cal[[n]] = args[[n]]
    }
  }

  # allow first argument in tinyplot_add() to be unnamed
  if (isTRUE(names(args)[1] == "")) {
    cal[[2]] = args[[1]]
  }

  cal[["add"]] = TRUE
  eval(cal, envir = parent.frame())
}



#' @export
#' @name plt_add
#' @rdname tinyplot_add
plt_add = tinyplot_add
