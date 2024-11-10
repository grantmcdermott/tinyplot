#' Add new elements to the current `tinyplot`
#'
#' @description
#' This function grabs the last `tinyplot` call, sets `add=TRUE`, and 
#' overwrites some of the arguments with those explicitly supplied by the 
#' user. Then, the call is evaluated again to add a layer to an existing plot.
#'
#' @details
#' Note: the automatic legend for the added elements will be turned off.
#'
#' @param ... All named arguments override arguments from the previous calls.
#' Arguments not supplied to `tinyplot_add()` remain unchanged from the previous call.
#'
#' @examples
#' library(tinyplot)
#' 
#' tinyplot(Sepal.Width ~ Sepal.Length | Species,
#'          facet = ~Species,
#'          data = iris, 
#'          type = type_lm())
#' 
#' tinyplot_add(type = "p")
#'
#' @returns No return value, called for side effect of producing a plot.
#'
#' @export
tinyplot_add <- function(...) {
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
  cal[["add"]] = TRUE
  eval(cal)
}



#' @export
#' @name plt_add
#' @rdname tinyplot_add
plt_add = tinyplot_add
