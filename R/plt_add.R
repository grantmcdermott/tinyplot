#' Add to the last plot
#' 
#' @export
plt_add <- function(...) {
  cal <- getOption("tinyplot_last_call")
  args <- list(...)
  for (n in names(args)) {
    cal[[n]] <- args[[n]]
  }
  cal[["add"]] <- TRUE
  eval(cal)
}

