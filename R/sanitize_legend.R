sanitize_legend = function(legend, legend_args) {
  if (is.null(legend_args[["x"]])) {
    if (is.null(legend)) {
      legend_args[["x"]] = "right!"
    } else if (is.character(legend)) {
      legend_args = modifyList(legend_args, list(x = legend))
    } else if (is.list(legend)) {
      if (is.null(legend[["x"]])) legend[["x"]] = "right!"
      legend_args = modifyList(legend_args, legend, keep.null = TRUE)
    } else if (class(legend) %in% c("call", "name")) {
      largs = as.list(legend)
      if (is.null(largs[["x"]])) {
        lnms = names(largs)
        # check second position b/c first will be a symbol
        if (is.null(lnms)) {
          largs = setNames(largs, c("", "x"))
        } else if (length(largs) >= 2 && lnms[2] == "") {
          lnms[2] = "x"
          largs = setNames(largs, lnms)
        } else {
          largs[["x"]] = "right!"
        }
      }
      # Finally, combine with any pre-existing legend args (e.g., title from the by label)
      legend_args = modifyList(legend_args, largs, keep.null = TRUE)
    }
  }
  return(legend_args)
}
