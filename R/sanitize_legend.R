sanitize_legend = function(legend, legend_args) {
  if (is.null(legend_args[["x"]])) {
    
    # Normalize legend to a list
    largs = if (is.null(legend)) {
      list(x = "right!")
    } else if (is.character(legend)) {
      list(x = legend)
    } else if (is.list(legend)) {
      # Handle unnamed first element as position
      if (length(legend) >= 1 && is.character(legend[[1]]) && 
          (is.null(names(legend)) || names(legend)[1] == "")) {
        names(legend)[1] = "x"
      }
      legend
    } else if (inherits(legend, c("call", "name"))) {
      # Convert call to list and handle unnamed first arg as position
      new_legend = as.list(legend)[-1]  # Remove function name
      if (length(new_legend) >= 1 && (is.null(names(new_legend)) || names(new_legend)[1] == "")) {
        names(new_legend)[1] = "x"
      }
      new_legend
    } else {
      list(x = "right!")  # Fallback
    }
    
    # Ensure position exists
    if (is.null(largs[["x"]])) largs[["x"]] = "right!"
    
    # Merge
    legend_args = modifyList(legend_args, largs, keep.null = TRUE)
  }
  
  legend_args
}
