## set up empty environment for storing parameters/calls/hooks/...
.tinyplot_env = new.env()

## query environment
get_environment_variable = function(name = NULL) {
  ## either one 'name' or entire environment as list
  if(is.null(name)) return(as.list(.tinyplot_env))
  name = as.character(name)[1L]
  return(.tinyplot_env[[name]])
}

## update environment
set_environment_variable = function(...) {
  ## check for unnamed arguments
  dots = list(...)
  if(is.null(names(dots))) {
    stop("arguments must be named")
  } else if(any(names(dots) == "")) {
    warning("ignoring unnamed arguments")
    dots = dots[names != ""]
  }
  
  ## set environment variables
  if(length(dots) > 0L) {
    for(i in names(dots)) {
      .tinyplot_env[[i]] = dots[[i]]
    }
  }
  invisible(dots)
}

## initialize all environment variables with NULL
set_environment_variable(
  .saved_par_before = NULL,
  .saved_par_after = NULL,
  .saved_par_first = NULL,
  .last_call = NULL,
  .tpar_hooks = NULL
)
