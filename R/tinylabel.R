#' Format labels
#' 
#' @description Internal function for formatting label appearance, e.g. axis
#' ticks labels. This is what the top-level `xaxl` and `yaxl` arguments
#' ultimately get passed to.
#' @param x a numeric or character vector
#' @param labeller a formatting function to be applied to `x`, e.g. `abs`,
#'   `topper`, etc. Can also be one of the following convenience strings, for
#'   which common formatting transformations are provided: `"percent"`,
#'   `"comma"`, `"dollar"`, `"euro"`, or `"sterling"`.
#'
#' @keywords internal
tinylabel = function(x, labeller = NULL) {
  if (is.null(labeller)) return(x)
  if (is.character(labeller)) labeller = labeller_fun((labeller))
  return(labeller(x))
}


labeller_fun = function(label = c("percent", "comma", "dollar", "euro", "sterling")) {
  label = match.arg(label)
  
  format_percent = function(x) {
    sprintf("%.0f%%", x * 100)
  }
  
  format_comma = function(x) {
    prettyNum(x, big.mark = ",", scientific = FALSE)
  }
  
  format_dollar = function(x) {
    paste0("$", prettyNum(x, big.mark = ",", scientific = FALSE))
  }
  
  format_euro = function(x) {
    paste0("\u20ac", prettyNum(x, big.mark = ",", scientific = FALSE))
  }
  
  format_sterling = function(x) {
    paste0("\u00a3", prettyNum(x, big.mark = ",", scientific = FALSE))
  }
  
  switch(
    label,
    percent  = format_percent,
    comma    = format_comma,
    dollar   = format_dollar,
    euro     = format_euro,
    sterling = format_sterling
  )
}
