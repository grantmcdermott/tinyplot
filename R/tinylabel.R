#' Format labels
#' 
#' @description Internal function for formatting label appearance, e.g. axis
#' ticks labels. This is what the top-level `xaxl` and `yaxl` arguments
#' from [`tinyplot`] ultimately get passed to.
#' @param x a numeric or character vector
#' @param labeller a formatting function to be applied to `x`, e.g. [`format`],
#'   [`toupper`], [`abs`], or other custom function (including from the popular
#'   **scales** package). Can also be one of the following convenience strings
#'   (symbols), for which common formatting transformations are provided:
#'   `"percent"` (`"%"`), `"comma"` (`","`), `"log"` (`"l"`), `"dollar"`
#'   (`"$"`), `"euro"` (`"€"`), or `"sterling"` (`"£"`).
#' @examples
#' \dontrun{
#' x = 1e4
#' tinyplot:::tinylabel(x, "comma")
#' tinyplot:::tinylabel(x, ",") # same
#' tinyplot:::tinylabel(x, "$") # or "dollar"
#' 
#' # pass to xaxl/yaxl for adjusting axes tick labels in a tinyplot call
#' tinyplot(I(mpg/hp) ~ hp, data = mtcars, yaxl = "%")
#' 
#' # log example (combined with axis scaling)
#' tinyplot(y = 0:10, type = "b", log = "x", xaxl = "log")
#' 
#' #
#' ## custom function examples
#' 
#' ## example I: date formatting
#' 
#' dat = data.frame(
#'   date = seq(as.Date("2000/1/1"), by = "month", length.out = 12),
#'   trend = 1:12 + rnorm(12, sd = 1)
#' )
#' 
#' tinyplot(trend ~ date, data = dat, xaxl = function(x) format(x, "%b, %Y"))
#' 
#' ## example II: string wrapping 
#' 
#' # create a "vectorised" version of `base::strwrap` that breaks long
#' # strings into new lines every 18 characters
#' strwrap18 = function(x) sapply(
#'   strwrap(x, width = 18, simplify = FALSE),
#'   paste,
#'   collapse = "\n"
#' )
#' 
#' # now demonstrate on a dataset with long y-tick labels
#' dat2 = data.frame(
#'   x = rep(rnorm(100), 3),
#'   y = c(
#'     "tinyplot is a lightweight extension of the base R graphics system.",
#'     "R is a language for statistical computing.",
#'     "Data visualization is an essential skill."
#'   )
#' )
#' 
#' tinytheme("bw")
#' tinyplot(y ~ x, data = dat2, type = "j", yaxl = strwrap18)
#' tinytheme()
#' }
#' @keywords internal
tinylabel = function(x, labeller = NULL) {
  if (is.null(labeller)) return(x)
  if (is.character(labeller)) labeller = labeller_fun((labeller))
  return(labeller(x))
}


labeller_fun = function(label = "percent") {
  
  labels = c(
    "%"       = "percent",
    ","       = "comma",
    "$"       = "dollar",
    "\u20ac"  = "euro",
    "\u00a3"  = "sterling",
    "l"       = "log"
  )
  if (label %in% names(labels)) label = labels[label]
  
  ## all labels plus absolute value version
  # labels = c("percent", "comma", "dollar", "euro", "sterling")
  labels = c(labels, paste0("abs_", labels))

  ## match full label first, then store abs_ info separately
  label = match.arg(label, labels)
  abs_ = substr(label, 1L, 4L) == "abs_"
  if (abs_) label = substr(label, 5L, nchar(label))

  ## actual formatting function
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
  
  format_log = function(x) {
    parse(text = paste0(10, "^", format(log10(x), digits = 3)))
  }
  
  fun = switch(
    label,
    percent  = format_percent,
    comma    = format_comma,
    dollar   = format_dollar,
    euro     = format_euro,
    sterling = format_sterling,
    log      = format_log
  )

  ## combine with absolute value if necessary
  if (abs_) function(x) fun(abs(x)) else fun
}
