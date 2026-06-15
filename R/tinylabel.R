#' Format labels
#'
#' @description Function for formatting label appearance, e.g. axis
#' ticks labels. This is what the top-level `xaxl` and `yaxl` arguments
#' from [`tinyplot`] ultimately get passed to.
#' @param x a numeric or character vector
#' @param labeller a formatting function to be applied to `x`, e.g. [`format`],
#'   [`toupper`], [`abs`], or other custom function (including from the popular
#'   **scales** package). Can also be one of the following convenience strings
#'   (symbols), for which common formatting transformations are provided:
#'   `"percent"` (`"%"`), `"comma"` (`","`), `"log"` (`"l"`), `"dollar"`
#'   (`"$"`), `"euro"` (`"€"`), or `"sterling"` (`"£"`).
#' @param na.ignore logical indicating whether the labelling function should
#'   ignore `NA` values in `x`. In other words, should the `NA` values be left
#'   as-is? Default is `TRUE`.
#' @param na.rm logical indicating whether `NA` values should be removed from
#'   `x` and thus the return object too. Default is `TRUE`, but only evaluated
#'   if `na.ignore` is `FALSE`.
#' @return a character vector of the same length as `x` with the transformed
#'   labels.
#' @export
#' @seealso [`tinyplot`]
#' @examples
#' x = 1e4
#' tinylabel(x, "comma")
#' tinylabel(x, ",") # same
#' tinylabel(x, "$") # or "dollar"
#' 
#' # invoke tinylabel from a parent tinyplot call...
#' #   => x/yaxl for adjusting axes tick labels
#' #   => legend = list(labeller = ...) for adjusting the legend labels
#' s77 = transform(data.frame(state.x77), Illiteracy = Illiteracy / 100)
#' tinyplot(Life.Exp ~ Income | Illiteracy, data = s77,
#'          xaxl = '$',
#'          legend = list(labeller = '%'))
#'
#' # log example (combined with axis scaling)
#' tinyplot(x = 10^c(10:0), y = 0:10, type = "b",
#'          log = "x", xaxl = "log")
#'
#' # combine with `x/yaxb` to adjust the actual tick marks ("break points")
#' # at the same time
#' tinyplot(x = 10^c(10:0), y = 0:10, type = "b",
#'          log = "x", xaxl = "log", xaxb = 10^c(1,3,5,7,9))
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
#' tinyplot(trend ~ date, data = dat,
#'          xaxl = function(x) format(x, "%b, %Y"))
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
#' tinyplot(y ~ x, data = dat2, type = "j",
#'          yaxl = strwrap18,
#'          theme = "bw") # use theme for horizontal labels + dynamic margin
#' @export
tinylabel = function(x, labeller = NULL, na.ignore = TRUE, na.rm = TRUE) {
  if (is.null(labeller)) {
    return(x)
  }
  assert_logical(na.ignore)
  assert_logical(na.rm)
  xidx = if (na.ignore) {
    which(!is.na(x))
  } else if (na.rm) {
    !is.na(x)
  } else {
    seq_along(x)
  }
  if (is.character(labeller)) {
    labeller = labeller_fun((labeller))
  }
  # don't need to subset if everything is being used. DateTime also require
  # exception logic (e.g., date format needs to be consistent for whole vector)
  if (length(xidx) == length(x) || inherits(x, c("POSIXt", "Date"))) {
    x = labeller(x)
  } else {
    x[xidx] = labeller(x[xidx])
  }
  return(x)
}


labeller_fun = function(label = "percent") {
  labels = c(
    "%"      = "percent",
    ","      = "comma",
    "$"      = "dollar",
    "\u20ac" = "euro",
    "\u00a3" = "sterling",
    "l"      = "log"
  )
  if (label %in% names(labels)) {
    label = labels[label]
  }

  ## all labels plus absolute value version
  # labels = c("percent", "comma", "dollar", "euro", "sterling")
  labels = c(labels, paste0("abs_", labels))

  ## match full label first, then store abs_ info separately
  label = match.arg(label, labels)
  abs_ = substr(label, 1L, 4L) == "abs_"
  if (abs_) {
    label = substr(label, 5L, nchar(label))
  }

  ## actual formatting functions

  # Find the smallest number of decimal places (from 0:max_decimals) that keeps
  # the unique values distinct, so a single consistent format can be applied to
  # the whole vector. Falls back to max_decimals.
  consistent_decimals = function(x, max_decimals = 5L) {
    ux = unique(as.numeric(x))
    Find(
      function(d) {
        length(unique(sprintf(paste0("%.", d, "f"), ux))) == length(ux)
      },
      0:max_decimals
    ) %||%
      max_decimals
  }

  format_percent = function(x) {
    pct = as.numeric(x) * 100
    d = consistent_decimals(pct)
    sprintf(paste0("%.", d, "f%%"), pct)
  }

  # Currency convention: keep clean integers integer-valued, but show at least
  # two decimal places whenever any fractional component is present (e.g.
  # "$0.50" rather than "$0.5"). Negative values place the sign in front of the
  # currency symbol (e.g. "-$1.50" rather than "$-1.50"). The comma formatter is
  # not currency, so it just uses the smallest consistent number of decimals.
  format_currency = function(x, symbol) {
    xn = as.numeric(x)
    d = consistent_decimals(xn)
    has_decimal = any(xn[!is.na(xn)] %% 1 != 0)
    if (has_decimal) d = max(d, 2L)
    fmt = formatC(abs(xn), format = "f", digits = d, big.mark = ",")
    sign = ifelse(!is.na(xn) & xn < 0, "-", "")
    paste0(sign, symbol, fmt)
  }

  format_comma = function(x) {
    d = consistent_decimals(x)
    formatC(as.numeric(x), format = "f", digits = d, big.mark = ",")
  }

  format_dollar = function(x) format_currency(x, "$")

  format_euro = function(x) format_currency(x, "\u20ac")

  format_sterling = function(x) format_currency(x, "\u00a3")

  format_log = function(x) {
    x = as.numeric(x)
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
  numeric_fun = if (abs_) function(x) fun(abs(x)) else fun

  # The built-in formatters coerce their input via as.numeric(). Guard against
  # non-numeric input (e.g. categorical axis labels reaching a numeric labeller
  # via flip = TRUE) by leaving such values unchanged, which avoids "NAs
  # introduced by coercion" warnings (#622). The is.na(xn) & !is.na(x) test only
  # flags values that became NA through coercion, so genuine NA input still
  # reaches the formatter.
  function(x) {
    xn = suppressWarnings(as.numeric(x))
    if (any(is.na(xn) & !is.na(x))) {
      return(x)
    }
    numeric_fun(x)
  }
}
