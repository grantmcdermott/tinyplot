#' tinyplot Method for Plotting ts Objects (Time Series)
#'
#' @description Convenience interface for visualizing ts objects
#'   (time sereis with tinyplot.
#'
#' @details Internally the time series object is converted to a long
#'   data frame with columns `Time` (time index), `Value` (observations),
#'   and `Series` (factor with column labels). Depending on the settings
#'  of `facet` this data frame is visualized either with the formula
#'  `Value ~ Time` or `Value ~ Time | Series`. See the `facet` argument
#'  description for more details and the examples for some illustrations.
#'
#'  An exception is made if the user explicitly supplies a distribution `type`
#'  argument (i.e, one of `"histogram"`, `"density"`, `"boxplot"`, `"violin"`,
#'  `"qq"`, `"ridge"`, or `"rug"`). These summarize the series values and
#'  ignore the time index, so they are visualized with the one-sided
#'  formula `~ Value` (or `~ Value | Series` for multivariate series). This
#'  preserves base-R-compatible behaviour such as
#'  `tinyplot(Nile, type = "histogram")`.
#'
#' @param x an object of class `"ts"`.
#' @param facet specification of `facet` for `tinyplot.formula`. The
#'   default in the `tinyplot` method is to use `facet = NULL` for univariate
#'   series and `facet = ~ Series` (equivalent to `facet = "by"`) for multivariate series.
#' @param type,facet.args,ylab,... further arguments passed to `tinyplot`.
#'
#' @examples
#' tinytheme("clean2")
#' 
#' ## univariate series
#' tinyplot(Nile)
#' 
#' # exception: expicitly passing a distribution type on a univariate series
#' # still triggers the corresponding transformation
#' tinyplot(Nile, type = "histogram")
#' 
#' ## multivariate 
#' tinyplot(EuStockMarkets)                    ## multiple, same color, free scales
#' tinyplot(EuStockMarkets, facet.args = NULL) ## multiple, same color, same scale
#' tinyplot(EuStockMarkets, facet = "by")      ## multiple, separate colors, free scales
#' tinyplot(EuStockMarkets, facet = NULL)      ## single, separate colors
#'
#' ## further variations
#' tinyplot(EuStockMarkets, facet = "by", facet.args = NULL)
#' tinyplot(EuStockMarkets, facet.args = list(free = TRUE, ncol = 1))
#' tinyplot(EuStockMarkets, facet = NULL, legend = list("direct", repel = TRUE))
#'
#' tinytheme() ## reset
#'
#' @importFrom stats time
#' @export
tinyplot.ts = function(x, facet, type = "l", facet.args = list(free = TRUE), ylab = "", ...) {
  ## basic object properties
  n = NROW(x)
  k = NCOL(x)
  lab = deparse(substitute(x))
  if (k > 1L) lab = paste(lab, 1L:k, sep = ".")
  if (!is.null(colnames(x))) lab = colnames(x)

  single = k == 1L

  ## convert to long data.frame
  df = data.frame(
    Time = rep.int(as.numeric(time(x)), k),
    Value = as.numeric(x),
    Series = factor(rep(1L:k, each = n), labels = lab)
  )

  ## Distribution types summarize the series *values* and ignore the time index,
  ## so they dispatch with a one-sided formula (`~ Value`, or `~ Value | Series`
  ## when multivariate) rather than `Value ~ Time`, and skip the time-series
  ## `ylab` default so tinyplot's own label (e.g. "Frequency") shows. This
  ## preserves base-R-compatible behaviour like `tinyplot(Nile, type = "histogram")`.
  dist_types = c("histogram", "hist", "density", "boxplot", "box",
                 "violin", "qq", "ridge", "rug")
  ## normalize to a type name to catch both the string ("histogram") and the
  ## type object (`type_histogram()`, whose `$name` is the canonical "histogram")
  type_name = if (inherits(type, "tinyplot_type")) {
    type[["name"]]
  } else if (is.character(type) && length(type) == 1L) {
    type
  } else {
    NA_character_
  }
  is_dist = !is.na(type_name) && type_name %in% dist_types

  ## default for facet
  if(missing(facet)) {
    auto = TRUE
    facet = if(single || is_dist) NULL else ~ Series
  } else {
    auto = FALSE
  }
  if (is.null(facet)) facet.args = NULL

  ## dispatch formula and axis labels
  xlab = NULL
  if (is_dist) {
    fml = if (single) ~ Value else ~ Value | Series
    if (single) xlab = lab
    ## use tinyplot's own ylab default (e.g. "Frequency") unless caller set one
    if (missing(ylab)) ylab = NULL
  } else if (single || (!is.null(facet) && auto)) {
    fml = Value ~ Time
  } else {
    fml = Value ~ Time | Series
  }

  ## call tinyplot
  tinyplot(fml, data = df, type = type, facet = facet, facet.args = facet.args,
           xlab = xlab, ylab = ylab, ...)
}
