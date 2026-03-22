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
#' ## multivariate 
#' tinyplot(EuStockMarkets)                    ## multiple, same color, free scales
#' tinyplot(EuStockMarkets, facet.args = NULL) ## multiple, same color, same scale
#' tinyplot(EuStockMarkets, facet = "by")      ## multiple, separate colors, free scales
#' tinyplot(EuStockMarkets, facet = NULL)      ## single, separate colors
#' 
#' ## further variations
#' tinyplot(EuStockMarkets, facet = "by", facet.args = NULL)
#' tinyplot(EuStockMarkets, facet.args = list(free = TRUE, ncol = 1))
#' 
#' tinytheme() ## reset
#' 
#' @export
tinyplot.ts = function(x, facet, type = "l", facet.args = list(free = TRUE), ylab = "", ...) {
  ## basic object properties
  n = NROW(x)
  k = NCOL(x)
  lab = deparse(substitute(x))
  if (k > 1L) lab = paste(lab, 1L:k, sep = ".")
  if (!is.null(colnames(x))) lab = colnames(x)

  ## convert to long data.frame
  df = data.frame(
    Time = rep.int(as.numeric(time(x)), k),
    Value = as.numeric(x),
    Series = factor(rep(1L:k, each = n), labels = lab)
  )

  ## default for facet
  single = k == 1L
  if(missing(facet)) {
    auto = TRUE
    facet = if(single) NULL else ~ Series
  } else {
    auto = FALSE
  }
  if (is.null(facet)) facet.args = NULL
  
  ## call tinyplot
  if(single | (!is.null(facet) & auto)) {
    tinyplot(Value ~ Time, data = df, facet = facet, facet.args = facet.args, type = type, ylab = ylab, ...)
  } else {
    tinyplot(Value ~ Time | Series, data = df, facet = facet, facet.args = facet.args, type = type, ylab = ylab, ...)
  }
}
