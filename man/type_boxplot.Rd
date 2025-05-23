% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type_boxplot.R
\name{type_boxplot}
\alias{type_boxplot}
\title{Boxplot type}
\usage{
type_boxplot(
  range = 1.5,
  width = NULL,
  varwidth = FALSE,
  notch = FALSE,
  outline = TRUE,
  boxwex = 0.8,
  staplewex = 0.5,
  outwex = 0.5
)
}
\arguments{
\item{range}{this determines how far the plot whiskers extend out
    from the box.  If \code{range} is positive, the whiskers extend
    to the most extreme data point which is no more than
    \code{range} times the interquartile range from the box. A value
    of zero causes the whiskers to extend to the data extremes.}

\item{width}{a vector giving the relative widths of the boxes making
    up the plot.}

\item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are
    drawn with widths proportional to the square-roots of the number
    of observations in the groups.}

\item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in
    each side of the boxes.  If the notches of two plots do not
    overlap this is \sQuote{strong evidence} that the two medians differ
    (Chambers \abbr{et al.}, 1983, p.\ifelse{latex}{\out{~}}{ }{}62).  See \code{\link{boxplot.stats}}
    for the calculations used.}

\item{outline}{if \code{outline} is not true, the outliers are
    not drawn (as points whereas S+ uses lines).}

\item{boxwex}{a scale factor to be applied to all boxes.  When there
    are only a few groups, the appearance of the plot can be improved
    by making the boxes narrower.}

\item{staplewex}{staple line width expansion, proportional to box
    width.}

\item{outwex}{outlier line width expansion, proportional to box
    width.}
}
\description{
Type function for producing box-and-whisker plots.
Arguments are passed to \code{\link[graphics]{boxplot}}, although \code{tinyplot}
scaffolding allows added functionality such as grouping and faceting.
Box-and-whisker plots are the default plot type if \code{x} is a factor and \code{y} is
numeric.
}
\examples{
# "boxplot" type convenience string
tinyplot(count ~ spray, data = InsectSprays, type = "boxplot")

# Note: Specifying the type here is redundant. Like base plot, tinyplot
# automatically produces a boxplot if x is a factor and y is numeric
tinyplot(count ~ spray, data = InsectSprays)

# Grouped boxplot example
tinyplot(len ~ dose | supp, data = ToothGrowth, type = "boxplot")

# Use `type_boxplot()` to pass extra arguments for customization
tinyplot(
  len ~ dose | supp, data = ToothGrowth, lty = 1,
  type = type_boxplot(boxwex = 0.3, staplewex = 0, outline = FALSE)
)
}
