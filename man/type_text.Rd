% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type_text.R
\name{type_text}
\alias{type_text}
\title{Text annotations plot type}
\usage{
type_text(
  labels,
  adj = NULL,
  pos = NULL,
  offset = 0.5,
  vfont = NULL,
  font = NULL,
  xpd = NULL,
  srt = 0
)
}
\arguments{
\item{labels}{Character vector of length 1 or of the same length as the
number of x,y coordinates.}

\item{adj}{one or two values in \eqn{[0, 1]} which specify the x
    (and optionally y) adjustment (\sQuote{justification}) of the
    labels, with 0 for left/bottom, 1 for right/top, and 0.5 for
    centered.  On most devices values outside \eqn{[0, 1]} will also
    work.  See below.}

\item{pos}{a position specifier for the text.  If specified this
    overrides any \code{adj} value given.  Values of \code{1},
    \code{2}, \code{3} and \code{4}, respectively indicate
    positions below, to the left of, above and to the right of
    the specified \code{(x,y)} coordinates.}

\item{offset}{when \code{pos} is specified, this value controls the
    distance (\sQuote{offset}) of the text label from the specified
    coordinate in fractions of a character width.}

\item{vfont}{\code{NULL} for the current font family, or a character
    vector of length 2 for \code{\link[grDevices]{Hershey}} vector
    fonts.  The first element of
    the vector selects a typeface and the second element selects a
    style.  Ignored if \code{labels} is an expression.}

\item{font}{Font to be used, following \code{\link[graphics:par]{graphics::par()}}.}

\item{xpd}{Logical value or \code{NA} denoting text clipping behaviour, following
\code{\link[graphics:par]{graphics::par()}}.}

\item{srt}{Numeric giving the desired string rotation in degrees.}
}
\description{
Type function for adding text annotations to a plot. This function allows
you to draw text at specified (x,y) coordinates.
}
\examples{
tinyplot(mpg ~ hp | factor(cyl),
  data = mtcars,
  type = type_text(
    labels = row.names(mtcars),
    font = 2,
    adj = 0
  )
)

# to avoid clipping text at the plot region, we can use xpd = NA
tinyplot(mpg ~ hp | factor(cyl),
  data = mtcars,
  type = type_text(
    labels = row.names(mtcars),
    font = 2,
    adj = 0,
    xpd = NA
  )
)

}
