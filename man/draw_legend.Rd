% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_legend.R
\name{draw_legend}
\alias{draw_legend}
\title{Calculate placement of legend and draw it}
\usage{
draw_legend(
  legend = NULL,
  legend_args = NULL,
  by_dep = NULL,
  lgnd_labs = NULL,
  type = NULL,
  pch = NULL,
  lty = NULL,
  lwd = NULL,
  col = NULL,
  bg = NULL,
  cex = NULL,
  gradient = FALSE,
  lmar = NULL,
  has_sub = FALSE,
  new_plot = TRUE,
  draw = TRUE
)
}
\arguments{
\item{legend}{Legend placement keyword or list, passed down from \link{tinyplot}.}

\item{legend_args}{Additional legend arguments to be passed to
\code{\link[graphics]{legend}}.}

\item{by_dep}{The (deparsed) "by" grouping variable name.}

\item{lgnd_labs}{The labels passed to \code{legend(legend = ...)}.}

\item{type}{Plotting type(s), passed down from \link{tinyplot}.}

\item{pch}{Plotting character(s), passed down from \link{tinyplot}.}

\item{lty}{Plotting linetype(s), passed down from \link{tinyplot}.}

\item{lwd}{Plotting line width(s), passed down from \link{tinyplot}.}

\item{col}{Plotting colour(s), passed down from \link{tinyplot}.}

\item{bg}{Plotting character background fill colour(s), passed down from \link{tinyplot}.}

\item{cex}{Plotting character expansion(s), passed down from \link{tinyplot}.}

\item{gradient}{Logical indicating whether a continuous gradient swatch
should be used to represent the colors.}

\item{lmar}{Legend margins (in lines). Should be a numeric vector of the form
\code{c(inner, outer)}, where the first number represents the "inner" margin
between the legend and the plot, and the second number represents the
"outer" margin between the legend and edge of the graphics device. If no
explicit value is provided by the user, then reverts back to \code{tpar("lmar")}
for which the default values are \code{c(1.0, 0.1)}.}

\item{has_sub}{Logical. Does the plot have a sub-caption. Only used if
keyword position is "bottom!", in which case we need to bump the legend
margin a bit further.}

\item{new_plot}{Logical. Should we be calling plot.new internally?}

\item{draw}{Logical. If \code{FALSE}, no legend is drawn but the sizes are
returned. Note that a new (blank) plot frame will still need to be started
in order to perform the calculations.}
}
\value{
No return value, called for side effect of producing a(n empty) plot
with a legend in the margin.
}
\description{
Function used to calculate the placement of (including
outside the plotting area) and drawing of legend.
}
\examples{
oldmar = par("mar")

draw_legend(
  legend = "right!", ## default (other options incl, "left(!)", ""bottom(!)", etc.)
  legend_args = list(title = "Key", bty = "o"),
  lgnd_labs = c("foo", "bar"),
  type = "p",
  pch = 21:22,
  col = 1:2
)

# The legend is placed in the outer margin...
box("figure", col = "cyan", lty = 4)
# ... and the plot is proportionally adjusted against the edge of this
# margin.
box("plot")
# You can add regular plot objects per normal now
plot.window(xlim = c(1,10), ylim = c(1,10))
points(1:10)
points(10:1, pch = 22, col = "red")
axis(1); axis(2)
# etc.

# Important: A side effect of draw_legend is that the inner margins have been
# adjusted. (Here: The right margin, since we called "right!" above.)
par("mar")

# To reset you should call `dev.off()` or just reset manually.
par(mar = oldmar)

# Note that the inner and outer margin of the legend itself can be set via
# the `lmar` argument. (This can also be set globally via
# `tpar(lmar = c(inner, outer))`.)
draw_legend(
  legend_args = list(title = "Key", bty = "o"),
  lgnd_labs = c("foo", "bar"),
  type = "p",
  pch = 21:22,
  col = 1:2,
  lmar = c(0, 0.1) ## set inner margin to zero
)
box("figure", col = "cyan", lty = 4)

par(mar = oldmar)

# Continuous (gradient) legends are also supported
draw_legend(
  legend = "right!",
  legend_args = list(title = "Key"),
  lgnd_labs = LETTERS[1:5],
  col = hcl.colors(5),
  gradient = TRUE ## enable gradient legend
)

par(mar = oldmar)

}
