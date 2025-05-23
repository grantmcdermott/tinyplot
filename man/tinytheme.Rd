% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tinytheme.R
\name{tinytheme}
\alias{tinytheme}
\title{Set or Reset Plot Themes for \code{tinyplot}}
\usage{
tinytheme(
  theme = c("default", "basic", "clean", "clean2", "bw", "classic", "minimal", "ipsum",
    "dark", "ridge", "ridge2", "tufte", "void"),
  ...
)
}
\arguments{
\item{theme}{A character string specifying the name of the theme to apply.
Themes are arranged in an approximate hierarchy, adding or subtracting
elements in the order presented below. Note that several themes are
\emph{dynamic}, in the sense that they attempt to reduce whitespace in a way
that is responsive to the length of axes labels, tick marks, etc. These
dynamic plots are marked with an asterisk (*) below.
\itemize{
\item \code{"default"}: inherits the user's default base graphics settings.
\item \code{"basic"}: light modification of \code{"default"}, only adding filled points, a panel background grid, and light gray background to facet titles.
\item \code{"clean"} (*): builds on \code{"basic"} by moving the subtitle above the plotting area, adding horizontal axis labels, employing tighter default plot margins and title gaps to reduce whitespace, and setting different default palettes ("Tableau 10" for discrete colors and "agSunset" for gradient colors). The first of our dynamic themes and the foundation for several derivative themes that follow below.
\item \code{"clean2"} (*): removes the plot frame (box) from \code{"clean"}.
\item \code{"classic"} (*): connects the axes in a L-shape, but removes the other top and right-hand edges of the plot frame (box). Also sets the "Okabe-Ito" palette as a default for discrete colors. Inspired by the \strong{ggplot2} theme of the same name.
\item \code{"bw"} (*): similar to \code{"clean"}, except uses thinner lines for the plot frame (box), solid grid lines, and sets the "Okabe-Ito" palette as a default for discrete colors. Inspired by the \strong{ggplot2} theme of the same name.
\item \code{"minimal"} (*): removes the plot frame (box) from \code{"bw"}, as well as the background for facet titles. Inspired by the \strong{ggplot2} theme of the same name.
\item \code{"ipsum"} (*): similar to \code{"minimal"}, except subtitle is italicised and axes titles are aligned to the far edges. Inspired by the \strong{hrbrthemes} theme of the same name for \strong{ggplot2}.
\item \code{"dark"} (*): similar to \code{"minimal"}, but set against a dark background with foreground and a palette colours lightened for appropriate contrast.
\item \code{"ridge"} (*): a specialized theme for ridge plots (see \code{\link[=type_ridge]{type_ridge()}}). Builds off of \code{"clean"}, but adds ridge-specific tweaks (e.g. default "Zissou 1" palette for discrete colors, solid horizontal grid lines, and minor adjustments to y-axis labels). Not recommended for non-ridge plots.
\item \code{"ridge2"} (*): removes the plot frame (box) from \code{"ridge"}, but retains the x-axis line. Again, not recommended for non-ridge plots.
\item \code{"tufte"}: floating axes and minimalist plot artifacts in the style of Edward Tufte.
\item \code{"void"}: switches off all axes, titles, legends, etc.
}}

\item{...}{Named arguments to override specific theme settings. These
arguments are passed to \code{tpar()} and take precedence over the predefined
settings in the selected theme.}
}
\value{
The function returns nothing. It is called for its side effects.
}
\description{
The \code{tinytheme} function sets or resets the theme for plots created with
\code{tinyplot}. Themes control the appearance of plots, such as text alignment,
font styles, axis labels, and even dynamic margin adjustment to reduce
whitespace.
}
\details{
Sets a list of graphical parameters using \code{tpar()}

To reset the theme to default settings (no customization), call \code{tinytheme()}
without arguments.

\strong{Caveat emptor:} Themes are a somewhat experimental feature of \code{tinyplot}.
While we feel confident that themes should work as expected for most
"standard" cases, there may be some sharp edges. Please report any unexpected
behaviour to our GitHub repo:
\url{https://github.com/grantmcdermott/tinyplot/issues}

Known current limitations include:
\itemize{
\item Themes do not work well when \code{legend = "top!"}.
\item Dynamic margin spacing does not account for multi-line strings (e.g., axes
or main titles that contain "\\n").
}
}
\examples{
# Reusable plot function
p = function() tinyplot(
  lat ~ long | depth, data = quakes,
  main = "Earthquakes off Fiji",
  sub = "Data courtesy of the Harvard PRIM-H project"
)
p()

# Set a theme
tinytheme("bw")
p()

# Try a different theme
tinytheme("dark")
p()
         
# Customize the theme by overriding default settings
tinytheme("bw", fg = "green", font.main = 2, font.sub = 3, family = "Palatino")
p()

# Another custom theme example
tinytheme("bw", font.main = 2, col.axis = "darkcyan", family = "HersheyScript")
p()

# Aside: One or two specialized themes are only meant for certain plot types
tinytheme("ridge2")
tinyplot(I(cut(lat, 10)) ~ depth, data = quakes, type = "ridge")

# Reset the theme
tinytheme()
p()

# Themes showcase
## We'll use a slightly more intricate plot (long y-axis labs and facets)
## to demonstrate dynamic margin adjustment etc.

thms = eval(formals(tinytheme)$theme)

for (thm in thms) {
  tinytheme(thm)
  tinyplot(
    I(Sepal.Length*1e4) ~ Petal.Length | Species, facet = "by", data = iris,
    main = "Demonstration of tinyplot themes",
    sub = paste0('tinytheme("', thm, '")')
  )
}

# Reset
tinytheme()

}
\seealso{
\code{\link{tpar}} which does the heavy lifting under the hood.
}
