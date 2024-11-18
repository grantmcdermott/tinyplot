#' Set or Reset Plot Themes for `tinyplot`
#'
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, and axis labels. By default, a "simple" theme is available.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   If `NULL`, the current theme is reset to default settings. Available themes include:
#'   - `"simple"`
#'   - `"ipsum"`
#' @param action "append", "prepend", or "replace" to the hook.
#' @param ... Named arguments to override specific theme settings. These arguments are
#'   passed to `tpar()` and take precedence over the predefined settings in the selected
#'   theme.
#' @inheritParams setHook
#'
#' @details
#' The function uses `setHook("before.plot.new", ...)` to apply the specified
#' theme settings just before a new plot is drawn. Themes are implemented
#' as a list of graphical parameters, which are passed to the `tpar` function.
#'
#' To reset the theme to default settings (no customization), call `tinytheme(NULL)`.
#'
#' @return The function returns nothing. It is called for its side effects.
#'
#' @examples
#' # Set a theme
#' tinytheme("simple")
#' tinyplot(mpg ~ hp | factor(am), data = mtcars)
#'
#' # Customize the theme by overriding default settings
#' tinytheme("simple", fg = "blue", font.main = 2)
#' tinyplot(mpg ~ hp | factor(am), data = mtcars, main = "Hello World!")
#'
#' # Reset the theme
#' tinytheme()
#' tinyplot(mpg ~ hp | factor(am), data = mtcars)
#'
#' @export
tinytheme = function(theme = NULL, ..., action = "replace") {
  assert_choice(action, c("append", "prepend", "replace"))

  if (is.null(theme)) {
    setHook("before.plot.new", NULL, action = action)
    return(invisible(NULL))
  }

  assert_choice(theme, c("simple", "ipsum", "grey"))
  settings = switch(theme,
    "simple" = theme_simple,
    "ipsum" = theme_ipsum,
    "grey" = theme_grey
  )

  dots = list(...)
  for (n in names(dots)) {
    settings[[n]] = dots[[n]]
  }

  theme_fun = function() {
    do.call(tpar, settings)
  }

  setHook("before.plot.new", theme_fun, action = "replace")
}

theme_simple = list(
  fg              = "black", # Foreground color
  adj             = 0.5, # Horizontal alignment of text
  bty             = "l", # Box type around the plot
  family          = "serif", # Font family
  font            = 1, # Font style (plain)
  font.axis       = 1, # Font style for axis labels
  font.lab        = 1, # Font style for axis titles
  font.main       = 1, # Font style for the main title (normal)
  font.sub        = 3, # Font style for the subtitle (italic)
  las             = 1, # Orientation of axis labels (1 = horizontal)
  tck             = 0 # Tick mark length (0 = none)
)


theme_ipsum = list(
  adj.main = 0,
  adj.sub = 0,
  adj.xlab = 1,
  adj.ylab = 1,
  bg = "white", # Background color
  bty = "n",
  cex = 1, # Base font size scaling
  cex.axis = .8,
  cex.lab = 1,
  cex.main = 1.5,
  cex.sub = 1,
  col.xaxs = NA,
  col.yaxs = NA,
  family = "Arial Narrow", # Base font family
  fg = "black",
  font = 1, # Font style (plain)
  font.axis = 1, # Font style for axis labels
  font.lab = 1, # Font style for axis titles
  font.main = 1, # Font style for the main title (normal)
  font.sub = 3, # Font style for the subtitle (italic)
  grid = TRUE,
  las = 1, # Orientation of axis labels (1 = horizontal)
  mgp = c(1.1, 0.1, 0),
  side.sub = 3,
  tck = 0 # Tick mark length (0 = none)
)


theme_grey = list(
  adj.main = 0.5, # Center main title
  adj.sub = 0.5, # Center subtitle
  adj.xlab = 0.5, # Center x-axis title
  adj.ylab = 0.5, # Center y-axis title
  bg = "white", # Background color
  bty = "n", # No box around the plot
  cex = 1, # Base font size scaling
  cex.axis = 0.8, # Axis text size
  cex.lab = 1, # Axis label size
  cex.main = 1.2, # Main title size
  cex.sub = 0.9, # Subtitle size
  col.axis = "#4D4D4D", # Axis text color
  col.lab = "black", # Axis label color
  col.main = "black", # Main title color
  col.sub = "black", # Subtitle color
  col.ticks = "#B3B3B3", # Tick color
  col.xaxs = NA, # No color for x-axis line
  col.yaxs = NA, # No color for y-axis line
  fg = "black", # Foreground color (text, lines)
  font = 1, # Font style (plain)
  font.axis = 1, # Font style for axis labels
  font.lab = 1, # Font style for axis titles
  font.main = 1, # Font style for the main title
  font.sub = 1, # Font style for the subtitle
  grid = TRUE, # Display grid
  grid.col = "white", # Grid line color
  grid.lwd = 1, # Grid line width
  grid.lty = "solid", # Grid line type
  grid.bg = "#EBEBEB",
  las = 1, # Horizontal axis labels
  family = "", # Base font family
  mgp = c(3, 1, 0), # Margin for axis titles (mgp[1]), axis labels (mgp[2]), axis lines (mgp[3])
  tck = -0.02 # Tick mark length (negative for inside ticks)
)
