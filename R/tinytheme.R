#' Set or Reset Plot Themes for `tinyplot`
#'
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, and axis labels. By default, a "simple" theme is available.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   If `NULL`, the current theme is reset to default settings. Available themes include:
#'   - `"simple"`
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

  assert_choice(theme, c("simple", "ipsum"))
  settings = switch(theme,
    "simple" = theme_simple,
    "ipsum" = theme_ipsum
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
  col.xaxs = NA,
  col.yaxs = NA,
  cex.axis = .8,
  bg = "white", # Background color
  bty = "n",
  cex = 1, # Base font size scaling
  cex.lab = 1,
  cex.main = 1.5,
  cex.sub = 1.2,
  family = "Arial Narrow", # Base font family
  fg = "black",
  font = 1, # Font style (plain)
  mgp = c(1.1, 0.1, 0),
  font.axis = 1, # Font style for axis labels
  font.lab = 1, # Font style for axis titles
  font.main = 1, # Font style for the main title (normal)
  font.sub = 3, # Font style for the subtitle (italic)
  las = 1, # Orientation of axis labels (1 = horizontal)
  grid = TRUE,
  tck = 0 # Tick mark length (0 = none)
)
