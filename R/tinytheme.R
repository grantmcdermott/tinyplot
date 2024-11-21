#' Set or Reset Plot Themes for `tinyplot`
#'
#' @description
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, and axis labels. By default, a "bw" theme is available.
#'
#' Warning: `tinytheme()` shuts down all open graphical devices.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   - `"default"`
#'   - `"bw"`
#'   - `"classic"`
#'   - `"dark"`
#'   - `"ipsum"`
#'   - `"minimal"`
#' @param ... Named arguments to override specific theme settings. These arguments are
#'   passed to `tpar()` and take precedence over the predefined settings in the selected
#'   theme.
#'
#' @details
#' Sets a list of graphical parameters using `tpar()`
#'
#' To reset the theme to default settings (no customization), call `tinytheme()` without arguments.
#'
#' The `"custom"` theme sets parameters stored in a list in a global option. See examples below.
#'
#' @return The function returns nothing. It is called for its side effects.
#'
#' @examples
#' # Set a theme
#' tinytheme("bw")
#' tinyplot(mpg ~ hp | factor(am), data = mtcars)
#'
#' # Customize the theme by overriding default settings
#' tinytheme("bw", fg = "blue", font.main = 2)
#' tinyplot(mpg ~ hp | factor(am), data = mtcars, main = "Hello World!")
#'
#' # Reset the theme
#' tinytheme()
#' tinyplot(mpg ~ hp | factor(am), data = mtcars)
#'
#' # Custom theme
#' tinytheme("default", font.main = 3, col = "red")
#' tinyplot(mpg ~ hp | factor(am), data = mtcars)
#'
#' @export
tinytheme = function(theme = "default", ...) {
  # Always close device and re-initialize graphical parameters
  init_tpar(rm_hook = TRUE)

  assert_choice(theme, c("default", sort(c("bw", "classic", "dark", "ipsum", "minimal"))))

  settings = switch(theme,
    "default" = theme_default,
    "bw" = theme_bw,
    "classic" = theme_classic,
    "dark" = theme_dark,
    "ipsum" = theme_ipsum,
    "minimal" = theme_minimal
  )

  dots = list(...)
  for (n in names(dots)) {
    settings[[n]] = dots[[n]]
  }

  tpar(settings, hook = TRUE)
}


theme_default = list()

theme_bw = list(
  tinytheme = "bw",
  adj = 0.5,
  adj.main = 0,
  adj.sub = 0,
  bty = "o",
  cex.axis = 1,
  cex.main = 1.2,
  cex.xlab = 1,
  cex.ylab = 1,
  font = 1,
  font.axis = 1,
  font.lab = 1,
  font.main = 1,
  grid = TRUE,
  grid.lty = 1,
  grid.lwd = 0.5,
  las = 1,
  lwd = 0.5,
  lwd.axis = 0.5,
  side.sub = 3,
  tck = -.02
)

theme_classic = modifyList(theme_bw, list(
  tinytheme = "classic",
  bty = "l",
  grid = FALSE,
  grid.lty = 0
))

theme_minimal = modifyList(theme_bw, list(
  tinytheme = "minimal",
  bty = "n",
  xaxt = "labels",
  yaxt = "labels"
))

theme_ipsum = modifyList(theme_minimal, list(
  tinytheme = "ipsum",
  bty = "n",
  font.sub = 3,
  adj.ylab = 1,
  adj.xlab = 1
))

## TODO: Does not work well with facets: area between facets is grey as well
# theme_grey = modifyList(theme_minimal, list(
#   grid = TRUE,
#   grid.col = "white",
#   grid.lwd = 1,
#   grid.lty = "solid",
#   grid.bg = "#EBEBEB"
# ))

theme_dark = modifyList(theme_minimal, list(
  tinytheme = "dark",
  bg = "#1A1A1A",
  fg = "#BBBBBB",
  # col = "white",
  col.xaxs = "#BBBBBB",
  col.yaxs = "#BBBBBB",
  col.lab = "#BBBBBB",
  col.main = "#BBBBBB",
  col.sub = "#BBBBBB",
  col.axis = "#BBBBBB",
  grid.col = "#6D6D6D"
))
