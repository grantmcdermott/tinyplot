#' Set or Reset Plot Themes for `tinyplot`
#'
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, and axis labels. By default, a "bw" theme is available.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   If `NULL`, the tinyplot settings are re-initialized and the graphical device is closed. Available themes include:
#'   - `"bw"`
#'   - `"classic"`
#'   - `"dark"`
#'   - `"ipsum"`
#'   - `"minimal"`
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
#' @export
tinytheme = function(theme = NULL, ..., action = "replace") {
  assert_choice(action, c("append", "prepend", "replace"))

  off = tryCatch(dev.off(), error = function(e) NULL)
  setHook("before.plot.new", NULL, action = action)
  rm(list = names(.tpar), envir = .tpar)
  init_tpar()

  if (is.null(theme)) {
    return(invisible(NULL))
  }

  assert_choice(theme, sort(c("bw", "classic", "dark", "ipsum", "minimal")))
  settings = switch(theme,
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

  do.call(tpar, settings)

  ## Hook approach doesn't work because some of our tinyplot() code requires access to
  ## themeing options before drawing the plot window, and the hook on "before.plot.new"
  ## only sets tpar() at that time, which is too late.
  ##
  # theme_fun = function() {
  #   do.call(tpar, settings)
  # }
  # setHook("before.plot.new", theme_fun, action = "replace")
}


theme_bw = list(
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
  bty = "l",
  grid = FALSE,
  grid.lty = 0
))

theme_minimal = modifyList(theme_bw, list(
  bty = "n",
  xaxt = "labels",
  yaxt = "labels"
))

theme_ipsum = modifyList(theme_minimal, list(
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
  bg = "#1A1A1A",
  fg = "#BBBBBB",
  col = "#BBBBBB",
  col.axis = "#BBBBBB",
  col.lab = "#BBBBBB",
  col.main = "#BBBBBB",
  col.sub = "#BBBBBB",
  col = "#BBBBBB",
  grid.col = "#323232"
))
