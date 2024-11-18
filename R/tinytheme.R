#' Set or Reset Plot Themes for `tinyplot`
#'
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, and axis labels. By default, a "bw" theme is available.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   If `NULL`, the current theme is reset to default settings. Available themes include:
#'   - `"bw"`
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

  # TODO: still does not reset everything
  if (is.null(theme)) {
    old = .tpar[["old_theme"]]
    if (!is.null(old)) {
      known_par = names(par(no.readonly = TRUE))
      tmp = old$par[names(old$par) %in% known_par]
      do.call(par, tmp)
      do.call(tpar, old[["tpar"]])
    }
    setHook("before.plot.new", NULL, action = action)
    return(invisible(NULL))
  }

  old_par = par()
  old_tpar = tpar()
  .tpar[["old_theme"]] = list(par = old_par, tpar = old_tpar)

  assert_choice(theme, c("bw", "minimal", "ipsum", "grey", "void"))
  settings = switch(theme,
    "bw" = theme_bw,
    "minimal" = theme_minimal,
    "ipsum" = theme_ipsum,
    "grey" = theme_grey,
    "void" = theme_void,
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


theme_bw = list(
  fg = "black",
  adj = 0.5,
  bty = "o",
  lwd = .5,
  font = 1,
  font.axis = 1,
  cex.main = 1.2,
  cex.xlab = 1,
  cex.ylab = 1,
  cex.axis = 1,
  side.sub = 3,
  adj.main = 0,
  adj.sub = 0,
  font.lab = 1,
  font.main = 1,
  font.sub = 1,
  font = 1,
  font.axis = 1,
  font.lab = 1,
  font.main = 1,
  font.sub = 3,
  grid = TRUE,
  grid.lty = 1,
  col.xaxs = NA,
  col.yaxs = NA,
  grid.lwd = 0.5,
  las = 1,
  tck = 0 # Tick mark length (0 = none)
)

theme_minimal = modifyList(theme_bw, list(
  bty = "n",
  lty = 0
))

theme_classic = modifyList(theme_bw, list(
  bty = "l"
))

theme_ipsum = modifyList(theme_bw, list(
  family = "Arial Narrow",
  font.sub = 3,
  bty = "n"
))

theme_grey = modifyList(theme_bw, list(
  bty = "n",
  lty = 0,
  grid = TRUE,
  grid.col = "white",
  grid.lwd = 1,
  grid.lty = "solid",
  grid.bg = "#EBEBEB"
))

theme_void = modifyList(theme_bw, list(
  xaxt = "n",
  yaxt = "n",
  ann = FALSE,
  bty = "n"
))
