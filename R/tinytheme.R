#' Set or Reset Plot Themes for `tinyplot`
#'
#' @description
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, and axis labels. By default, a "bw" theme is available.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   - `"default"`
#'   - `"basic"`
#'   - `"clean"`
#'   - `"clean2"`
#'   - `"bw"`
#'   - `"classic"`
#'   - `"dark"`
#'   - `"ipsum"`
#'   - `"minimal"`
#'   - `"tufte"`
#'   - `"void"`
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
# tinytheme = function(theme = NULL, ...) {
  # in interactive sessions, we close the graphics device to reset parameters
  # if (interactive()) {
  #   # grDevices::graphics.off()
  # } else {
  #   par_first = get_saved_par("first")
  #   if (!is.null(par_first)) {
  #     do.call(tpar, par_first)
  #   } else {
  #     set_saved_par("first", par())
  #   }
  # }

  # in notebooks, we don't want to close the device because no image.
  # init_tpar() tries to be smart, but may fail.
  init_tpar(rm_hook = TRUE)

  assert_choice(
    theme,
    c(
      "default",
      sort(c("basic", "bw", "classic", "clean", "clean2", "dark", "ipsum", "minimal", "tufte", "void"))
    )
  )

  settings = switch(theme,
    "default" = theme_default,
    "basic" = theme_basic,
    "bw" = theme_bw,
    "classic" = theme_classic,
    "clean" = theme_clean,
    "clean2" = theme_clean2,
    "dark" = theme_dark,
    "ipsum" = theme_ipsum,
    "minimal" = theme_minimal,
    "tufte" = theme_tufte,
    "void" = theme_void,
  )

  dots = list(...)
  for (n in names(dots)) {
    settings[[n]] = dots[[n]]
  }

  if (length(settings) > 0) {
    if (theme == "default") {
      # for default theme, we want to revert the original pars and turn off the
      # before.new.plot hook (otherwise manual par(x = y) changes won't work) 
      tpar(settings, hook = FALSE)
      setHook("before.new.plot", NULL, "replace")
    } else {
      tpar(settings, hook = TRUE)
    }
  }

  return(invisible(NULL))
}



#
## Themes (these are read and set at initial load time)

# theme_default = list()

theme_default = list(
  tinytheme = "default",
  adj = par("adj"), # 0.5,
  adj.main = par("adj"), # 0.5,
  adj.sub = par("adj"), # 0.5,
  bg = par("bg"), # "white"
  bty = par("bty"), #"o",
  cex.axis = par("cex.axis"), #1,
  cex.main = par("cex.main"), #1.2,
  cex.xlab = par("cex.axis"), #1,
  cex.ylab = par("cex.axis"), #1,
  col.axis = par("col.axis"), #1,
  col.xaxs = par("col.axis"), #1,
  col.yaxs = par("col.axis"), #1,
  col.lab = par("col.lab"), #"black",
  col.main = par("col.main"), #"black",
  col.sub = par("col.sub"), #"black",
  facet.bg = NULL,
  facet.border = NA,
  fg = par("fg"),
  font = par("font"), # 1,
  font.axis = par("font.axis"), # 1,
  font.lab = par("font.lab"), # 1,
  font.main = par("font.main"), # 1,
  grid = FALSE,
  grid.col = "lightgray",
  grid.lty = "dotted",
  grid.lwd = 1,
  lab = par("lab"), # c(5, 5, 7),
  las = par("las"), # 0,
  lwd = par("lwd"), # 1,
  lwd.axis = par("lwd"), # 1,
  # palette.qualitative = "R4",
  # palette.sequential = "ag_Sunset",
  pch = par("pch"), # 1,
  side.sub = 1,
  tck = NA,
  xaxt = "standard",
  yaxt = "standard"
)

theme_basic = modifyList(theme_default, list(
  tinytheme = "basic",
  facet.bg = "gray90",
  facet.border = "black",
  grid = TRUE,
  pch = 16
))

theme_clean = modifyList(theme_basic, list(
  tinytheme = "clean",
  adj.main = 0,
  adj.sub = 0,
  # facet.bg = "gray90",
  # facet.border = "black",
  # grid = TRUE,
  las = 1,
  palette.qualitative = "Tableau 10",
  palette.sequential = "ag_Sunset",
  # pch = 16,
  side.sub = 3#,
  # tck = -.02
))

theme_clean2 = modifyList(theme_clean, list(
  tinytheme = "clean2",
  facet.border = "gray90",
  xaxt = "labels",
  yaxt = "labels"
))

# theme_bw = list(
#   tinytheme = "bw",
#   adj = 0.5,
#   adj.main = 0,
#   adj.sub = 0,
#   # bty = "o",
#   # cex.axis = 1,
#   # cex.main = 1.2,
#   # cex.xlab = 1,
#   # cex.ylab = 1,
#   facet.bg = "gray90",
#   facet.border = "black",
#   font = 1,
#   font.axis = 1,
#   font.lab = 1,
#   font.main = 1,
#   grid = TRUE,
#   grid.lty = 1,
#   grid.lwd = 0.5,
#   las = 1,
#   lwd = 0.5,
#   lwd.axis = 0.5,
#   palette.qualitative = "Okabe-Ito",
#   palette.sequential = "ag_Sunset",
#   pch = 16,
#   side.sub = 3,
#   tck = -.02
# )
theme_bw = modifyList(theme_clean, list(
  tinytheme = "bw",
  grid.lty = 1,
  grid.lwd = 0.5,
  lwd = 0.5,
  lwd.axis = 0.5,
  palette.qualitative = "Okabe-Ito",
  palette.sequential = "ag_Sunset",
  tck = -.02
))

theme_classic = modifyList(theme_clean, list(
  tinytheme = "classic",
  bty = "l",
  facet.bg = NULL,
  grid = FALSE,
  palette.qualitative = "Okabe-Ito",
  tck = -.02
))

theme_minimal = modifyList(theme_bw, list(
  tinytheme = "minimal",
  bty = "n",
  facet.bg = NULL,
  facet.border = NULL,
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
  # facet.bg = "gray20",
  grid.col = "#6D6D6D",
  palette.qualitative = "Set 2",
  palette.sequential = "Sunset"#,
  # new = TRUE
))

theme_tufte = modifyList(theme_default, list(
  tinytheme = "tufte",
  adj.main = 0,
  adj.sub = 0,
  bty = "n",
  lab = c(10, 10, 7),
  # palette.sequential = "Grays",
  pch = 16,
  side.sub = 3,
  tck = .02
))

theme_void = modifyList(theme_clean, list(
  tinytheme = "void",
  adj.main = 0,
  adj.sub = 0,
  facet.bg = NULL,
  facet.border = NULL,
  grid = FALSE,
  side.sub = 3,
  # tck = -.02,
  xaxt = "none",
  yaxt = "none"
))
