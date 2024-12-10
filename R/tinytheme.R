#' Set or Reset Plot Themes for `tinyplot`
#'
#' @md
#' @description
#' The `tinytheme` function sets or resets the theme for plots created with
#' `tinyplot`. Themes control the appearance of plots, such as text alignment,
#' font styles, axis labels, and even dynamic margin adjustment to reduce
#' whitespace.
#'
#' @param theme A character string specifying the name of the theme to apply.
#'   Themes are arranged in an approximate hierarchy, adding or subtracting
#'   elements in the order presented below. Note that several themes are
#'   _dynamic_, in the sense that they attempt to reduce whitespace in a way
#'   that is responsive to the length of axes labels, tick marks, etc. These
#'   dynamic plots are marked with an asterisk (*) below.
#'   
#'   - `"default"`: inherits the user's default base graphics settings.
#'   - `"basic"`: light modification of `"default"`, only adding filled points, a panel background grid, and light gray background to facet titles.
#'   - `"clean"` (*): builds on `"basic"` by moving the subtitle above the plotting area, adding horizontal axis labels, employing tighter default plot margins and title gaps to reduce whitespace, and setting different default palettes ("Tableau 10" for discrete colors and "agSunset" for gradient colors). The first of our dynamic themes and the foundation for several derivative themes that follow below.
#'   - `"clean2"` (*): removes the plot frame (box) from `"clean"`,
#'   - `"classic"` (*): connects the axes in a L-shape, but removes the other top and right-hand edges of the plot frame (box). Also sets the "Okabe-Ito" palette as a default for discrete colors. Inspired by the **ggplot2** theme of the same name. 
#'   - `"bw"` (*): similar to `"clean"`, except uses thinner lines for the plot frame (box), solid grid lines, and sets the "Okabe-Ito" palette as a default for discrete colors. Inspired by the **ggplot2** theme of the same name. 
#'   - `"ipsum"` (*): similar to `"bw"`, except subtitle is italicised and axes titles are aligned to the far edges. Inspired by the **hrbrthemes** theme of the same name for **ggplot2**. 
#'   - `"minimal"` (*): removes the plot frame (box) from `"bw"`, as well as the background for facet titles. Inspired by the **ggplot2** theme of the same name. 
#'   - `"dark"` (*): similar to `"minimal"`, but set against a dark background with foreground and a palette colours lightened for appropriate contrast.
#'   - `"tufte"`: floating axes and minimalist plot artifacts in the style of Edward Tufte.
#'   - `"void"`: switches off all axes, titles, legends, etc.
#' @param ... Named arguments to override specific theme settings. These
#'   arguments are passed to `tpar()` and take precedence over the predefined
#'   settings in the selected theme.
#'
#' @details
#' Sets a list of graphical parameters using `tpar()`
#'
#' To reset the theme to default settings (no customization), call `tinytheme()`
#' without arguments.
#' 
#' **Cavear emptor:** Themes are a somewhat experimental feature of `tinyplot`.
#' We are reasonably confident that they should work as expected for most
#' "standard" cases. However, there may be some sharp edges. Please report any
#' unexpected behaviour to our GitHub repo:
#' https://github.com/grantmcdermott/tinyplot/issues
#' 
#' Known current limitations include:
#' 
#' - Themes do not work well when `legend = "top!"`.
#' - Themes do not play nicely with some complex plot types, particularly `"spineplot"` and `"ridge"`.
#' - Dynamic margin spacing does not account for multi-line strings (e.g., axes
#' or main titles that contain `"\n"`).
#'
#' @return The function returns nothing. It is called for its side effects.
#'
#' @examples
#' 
#' # Reusable plot function
#' p = function() tinyplot(
#'   lat ~ long | depth, data = quakes,
#'   main = "Earthquakes off Fiji",
#'   sub = "Data courtesy of the Harvard PRIM-H project"
#' )
#' p()
#' 
#' # Set a theme
#' tinytheme("bw")
#' p()
#'
#' # Try a different theme
#' tinytheme("dark")
#' p()
#'          
#' # Customize the theme by overriding default settings
#' tinytheme("bw", fg = "green", font.main = 2, font.sub = 3)
#' p()
#'
#' # Reset the theme
#' tinytheme()
#' p()
#' 
#' # Themes showcase
#' ## We'll use a slightly more intricate plot (long y-axis labs and facets)
#' ## to demonstrate dynamic margin adjustment etc.
#' 
#' thms = eval(formals(tinytheme)$theme)
#' 
#' for (thm in thms) {
#'   tinytheme(thm)
#'   tinyplot(
#'     I(Sepal.Length*1e4) ~ Petal.Length | Species, facet = "by", data = iris,
#'     main = "Demonstration of tinyplot themes",
#'     sub = paste0('tinytheme("', thm, '")')
#'   )
#' }
#' 
#' # Reset
#' tinytheme()
#'
#' @export
tinytheme = function(
    theme = c(
      "default", "basic",
      "clean", "clean2", "bw", "classic",
      "minimal", "ipsum", "dark",
      "tufte", "void"
    ),
    ...
    ) {
  
  theme = match.arg(theme)

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
  dynmar = FALSE,
  facet.bg = NULL,
  facet.border = NA,
  fg = par("fg"),
  font = par("font"), # 1,
  font.axis = par("font.axis"), # 1,
  font.lab = par("font.lab"), # 1,
  font.main = par("font.main"), # 2,
  font.sub = par("font.sub"), # 2,
  grid = FALSE,
  grid.col = "lightgray",
  grid.lty = "dotted",
  grid.lwd = 1,
  lab = par("lab"), # c(5, 5, 7),
  las = par("las"), # 0,
  lwd = par("lwd"), # 1,
  lwd.axis = par("lwd"), # 1,
  mar = c(5.1, 4.1, 4.1, 2.1), ## test
  mgp = par("mgp"),
  # palette.qualitative = "R4",
  # palette.sequential = "ag_Sunset",
  pch = par("pch"), # 1,
  side.sub = 1,
  tck = NA,
  xaxt = "standard",
  yaxt = "standard"
)

# derivatives of "default" 
# - basic
# - tufte
# - void

theme_basic = modifyList(theme_default, list(
  tinytheme = "basic",
  facet.bg = "gray90",
  facet.border = "black",
  grid = TRUE,
  pch = 16
))

theme_tufte = modifyList(theme_default, list(
  tinytheme = "tufte",
  adj.main = 0,
  adj.sub = 0,
  bty = "n",
  font.main = 1,
  lab = c(10, 10, 7),
  # palette.sequential = "Grays",
  pch = 16,
  side.sub = 3,
  tcl = 0.2
))

theme_void = modifyList(theme_default, list(
  tinytheme = "void",
  adj.main = 0,
  adj.sub = 0,
  font.main = 1,
  palette.qualitative = "Tableau 10",
  palette.sequential = "ag_Sunset",
  pch = 16,
  side.sub = 3,
  # tck = -.02,
  xaxt = "none",
  yaxt = "none"
))

# derivatives of "basic" 
# - clean

theme_clean = modifyList(theme_basic, list(
  ## Notes:
  ##  - 1. Reduce axis title gap by 0.5 lines and also reduce tcl to 0.3 lines.
  ##  - 2. Sub moves to top.
  ##  - 3. Also want to remove excess white on rhs of plot margin (when no legend).
  ##  - Together, 1, 2, and 3 imply that...
  ##    -- mgp[1] should be adjusted by 0.8 (= 0.5 + 0.3)
  ##    -- mgp[2] should be adjusted by 0.3
  ##    -- mar[1] should be adjusted by 1.8 (= 1 (no sub) + 0.5 + 0.3 (tighter axis labs))
  ##    -- mar[2] should be adjusted by 0.8 (= 0.5 + 0.3)
  ##    -- mar[3] should remain unchanged (main + sub will adjust automatically)
  ##    -- mar[4] should be adjusted by 1.5 (relative to 2.1)
  ##
  tinytheme = "clean",
  adj.main = 0,
  adj.sub = 0,
  dynmar = TRUE,
  las = 1,
  mar = c(5.1, 4.1, 4.1, 2.1) - c(1+0.5+0.3, 0.5+0.3, 0, 1.5), ## test
  mgp = c(3, 1, 0) - c(0.5+0.3, 0.3, 0), # i.e., subtract 0.5 lines + the (abs) value of the tcl adjustment
  palette.qualitative = "Tableau 10",
  palette.sequential = "ag_Sunset",
  side.sub = 3,
  tcl = -0.3
))

# derivatives of "clean" 
# - clean2
# - classic
# - bw

theme_clean2 = modifyList(theme_clean, list(
  tinytheme = "clean2",
  facet.border = "gray90",
  xaxt = "labels",
  yaxt = "labels"
))

theme_classic = modifyList(theme_clean, list(
  tinytheme = "classic",
  bty = "l",
  facet.bg = NULL,
  font.main = 1,
  grid = FALSE,
  palette.qualitative = "Okabe-Ito"
))

theme_bw = modifyList(theme_clean, list(
  tinytheme = "bw",
  font.main = 1,
  grid.lty = 1,
  grid.lwd = 0.5,
  lwd = 0.5,
  lwd.axis = 0.5,
  palette.qualitative = "Okabe-Ito"
))

# derivatives of "bw"
# - minimal
# - ipsum
# - dark
 
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
  palette.sequential = "Sunset"
))

