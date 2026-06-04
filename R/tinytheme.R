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
#'   - `"dynamic"` (*): builds on `"basic"` by enabling dynamic margin adjustment with tighter default margins, horizontal axis labels, and the subtitle moved above the plotting area. Turns off the panel grid. Provides the foundation for all other dynamic themes and is a good starting point for users who wish to build custom dynamic themes.
#'     - `"clean"` (*): builds on `"dynamic"` by re-enabling the panel background grid and setting different default palettes ("Tableau 10" for discrete colors and "agSunset" for gradient colors).
#'       - `"clean2"` (*): removes the plot frame (box) from `"clean"`.
#'     - `"classic"` (*): builds on `"dynamic"` with L-shaped axes (removing the top and right-hand edges of the plot frame), smaller axis text, tighter axis spacing, and the "Okabe-Ito" palette as a default for discrete colors. Inspired by the **ggplot2** theme of the same name.
#'     - `"bw"` (*): similar to `"clean"`, except uses thinner lines for the plot frame (box), solid fine grid lines, smaller axis text, tighter axis spacing, and sets the "Okabe-Ito" palette as a default for discrete colors. Inspired by the **ggplot2** theme of the same name.
#'       - `"linedraw"` (*): builds on `"bw"` with black facet title strips and white strip text. Inspired by the **ggplot2** theme of the same name.
#'       - `"minimal"` (*): removes the plot frame (box) from `"bw"`, as well as the background for facet titles. Inspired by the **ggplot2** theme of the same name.
#'         - `"ipsum"` (*): builds on `"minimal"` with bold titles, no ticks, fine grid, edge-aligned axis titles, and a custom muted palette. Inspired by the **hrbrthemes** theme of the same name for **ggplot2**.
#'         - `"ipsum2"` (*): a lighter variant of `"ipsum"` that retains the original italic subtitle and edge-aligned axis titles, but without the additional spacing and palette changes.
#'         - `"dark"` (*): similar to `"minimal"`, but set against a dark background with foreground and a palette colours lightened for appropriate contrast.
#'         - `"socviz"` (*): builds on `"minimal"` with L-shaped axes, very light grid lines, and larger axis text. Inspired by Kieran Healy's **socviz** package theme for **ggplot2**.
#'     - `"broadsheet"` (*): a publication/newspaper style with only horizontal grid lines, no frame, short x-axis ticks, and muted secondary text (subtitle, caption). Compact axis spacing.
#'       - `"nber"` (*): builds on `"broadsheet"` for an NBER working paper style with a light blue-grey background, grey text, italic axis titles and captions, and a blue-grey discrete palette.
#'     - `"web"` (*): a FiveThirtyEight-inspired style with a light grey device background, no frame or axis lines, and bold grid lines. Suited to web/online publication.
#'     - `"tufte"` (*): floating axes and minimalist plot artifacts in the style of Edward Tufte.
#'       - `"float"` (*): builds on `"tufte"` with outward ticks, fewer tick marks, and a "dark" qualitative palette.
#'     - `"void"` (*): switches off all axes, titles, legends, etc.
#'     - `"ridge"` (*): a specialized theme for ridge plots (see [`type_ridge()`]). Builds off of `"clean"`, but adds ridge-specific tweaks (e.g. default "Zissou 1" palette for discrete colors, solid horizontal grid lines, and minor adjustments to y-axis labels). Not recommended for non-ridge plots.
#'       - `"ridge2"` (*): removes the plot frame (box) from `"ridge"`, but retains the x-axis line. Again, not recommended for non-ridge plots.
#' @param ... Named arguments to override specific theme settings. These
#'   arguments are passed to `tpar()` and take precedence over the predefined
#'   settings in the selected theme.
#'
#' @details
#' Sets a list of graphical parameters using `tpar()`
#'
#' **Persistent vs. ephemeral themes.** Calling `tinytheme("<theme>")` triggers
#' a persistent theme, which will be applied to all subsequent graphics (incl.
#' base `plot`). To reset the theme to your default settings, call `tinytheme()`
#' without arguments. Alternatively, invoke the `tinyplot(..., theme = <theme>)`
#' argument for an ephemeral theme that is automatically reset at the end of the
#' plot call.
#' 
#' ```
#' # Set a persistent theme
#' tinytheme("clean")
#' tinyplot(1:10)  # uses "clean"
#' tinyplot(10:1)  # still uses "clean"
#' tinytheme()     # reset to default
#' 
#' # Use an ephemeral theme for a single plot (incl. added components)
#' tinyplot(1:10, theme = "dark")  # uses "dark" ephemerally
#' tinyplot_add(type = "S")        # same plot, so retains theme
#' tinyplot(10:1)                  # new plot, so back to default
#' ```
#' 
#' **Custom overrides.** To customize a theme's parameters (e.g., `mar`, `las`,
#' etc.), either pass them directly as additional args to
#' `tinytheme(<theme>, ...)` or as a list arguments to
#' `tinyplot(..., theme = list(<theme>, ...))`.
#' Please note that passing overrides through `tpar()` or `par()` _won't_ work,
#' because themes work by installing a persistent hook, which means that an
#' active theme will override any subsequent calls for parameters that the theme
#' controls.
#' 
#' ```
#' # Do this
#' tinytheme("clean", mar = c(5, 5, 2, 2))
#' <some plot>
#' 
#' # Or this (ephemeral version)
#' tinyplot(..., theme = list("clean", mar = c(5, 5, 2, 2)))
#' 
#' # Don't do this (the theme hook will overwrite your changes)
#' tinytheme("clean")
#' tpar(mar = c(5, 5, 2, 2))
#' <some plot>
#' ```
#' 
#' **Spacing primitives.** Dynamic themes compute `mgp` (margin line positions)
#' automatically from two spacing primitives, rather than requiring users to
#' reason about how `mar`, `mgp`, and `tcl` combine:
#'
#' - `gap.axis`: the gap in margin lines between the tick tip and the near edge
#'   of the tick label. Default `0.2`.
#' - `gap.lab`: the gap in margin lines between the far edge of the tick label
#'   and the near edge of the axis title. Default `1.0`.
#'
#' These primitives scale automatically with `cex.axis` and `cex.lab`, so the
#' visible spacing between elements remains constant regardless of text size.
#' To adjust spacing, pass them as overrides:
#'
#' ```
#' # Tighter spacing between tick labels and axis titles
#' tinytheme("clean", gap.lab = 0.5)
#'
#' # More room between ticks and tick labels
#' tinytheme("clean", gap.axis = 0.5)
#' ```
#'
#' If you supply an explicit `mgp` value, it is used as-is and the primitives
#' are ignored.
#'
#' @return The function returns nothing. It is called for its side effects.
#' 
#' @seealso [`tpar`] which does the heavy lifting under the hood;
#'   [tinytheme_register()] for adding custom named themes.
#'
#' @examples
#' # Reusable plot function
#' p = function() tinyplot(
#'   lat ~ long | depth, data = quakes,
#'   main = "Earthquakes off Fiji",
#'   sub = "Data courtesy of the Harvard PRIM-H project"
#' )
#' p()
#' 
#' # Set a theme
#' tinytheme("dark")
#' p()
#' 
#' #  A set theme is persistent and will apply to subsequent plots
#' tinyplot(0:10)
#'
#' # Try a different theme
#' tinytheme("clean")
#' p()
#'          
#' # Customize the theme by overriding default settings
#' tinytheme("clean",
#'           adj.xlab = 1, adj.ylab = 1,
#'           cex.lab = 0.75, cex.axis = 0.9,
#'           font.sub = 3,
#'           gap.axis = 0, gap.lab = 0.5,
#'           tcl = -0.1)
#' p()
#' 
#' # Another custom theme example, including a different font
#' tinytheme("bw", font.main = 2, col.axis = "darkcyan", family = "HersheyScript")
#' p()
#' 
#' # Aside: One or two specialized themes are only meant for certain plot types
#' tinytheme("ridge2")
#' tinyplot(I(cut(lat, 10)) ~ depth, data = quakes, type = "ridge")
#'
#' # Reset the theme
#' tinytheme()
#' p()
#' 
#' # For an ephemeral theme, use `tinyplot(..., theme = <theme>)` directly
#' tinyplot(0:10, theme = "clean", main = "This theme is ephemeral")
#' tinyplot(10:0, main = "See, no more theme")
#' 
#' # Customize an ephemeral theme by passing arguments as a list
#' tinyplot(0:10, main = "Custom", theme = list("clean", grid.col = "hotpink"))
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
#'     yaxl = ",", 
#'     main = paste0('tinytheme("', thm, '")'),
#'     sub = "A subtitle",
#'     cap = "A caption"
#'   )
#'   box("outer", lty = 2)
#' }
#' 
#' # Reset
#' tinytheme()
#'
#' @export
tinytheme = function(
    theme = c(
      "default", "basic", "dynamic",
      "clean", "clean2", "bw", "linedraw", "classic",
      "minimal", "ipsum", "ipsum2", "dark",
      "socviz", "broadsheet", "nber", "web",
      "ridge", "ridge2",
      "tufte", "float", "void"
    ),
    ...
    ) {
  
  if (length(theme) > 1) theme = theme[1]

  registered = names(get_environment_variable(".registered_themes"))
  assert_choice(theme, c(builtin_themes, registered))

  # in notebooks, we don't want to close the device because no image.
  # init_tpar() tries to be smart, but may fail.
  init_tpar(rm_hook = TRUE)

  settings = switch(theme,
    "default" = theme_default,
    "basic" = theme_basic,
    "broadsheet" = theme_broadsheet,
    "bw" = theme_bw,
    "linedraw" = theme_linedraw,
    "classic" = theme_classic,
    "clean" = theme_clean,
    "clean2" = theme_clean2,
    "dark" = theme_dark,
    "dynamic" = theme_dynamic,
    "ipsum" = theme_ipsum,
    "ipsum2" = theme_ipsum2,
    "minimal" = theme_minimal,
    "nber" = theme_nber,
    "ridge" = theme_ridge,
    "ridge2" = theme_ridge2,
    "socviz" = theme_socviz,
    "tufte" = theme_tufte,
    "float" = theme_float,
    "void" = theme_void,
    "web" = theme_web,
    get_environment_variable(".registered_themes")[[theme]]
  )

  dots = list(...)
  for (n in names(dots)) {
    settings[[n]] = dots[[n]]
  }

  # Compute mgp from spacing primitives when dynmar is active and the user
  # didn't provide an explicit mgp override. The near edge of margin text
  # (facing the plot region) aligns with the half-cell boundary (0.5*cex from
  # center). Using 0.5*cex in mgp keeps the visible gap between adjacent text
  # elements constant regardless of cex scaling.
  if (isTRUE(settings[["dynmar"]]) && !("mgp" %in% names(dots))) {
    .ga = settings[["gap.axis"]] %||% 0.2
    .gl = settings[["gap.lab"]] %||% 1.0
    .ca = settings[["cex.axis"]] %||% 1
    # FIXME: mgp is shared across sides, so we use the larger label cex to
    # avoid clipping on either axis. Ideally we'd set side-specific mgp when
    # cex.xlab and cex.ylab differ.
    .cl = max(
      settings[["cex.lab"]] %||% 1,
      settings[["cex.xlab"]] %||% 0,
      settings[["cex.ylab"]] %||% 0
    )
    .mgp2 = .ga + 0.5 * .ca
    .mgp1 = .mgp2 + .gl + 0.5 * .cl
    settings[["mgp"]] = c(.mgp1, .mgp2, 0)
  }

  if (length(settings) > 0) {
    if (theme == "default") {
      # for default theme, we want to revert the original pars and turn off the
      # before.new.plot hook (otherwise manual par(x = y) changes won't work) 
      tpar(settings, hook = FALSE)
      old_hooks = get_environment_variable(".tpar_hooks")
      remove_hooks(old_hooks)
    } else {
      tpar(settings, hook = TRUE)
    }
  }

  return(invisible(NULL))
}



#
## Themes (these are read and set at initial load time)

builtin_themes = c(
  "default", "basic", "dynamic",
  "clean", "clean2", "bw", "linedraw", "classic",
  "minimal", "ipsum", "ipsum2", "dark",
  "socviz", "broadsheet", "nber", "web",
  "ridge", "ridge2",
  "tufte", "float", "void"
)

# theme_default = list()

theme_default = list(
  tinytheme = "default",
  adj = par("adj"), # 0.5,
  adj.main = par("adj"), # 0.5,
  adj.cap = par("adj"), # 0.5,
  adj.sub = par("adj"), # 0.5,
  bg = "white", # par("bg") # "white"
  bty = par("bty"), #"o",
  cex = par("cex"), #1,
  cex.axis = par("cex.axis"), #1,
  cex.lab = par("cex.lab"), #1,
  cex.main = par("cex.main"), #1.2,
  cex.cap = 1,
  cex.sub = par("cex.sub"), #1,
  cex.xlab = NULL, # defer to par("cex.lab") unless set explicitly
  cex.ylab = NULL, # defer to par("cex.lab") unless set explicitly
  col = par("col"), #"black",
  col.axis = par("col.axis"), #1,
  col.xaxs = par("col.axis"), #1,
  col.yaxs = par("col.axis"), #1,
  col.lab = par("col.lab"), #"black",
  col.main = par("col.main"), #"black",
  col.cap = "black",
  col.sub = par("col.sub"), #"black",
  dynmar = FALSE,
  facet.bg = NULL,
  facet.border = NA,
  family = par("family"), # ""
  fg = par("fg"),
  font = par("font"), # 1,
  font.axis = par("font.axis"), # 1,
  font.lab = par("font.lab"), # 1,
  font.main = par("font.main"), # 2,
  font.cap = 1,
  font.sub = par("font.sub"), # 2,
  grid = FALSE,
  grid.col = "lightgray",
  grid.lty = "dotted",
  grid.lwd = 1,
  ljust = "left",
  lab = par("lab"), # c(5, 5, 7),
  las = par("las"), # 0,
  lwd = par("lwd"), # 1,
  lwd.axis = par("lwd"), # 1,
  mar = c(5.1, 4.1, 4.1, 2.1), ## test
  mgp = par("mgp"),
  # palette.qualitative = "R4",
  # palette.sequential = "Viridis",
  pch = par("pch"), # 1,
  ribbon.alpha = 0.2,
  side.sub = 1,
  tck = NA,
  tcl = par("tcl"), # -0.5
  xaxt = "standard",
  yaxt = "standard"
)

# derivatives of "default" 
# - basic
# - tufte
# - void

theme_basic = modifyList(theme_default, list(
  tinytheme = "basic",
  adj.cap = 1,
  facet.bg = "gray90",
  facet.border = "black",
  grid = TRUE,
  pch = 16
))

# derivatives of "basic"
# - dynamic

theme_dynamic = modifyList(theme_basic, list(
  ## Notes:
  ##  - Dynamic themes start from an (almost) zero `mar` baseline. The
  ##    `draw_facet_window()` helper builds each side's margin up from this
  ##    pad, adding only what the plot actually needs (tick row, axis label,
  ##    main, sub). See `dynmar_side()` in utils.R.
  ##  - `mgp` is computed in `tinytheme()` from the spacing primitives
  ##    (gap.axis, gap.lab) and the active cex.axis/cex.lab values. If the
  ##    user provides an explicit `mgp`, it is used as-is and the primitives
  ##    are ignored.
  ##  - `side.sub = 3` moves the sub-caption above the plot (below main).
  ##  - `tcl = -0.3` tightens axis tick marks relative to the base default.
  ##
  tinytheme = "dynamic",
  adj.main = 0,
  adj.sub = 0,
  dynmar = TRUE,
  gap.axis = 0.2,  # gap (lines) between tick tip and tick label cell edge
  gap.lab = 1.0,   # gap (lines) from tick label cell edge to title cell edge
  gap.main = 0.7,  # gap (lines) from main baseline to element below (plot or sub top)
  gap.sub = 0.7,   # gap (lines) from sub baseline to plot box
  grid = FALSE,
  las = 1,
  mar = c(0.1, 0.1, 0.6, 0.6),
  mgp = NULL,      # computed from gap.axis/gap.lab in tinytheme()
  side.sub = 3,
  tcl = -0.3
))

# derivatives of "dynamic"
# - clean
# - classic

theme_clean = modifyList(theme_dynamic, list(
  tinytheme = "clean",
  cex.cap = 0.8,
  grid = TRUE,
  palette.qualitative = "Tableau 10",
  palette.sequential = "ag_Sunset"
))

theme_classic = modifyList(theme_dynamic, list(
  tinytheme = "classic",
  bty = "l",
  cex.axis = 0.8,
  cex.cap = 0.8,
  facet.bg = NULL,
  font.main = 1,
  gap.axis = 0.1,
  gap.lab = 0.4,
  palette.qualitative = "Okabe-Ito"
))

# derivatives of "clean"
# - clean2
# - bw

theme_clean2 = modifyList(theme_clean, list(
  tinytheme = "clean2",
  facet.border = "gray90",
  xaxt = "labels",
  yaxt = "labels"
))

theme_bw = modifyList(theme_clean, list(
  tinytheme = "bw",
  cex.axis = 0.8,
  font.main = 1,
  gap.axis = 0.1,
  gap.lab = 0.4,
  grid = "xy",
  grid.lty = 1,
  grid.lwd = 0.5,
  lwd = 0.5,
  lwd.axis = 0.5,
  palette.qualitative = "Okabe-Ito"
))

theme_linedraw = modifyList(theme_bw, list(
  tinytheme = "linedraw",
  facet.bg = "black",
  facet.col = "white",
  grid.col = "black",
  grid.lwd = 0.1
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
  adj.xlab = 1,
  adj.ylab = 1,
  cex.lab = 0.8,
  font.main = 2,
  font.sub = 1,
  font.cap = 3,
  gap.axis = 0,
  gap.lab = 0.7,
  grid = "xy",
  palette.qualitative = c("#D18975", "#8FD175", "#3F2D54", "#75B8D1",
                          "#2D543D", "#C9D175", "#D1AB75", "#D175B8", "#758BD1"),
  tcl = 0
))

theme_ipsum2 = modifyList(theme_minimal, list(
  tinytheme = "ipsum2",
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
  col.cap = "#BBBBBB",
  col.sub = "#BBBBBB",
  col.axis = "#BBBBBB",
  # facet.bg = "gray20",
  grid.col = "#6D6D6D",
  palette.qualitative = "Set 2",
  palette.sequential = "Sunset",
  ribbon.alpha = 0.5
))

# derivatives of clean/clean2

theme_ridge = modifyList(theme_clean, list(
  tinytheme = "ridge",
  palette.qualitative = "Zissou 1",
  grid = FALSE
))
theme_ridge2 = modifyList(theme_clean2, list(
  tinytheme = "ridge2",
  palette.qualitative = "Zissou 1",
  grid = FALSE
))

# derivatives of "minimal"
# - socviz

theme_socviz = modifyList(theme_minimal, list(
  tinytheme = "socviz",
  bty = "l",
  cex.axis = 1.1,
  cex.cap = 0.75,
  cex.lab = 1,
  cex.main = 1.4,
  cex.sub = 1.05,
  col.xaxs = "gray10",
  col.yaxs = "gray10",
  facet.bg = NULL,
  facet.col = "grey10",
  font.main = 2,
  grid.col = "gray90",
  grid.lty = 1,
  grid.lwd = 0.3,
  lwd = 1,
  lwd.axis = 1,
  palette.qualitative = c("#E69F00", "#56B4E9", "#009E73", "#D55E00",
                          "#CC79A7", "#0072B2", "#F0E442", "#000000"),
  tcl = -0.25,
  xaxt = "standard",
  yaxt = "standard"
))

# derivatives of "dynamic"
# - broadsheet
# - web

theme_broadsheet = modifyList(theme_dynamic, list(
  tinytheme = "broadsheet",
  bty = "n",
  cex.cap = 0.8,
  col.cap = "gray40",
  col.sub = "gray40",
  font.main = 2,
  gap.axis = 0.1,
  gap.lab = 0.5,
  grid = "Y",
  grid.col = "gray85",
  grid.lty = 1,
  grid.lwd = 0.5,
  palette.qualitative = "Okabe-Ito",
  tcl = -0.2,
  yaxt = "labels"
))

# derivatives of "broadsheet"
# - nber

theme_nber = modifyList(theme_broadsheet, list(
  tinytheme = "nber",
  bg = "#F2F7FB",
  cex.cap = 1,
  cex.main = 1.4,
  cex.sub = 1,
  col.axis = "#4C4D4F",
  col.lab = "#6D6E72",
  col.main = "#4C4D4F",
  col.sub = "#6D6E72",
  col.cap = "#6D6E72",
  col.xaxs = "#6D6E72",
  facet.bg = "white",
  facet.col = "#6D6E72",
  font.cap = 3,
  font.lab = 3,
  font.sub = 3,
  grid.col = "#BCBFC3",
  palette.qualitative = c(
    "#0063A7", "#6D6E70", "#941A22", 
    "#EDAF48", "#2E8B57", "#7B5EA7")
))

theme_web = modifyList(theme_dynamic, list(
  tinytheme = "web",
  bg = "#F0F0F0",
  bty = "n",
  cex.cap = 0.8,
  col.cap = "gray40",
  col.sub = "gray40",
  font.main = 2,
  grid = TRUE,
  grid.col = "#D2D2D2",
  grid.lty = 1,
  grid.lwd = 0.5,
  palette.qualitative = c("#008FD5", "#FF2700", "#77AB43"),
  tcl = 0,
  xaxt = "labels",
  yaxt = "labels"
))

# standalone dynamic derivatives
# - tufte
# - void

theme_tufte = modifyList(theme_dynamic, list(
  tinytheme = "tufte",
  bty = "n",
  facet.bg = NULL,
  facet.border = NA,
  font.main = 1,
  gap.axis = -0.2,
  gap.lab = 0.5,
  grid = FALSE,
  lab = c(10, 10, 7),
  tcl = 0.2
))

theme_float = modifyList(theme_tufte, list(
  tinytheme = "float",
  gap.axis = 0,
  gap.lab = 0.7,
  lab = c(5, 5, 7),
  palette.qualitative = "dark",
  tcl = -0.2
))

theme_void = modifyList(theme_dynamic, list(
  tinytheme = "void",
  facet.bg = NULL,
  facet.border = NA,
  font.main = 1,
  grid = FALSE,
  palette.qualitative = "Tableau 10",
  palette.sequential = "ag_Sunset",
  xaxt = "none",
  yaxt = "none"
))


#
## Theme registry helpers
#

# Internal: unified theme lookup (registered first, then built-in)
get_theme_def = function(name) {
  if (is.null(name) || name == "default") return(theme_default)
  registry = get_environment_variable(".registered_themes")
  if (!is.null(registry[[name]])) return(registry[[name]])
  obj_name = paste0("theme_", name)
  if (exists(obj_name, envir = asNamespace("tinyplot"), inherits = FALSE)) {
    return(get(obj_name, envir = asNamespace("tinyplot")))
  }
  NULL
}


#' Register a Custom Named Theme
#'
#' @md
#' @description
#' Register a custom theme so it can be used by name with `tinytheme(<theme>)`
#' or `tinyplot(..., theme = <theme>)`. Custom themes inherit from a base theme
#' and apply user-specified overrides.
#'
#' Registered themes are session-scoped: they persist across plots but not
#' across R sessions. To make a custom theme available automatically, register
#' it in your `.Rprofile`.
#'
#' @param name Character string. The name for your custom theme. Cannot clash
#'   with or overwrite a built-in theme name (`"default"`, `"clean"`, etc.)
#' @param theme Character string or list. The base theme to inherit from.
#'   If a string, it must reference a built-in or previously-registered theme.
#'   If a list, it is used directly as the base definition. Default is
#'   `"default"`.
#' @param ... Named arguments to override specific theme settings. These are
#'   the same parameters accepted by `tpar()`.
#'
#' @return Returns the theme definition list (invisibly).
#'
#' @seealso [tinytheme()], [tinytheme_list()], [tinytheme_unregister()]
#'
#' @examples
#' # Register a simple custom theme, based on "float" but now with a grid
#' tinytheme_register("float2", theme = "float", grid = TRUE)
#'
#' # Use it persistently
#' tinytheme("float2")
#' tinyplot(1:5)
#' tinytheme()
#'
#' # Or ephemerally
#' tinyplot(1:5, theme = "float2")
#'
#' # Optional: unregister the theme
#' tinytheme_unregister("float2")
#'
#' # A more elaborate, pirate-themed example
#' tinytheme_register(
#'   "pirate",
#'   theme = "clean",
#'   family = "HersheyScript",
#'   bg = "#f5e6c8", fg = "#3b2209",
#'   cex.lab = 1.5, cex.main = 1.5, cex.sub = 1.2, cex.cap = 1.2,
#'   col = "#3b2209", col.axis = "#5c3a1e", col.cap = "#7a5230", 
#'   col.lab = "#3b2209", col.main = "#1a0f04", col.sub = "#7a5230",
#'   grid = TRUE, grid.col = "#c9a96e", grid.lty = "dotted",
#'   facet.bg = "#e8d4a8", facet.border = "#5c3a1e",
#'   pch = 4,
#'   palette.qualitative = c(
#'     "#8b0000", "#1a5276", "#196f3d", "#7d6608",
#'     "#6c3483", "#a04000", "#1b4f72", "#145a32"
#'   )
#' )
#' tinyplot(
#'   Sepal.Length ~ Petal.Length | Species, iris,
#'   main = "Avast, me hearties!",
#'   sub  = "x marks the petal",
#'   cap  = "Ye Olde Iris Data", 
#'   theme = "pirate"
#' )
#' tinytheme_unregister("pirate")
#'
#' @export
tinytheme_register = function(name, theme = "default", ...) {
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  builtins = builtin_themes
  if (name %in% builtins) {
    stop(
      sprintf("'%s' is a built-in theme and cannot be overridden.", name),
      call. = FALSE
    )
  }

  if (is.character(theme) && length(theme) == 1) {
    base_theme = get_theme_def(theme)
    if (is.null(base_theme)) {
      stop(sprintf("Base theme '%s' not found.", theme), call. = FALSE)
    }
  } else if (is.list(theme)) {
    base_theme = theme
  } else {
    stop("`theme` must be a theme name (string) or a list.", call. = FALSE)
  }

  dots = list(...)
  new_theme = if (length(dots) > 0) modifyList(base_theme, dots) else base_theme
  new_theme[["tinytheme"]] = name

  registry = get_environment_variable(".registered_themes") %||% list()
  registry[[name]] = new_theme
  set_environment_variable(.registered_themes = registry)
  invisible(new_theme)
}


#' List Available Themes
#'
#' @md
#' @description
#' Returns the names of all available themes, both built-in and user-registered.
#'
#' @return A named list with two character vectors: `builtin` and `registered`.
#'
#' @seealso [tinytheme()], [tinytheme_register()]
#'
#' @examples
#' tinytheme_list()
#'
#' @export
tinytheme_list = function() {
  builtins = builtin_themes
  registered = names(get_environment_variable(".registered_themes"))
  list(builtin = builtins, registered = registered)
}


#' Unregister a Custom Theme
#'
#' @md
#' @description
#' Remove a previously registered custom theme. Does not reset an active theme;
#' it only removes the theme from the registry so it can no longer be selected
#' by name.
#'
#' @param name Character string. The name of the registered theme to remove.
#'
#' @return Returns `NULL` (invisibly).
#'
#' @seealso [tinytheme_register()], [tinytheme_list()]
#'
#' @examples
#' tinytheme_register("my_theme", .base = "clean", grid = FALSE)
#' tinytheme_unregister("my_theme")
#'
#' @export
tinytheme_unregister = function(name) {
  registry = get_environment_variable(".registered_themes")
  if (!name %in% names(registry)) {
    warning(
      sprintf("Theme '%s' is not registered. Nothing to remove.", name),
      call. = FALSE
    )
    return(invisible(NULL))
  }
  registry[[name]] = NULL
  set_environment_variable(.registered_themes = registry)
  invisible(NULL)
}
