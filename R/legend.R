#
## Helper Functions -----
#

# Unit conversion helpers (used extensively throughout legend positioning)
lines_to_npc = function(val) {
  grconvertX(val, from = "lines", to = "npc") - grconvertX(0, from = "lines", to = "npc")
}

lines_to_user_x = function(val) {
  grconvertX(val, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user")
}

lines_to_user_y = function(val) {
  grconvertY(val, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
}

# Adjust margins for outer legend placement
legend_outer_margins_prepare = function(spec) {
  omar = spec$margins$omar
  ooma = spec$margins$ooma
  lmar = spec$margins$lmar

  if (spec$flags$outer_side) {
    # Extra bump for spineplot if outer_right legend (to accommodate secondary y-axis)
    if (identical(spec$meta$type, "spineplot")) {
      lmar[1] = lmar[1] + 1.1
    }

    # Set inner margins before fake legend is drawn
    if (spec$flags$outer_right) {
      omar[4] = 0
    } else {
      # For outer left we have to account for the y-axis label too
      omar[2] = par("mgp")[1] + 1 * par("cex.lab")
    }
    par(mar = omar)

    if (spec$meta$new_plot) {
      plot.new()
      # For themed + dynamic plots, reinstate adjusted plot margins
      if (spec$meta$dynmar) {
        omar = par("mar")
        if (spec$flags$outer_right) {
          omar[4] = 0
        } else {
          omar[2] = par("mgp")[1] + 1 * par("cex.lab")
        }
        par(mar = omar)
      }
    }

  } else if (spec$flags$outer_end) {
    # Set inner margins before fake legend is drawn
    if (spec$flags$outer_bottom) {
      omar[1] = par("mgp")[1] + 1 * par("cex.lab")
      if (spec$meta$has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]] == 1)) {
        omar[1] = omar[1] + 1 * par("cex.sub")
      }
    } else {
      # For "top!", expand existing inner margin rather than outer margin
      ooma[3] = ooma[3] + spec$meta$topmar_epsilon
      par(oma = ooma)
    }
    par(mar = omar)

    if (spec$meta$new_plot) {
      plot.new()
      # For themed + dynamic plots, reinstate adjusted plot margins
      if (spec$meta$dynmar) {
        omar = par("mar")
        if (spec$flags$outer_bottom) {
          omar[1] = theme_clean$mgp[1] + 1 * par("cex.lab")
          if (spec$meta$has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]] == 1)) {
            omar[1] = omar[1] + 1 * par("cex.sub")
          }
        } else {
          ooma[3] = ooma[3] + spec$meta$topmar_epsilon
          par(oma = ooma)
        }
        par(mar = omar)
      }
    }
  } else {
    if (spec$meta$new_plot) plot.new()
  }

  list(omar = omar, ooma = ooma, lmar = lmar)
}

# Calculate legend inset for outer placement
measure_legend_inset = function(spec) {
  if (spec$flags$outer_side) {
    inset_val = lines_to_npc(spec$margins$lmar[1])
    # Extra space needed for "left!" because of lhs inner margin
    if (!spec$flags$outer_right) {
      inset_val = inset_val + lines_to_npc(par("mar")[2])
    }
    c(1 + inset_val, 0)

  } else if (spec$flags$outer_end) {
    # Note: Y-direction uses grconvertY (not lines_to_npc which is X-only)
    inset_val = grconvertY(spec$margins$lmar[1], from = "lines", to = "npc") -
                grconvertY(0, from = "lines", to = "npc")
    if (spec$flags$outer_bottom) {
      # Extra space needed for "bottom!" because of lhs inner margin
      inset_bump = grconvertY(par("mar")[1], from = "lines", to = "npc") -
                   grconvertY(0, from = "lines", to = "npc")
      inset_val = inset_val + inset_bump
    } else {
      epsilon_bump = grconvertY(spec$meta$topmar_epsilon, from = "lines", to = "npc") -
                     grconvertY(0, from = "lines", to = "npc")
      inset_val = inset_val + epsilon_bump
    }
    c(0, 1 + inset_val)

  } else {
    0
  }
}

# Measure legend dimensions using a fake (non-plotted) legend
measure_fake_legend = function(spec) {
  fklgnd.args = modifyList(
    spec$args,
    list(plot = FALSE),
    keep.null = TRUE
  )

  if (spec$flags$gradient) {
    lgnd_labs_tmp = na.omit(fklgnd.args[["legend"]])
    if (length(lgnd_labs_tmp) < 5L) {
      nmore = 5L - length(lgnd_labs_tmp)
      lgnd_labs_tmp = c(lgnd_labs_tmp, rep("", nmore))
    }
    fklgnd.args = modifyList(
      fklgnd.args,
      list(legend = lgnd_labs_tmp),
      keep.null = TRUE
    )
    if (spec$flags$outer_end) {
      fklgnd.args = modifyList(
        fklgnd.args,
        list(title = NULL),
        keep.null = TRUE
      )
    }
  }

  do.call("legend", fklgnd.args)
}

# Calculate and apply soma (outer margin size) based on legend dimensions
legend_outer_margins_apply = function(spec) {
  # Calculate size
  soma = if (spec$flags$outer_side) {
    grconvertX(spec$dims$rect$w, to = "lines") - grconvertX(0, to = "lines")
  } else if (spec$flags$outer_end) {
    grconvertY(spec$dims$rect$h, to = "lines") - grconvertY(0, to = "lines")
  } else {
    0
  }
  soma = soma + sum(spec$margins$lmar)

  # Apply to appropriate margin
  ooma = spec$margins$ooma
  omar = spec$margins$omar

  if (spec$flags$outer_side) {
    ooma[if (spec$flags$outer_right) 4 else 2] = soma
  } else if (spec$flags$outer_end) {
    if (spec$flags$outer_bottom) {
      ooma[1] = soma
    } else {
      omar[3] = omar[3] + soma - spec$meta$topmar_epsilon
      par(mar = omar)
    }
  }
  par(oma = ooma)

  list(ooma = ooma, omar = omar)
}


#
## Input Sanitization -----
#

#' Sanitize and normalize legend input
#'
#' @description Converts various legend input formats (NULL, character, list,
#'   call) into a standardized legend_args list with an "x" position element.
#'
#' @param legend Legend specification (NULL, character, list, or call)
#' @param legend_args Existing legend_args list to merge with
#'
#' @returns Normalized legend_args list with at least an "x" element
#'
#' @keywords internal
sanitize_legend = function(legend, legend_args) {
  if (is.null(legend_args[["x"]])) {

    # Normalize legend to a list
    largs = if (is.null(legend)) {
      list(x = "right!")
    } else if (is.character(legend)) {
      list(x = legend)
    } else if (is.list(legend)) {
      # Handle unnamed first element as position
      if (length(legend) >= 1 && is.character(legend[[1]]) &&
          (is.null(names(legend)) || names(legend)[1] == "")) {
        names(legend)[1] = "x"
      }
      legend
    } else if (inherits(legend, c("call", "name"))) {
      # Convert call to list and handle unnamed first arg as position
      new_legend = as.list(legend)[-1]  # Remove function name
      if (length(new_legend) >= 1 && (is.null(names(new_legend)) || names(new_legend)[1] == "")) {
        names(new_legend)[1] = "x"
      }
      new_legend
    } else {
      list(x = "right!")  # Fallback
    }

    # Ensure position exists
    if (is.null(largs[["x"]])) largs[["x"]] = "right!"

    # Merge
    legend_args = modifyList(legend_args, largs, keep.null = TRUE)
  }

  legend_args
}


#
## Legend Context & Preparation -----
#

#' Prepare legend context from settings
#'
#' @description Main orchestrator that determines:
#'   - Whether to draw legend
#'   - Legend labels and formatting
#'   - Whether multi-legend is needed (for bubble charts)
#'   - Gradient legend setup for continuous grouping
#'
#' @param settings Settings environment from tinyplot
#'
#' @returns NULL (modifies settings environment in-place)
#'
#' @keywords internal
prepare_legend = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "col",
      "by_continuous",
      "by",
      "bubble",
      "null_by",
      "legend",
      "legend_args",
      "bubble_cex",
      "cex_fct_adj",
      "cex_dep",
      "add",
      "sub",
      "ngrps",
      "datapoints",
      "ylab"
    )
  )

  ncolors = length(col)
  lgnd_labs = rep(NA, times = ncolors)

  # Generate labels for continuous (gradient) legends
  if (isTRUE(by_continuous)) {
    nlabs = 5
    ubyvar = unique(by)
    byvar_range = range(ubyvar)
    pbyvar = pretty(byvar_range, n = nlabs)
    pbyvar = pbyvar[pbyvar >= byvar_range[1] & pbyvar <= byvar_range[2]]
    if (length(ubyvar) == 2 && all(ubyvar %in% pbyvar)) {
      pbyvar = ubyvar
    } else if (length(pbyvar) > nlabs) {
      pbyvar = pbyvar[seq_along(pbyvar) %% 2 == 0]
    }
    pidx = rescale_num(c(byvar_range, pbyvar), to = c(1, ncolors))[-c(1:2)]
    pidx = round(pidx)
    lgnd_labs[pidx] = pbyvar
  }

  has_legend = FALSE
  multi_legend = bubble && !null_by && !isFALSE(legend)
  lgnd_cex = NULL

  # Normalize legend argument
  if (isFALSE(legend)) {
    legend = "none"
  } else if (isTRUE(legend)) {
    legend = NULL
  }

  if (!is.null(legend) && is.character(legend) && legend == "none") {
    legend_args[["x"]] = "none"
    multi_legend = FALSE
  }

  # Handle bubble-only legend (no grouping)
  if (null_by) {
    if (bubble && !multi_legend) {
      legend_args[["title"]] = cex_dep
      lgnd_labs = names(bubble_cex)
      lgnd_cex = bubble_cex * cex_fct_adj
    } else if (is.null(legend)) {
      legend = "none"
      legend_args[["x"]] = "none"
    }
  }

  legend_draw_flag = (is.null(legend) || !is.character(legend) || legend != "none" || bubble) && !isTRUE(add)
  has_sub = !is.null(sub)

  # Generate labels for discrete legends
  if (legend_draw_flag && isFALSE(by_continuous) && (!bubble || multi_legend)) {
    if (ngrps > 1) {
      lgnd_labs = if (is.factor(datapoints$by)) levels(datapoints$by) else unique(datapoints$by)
    } else {
      lgnd_labs = ylab
    }
  }

  env2env(
    environment(),
    settings,
    c(
      "lgnd_labs",
      "has_legend",
      "multi_legend",
      "lgnd_cex",
      "legend",
      "legend_args",
      "legend_draw_flag",
      "has_sub"
    )
  )
}


#' Build legend specification
#'
#' @description Constructs a complete legend_args list by:
#'   - Sanitizing legend input
#'   - Setting defaults for all legend parameters
#'   - Computing positioning flags (outer_side, outer_right, etc.)
#'   - Adjusting for special cases (gradient, horizontal, multi-column)
#'
#' @param legend Legend placement keyword or list
#' @param legend_args Additional legend arguments
#' @param by_dep The (deparsed) "by" grouping variable name
#' @param lgnd_labs The legend labels
#' @param labeller Character or function for formatting labels
#' @param type Plot type
#' @param pch Plotting character(s)
#' @param lty Line type(s)
#' @param lwd Line width(s)
#' @param col Color(s)
#' @param bg Background fill color(s)
#' @param cex Character expansion(s)
#' @param gradient Logical indicating gradient legend
#'
#' @returns List with legend_args and positioning flags
#'
#' @keywords internal
build_legend_spec = function(
  legend,
  legend_args,
  by_dep,
  lgnd_labs,
  labeller = NULL,
  type,
  pch,
  lty,
  lwd,
  col,
  bg,
  cex,
  gradient
) {
  legend_args = sanitize_legend(legend, legend_args)

  # Set defaults
  if (!exists("title", where = legend_args)) legend_args[["title"]] = by_dep
  legend_args[["pch"]] = legend_args[["pch"]] %||% pch
  legend_args[["lty"]] = legend_args[["lty"]] %||% lty
  legend_args[["col"]] = legend_args[["col"]] %||% col
  legend_args[["bty"]] = legend_args[["bty"]] %||% "n"
  legend_args[["horiz"]] = legend_args[["horiz"]] %||% FALSE
  legend_args[["xpd"]] = legend_args[["xpd"]] %||% NA
  legend_args[["lwd"]] = legend_args[["lwd"]] %||% lwd

  # Special handling of pt.cex for bubble plots
  if (is.null(type) || type %in% c("p", "text")) {
    legend_args[["pt.cex"]] = legend_args[["pt.cex"]] %||% (cex %||% par("cex"))
  }

  # Gradient legend adjustments
  if (gradient) {
    legend_args[["pch"]] = 22
    legend_args[["pt.cex"]] = legend_args[["pt.cex"]] %||% 3.5
    legend_args[["y.intersp"]] = legend_args[["y.intersp"]] %||% 1.25
    legend_args[["seg.len"]] = legend_args[["seg.len"]] %||% 1.25
  }

  if (identical(type, "n") && isFALSE(gradient)) {
    legend_args[["pch"]] = legend_args[["pch"]] %||% par("pch")
  }

  # Special pt.bg handling for types that need color-based fills
  if (identical(type, "spineplot")) {
    legend_args[["pt.bg"]] = legend_args[["pt.bg"]] %||% legend_args[["col"]]
  } else if (identical(type, "ridge") && isFALSE(gradient)) {
    legend_args[["pt.bg"]] = legend_args[["pt.bg"]] %||% sapply(legend_args[["col"]], function(ccol) seq_palette(ccol, n = 2)[2])
  } else {
    legend_args[["pt.bg"]] = legend_args[["pt.bg"]] %||% bg
  }

  # Set legend labels
  legend_args[["legend"]] = legend_args[["legend"]] %||% lgnd_labs
  if (length(lgnd_labs) != length(eval(legend_args[["legend"]]))) {
    warning(
      "\nUser-supplied legend labels do not match the number of groups.\n",
      "Defaulting to automatic labels determined by the group splits in `by`,\n"
    )
    legend_args[["legend"]] = lgnd_labs
  }

  # Apply label formatter if provided
  if (!is.null(legend_args[["labeller"]])) {
    labeller = legend_args[["labeller"]]
    legend_args[["labeller"]] = NULL
    legend_args[["legend"]] = tinylabel(legend_args[["legend"]], labeller = labeller)
  }

  if (isTRUE(gradient)) {
    legend_args[["ncol"]] = NULL
  }

  # Flags
  mcol_flag = !is.null(legend_args[["ncol"]]) && legend_args[["ncol"]] > 1
  user_inset = !is.null(legend_args[["inset"]])

  # Determine positioning
  outer_side = outer_end = outer_right = outer_bottom = FALSE
  if (grepl("right!$|left!$", legend_args[["x"]])) {
    outer_side = TRUE
    outer_right = grepl("right!$", legend_args[["x"]])
  } else if (grepl("bottom!$|top!$", legend_args[["x"]])) {
    outer_end = TRUE
    outer_bottom = grepl("bottom!$", legend_args[["x"]])
  }

  # Adjust position anchor (we'll position relative to opposite side)
  if (outer_end) {
    if (outer_bottom) {
      legend_args[["x"]] = gsub("bottom!$", "top", legend_args[["x"]])
    } else {
      legend_args[["x"]] = gsub("top!$", "bottom", legend_args[["x"]])
    }
  } else if (outer_side) {
    if (outer_right) {
      legend_args[["x"]] = gsub("right!$", "left", legend_args[["x"]])
    } else {
      legend_args[["x"]] = gsub("left!$", "right", legend_args[["x"]])
    }
  }

  # Additional positioning adjustments
  if (outer_end) {
    # Enforce horizontal legend if user hasn't specified ncol arg
    if (is.null(legend_args[["ncol"]]) || gradient) legend_args[["horiz"]] = TRUE
  } else if (!outer_side) {
    legend_args[["inset"]] = 0
  }

  # Additional tweaks for horizontal and/or multi-column legends
  if (isTRUE(legend_args[["horiz"]]) || mcol_flag) {
    # Tighter horizontal labelling
    if (!gradient) {
      legend_args[["text.width"]] = NA
      # Add a space to all labs except the outermost right ones
      nlabs = length(legend_args[["legend"]])
      nidx = nlabs
      if (mcol_flag) nidx = tail(1:nlabs, (nlabs %/% legend_args[["ncol"]]))
      legend_args[["legend"]][-nidx] = paste(legend_args[["legend"]][-nidx], " ")
    }
    # Catch for horizontal ribbon legend spacing
    if (type == "ribbon") {
      if (legend_args[["pt.lwd"]] == 1) {
        legend_args[["x.intersp"]] = 1
      } else {
        legend_args[["x.intersp"]] = 0.5
      }
    } else if (gradient) {
      legend_args[["x.intersp"]] = 0.5
    }
  }

  list(
    legend_args = legend_args,
    mcol_flag = mcol_flag,
    user_inset = user_inset,
    outer_side = outer_side,
    outer_end = outer_end,
    outer_right = outer_right,
    outer_bottom = outer_bottom
  )
}


#
## Single Legend Rendering -----
#

#' Calculate placement and draw legend
#'
#' @description Main exported function for drawing legends. Supports:
#'   - Inner and outer positioning (with "!" suffix)
#'   - Discrete and continuous (gradient) legends
#'   - Automatic margin adjustment
#'
#' @md
#' @param legend Legend placement keyword or list, passed down from [tinyplot].
#' @param legend_args Additional legend arguments to be passed to
#'   \code{\link[graphics]{legend}}.
#' @param by_dep The (deparsed) "by" grouping variable name.
#' @param lgnd_labs The labels passed to `legend(legend = ...)`.
#' @param labeller Character or function for formatting the labels (`lgnd_labs`).
#'   Passed down to [`tinylabel`].
#' @param type Plotting type(s), passed down from [tinyplot].
#' @param pch Plotting character(s), passed down from [tinyplot].
#' @param lty Plotting linetype(s), passed down from [tinyplot].
#' @param lwd Plotting line width(s), passed down from [tinyplot].
#' @param col Plotting colour(s), passed down from [tinyplot].
#' @param bg Plotting character background fill colour(s), passed down from [tinyplot].
#' @param cex Plotting character expansion(s), passed down from [tinyplot].
#' @param gradient Logical indicating whether a continuous gradient swatch
#'   should be used to represent the colors.
#' @param lmar Legend margins (in lines). Should be a numeric vector of the form
#'   `c(inner, outer)`, where the first number represents the "inner" margin
#'   between the legend and the plot, and the second number represents the
#'   "outer" margin between the legend and edge of the graphics device. If no
#'   explicit value is provided by the user, then reverts back to `tpar("lmar")`
#'   for which the default values are `c(1.0, 0.1)`.
#' @param has_sub Logical. Does the plot have a sub-caption. Only used if
#'   keyword position is "bottom!", in which case we need to bump the legend
#'   margin a bit further.
#' @param new_plot Logical. Should we be calling plot.new internally?
#' @param draw Logical. If `FALSE`, no legend is drawn but the sizes are
#'   returned. Note that a new (blank) plot frame will still need to be started
#'   in order to perform the calculations.
#'
#' @returns No return value, called for side effect of producing a(n empty) plot
#'   with a legend in the margin.
#'
#' @importFrom graphics grconvertX grconvertY rasterImage strwidth
#' @importFrom grDevices as.raster recordGraphics
#' @importFrom utils modifyList
#'
#' @examples
#' oldmar = par("mar")
#'
#' draw_legend(
#'   legend = "right!", ## default (other options incl, "left(!)", ""bottom(!)", etc.)
#'   legend_args = list(title = "Key", bty = "o"),
#'   lgnd_labs = c("foo", "bar"),
#'   type = "p",
#'   pch = 21:22,
#'   col = 1:2
#' )
#'
#' # The legend is placed in the outer margin...
#' box("figure", col = "cyan", lty = 4)
#' # ... and the plot is proportionally adjusted against the edge of this
#' # margin.
#' box("plot")
#' # You can add regular plot objects per normal now
#' plot.window(xlim = c(1,10), ylim = c(1,10))
#' points(1:10)
#' points(10:1, pch = 22, col = "red")
#' axis(1); axis(2)
#' # etc.
#'
#' # Important: A side effect of draw_legend is that the inner margins have been
#' # adjusted. (Here: The right margin, since we called "right!" above.)
#' par("mar")
#'
#' # To reset you should call `dev.off()` or just reset manually.
#' par(mar = oldmar)
#'
#' # Note that the inner and outer margin of the legend itself can be set via
#' # the `lmar` argument. (This can also be set globally via
#' # `tpar(lmar = c(inner, outer))`.)
#' draw_legend(
#'   legend_args = list(title = "Key", bty = "o"),
#'   lgnd_labs = c("foo", "bar"),
#'   type = "p",
#'   pch = 21:22,
#'   col = 1:2,
#'   lmar = c(0, 0.1) ## set inner margin to zero
#' )
#' box("figure", col = "cyan", lty = 4)
#'
#' par(mar = oldmar)
#'
#' # Continuous (gradient) legends are also supported
#' draw_legend(
#'   legend = "right!",
#'   legend_args = list(title = "Key"),
#'   lgnd_labs = LETTERS[1:5],
#'   col = hcl.colors(5),
#'   gradient = TRUE ## enable gradient legend
#' )
#'
#' par(mar = oldmar)
#'
#' @export
draw_legend = function(
  legend = NULL,
  legend_args = NULL,
  by_dep = NULL,
  lgnd_labs = NULL,
  labeller = NULL,
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
) {
  if (is.null(lmar)) {
    lmar = tpar("lmar")
  } else {
    if (!is.numeric(lmar) || length(lmar) != 2) {
      stop("lmar must be a numeric of length 2.")
    }
  }

  assert_logical(gradient)
  assert_logical(has_sub)
  assert_logical(new_plot)
  assert_logical(draw)

  # Build complete legend arguments from inputs
  legend_build = build_legend_spec(
    legend = legend,
    legend_args = legend_args,
    by_dep = by_dep,
    lgnd_labs = lgnd_labs,
    labeller = labeller,
    type = type,
    pch = pch,
    lty = lty,
    lwd = lwd,
    col = col,
    bg = bg,
    cex = cex,
    gradient = gradient
  )

  # Restore margin defaults
  dynmar = isTRUE(.tpar[["dynmar"]])
  restore_margin_outer()
  if (!dynmar) {
    restore_margin_inner(par("oma"), topmar_epsilon = 0.1)
  }

  # Create spec object and populate from build_legend_spec results
  spec = structure(
    list(
      args = legend_build$legend_args,
      flags = list(
        outer_side   = legend_build$outer_side,
        outer_end    = legend_build$outer_end,
        outer_right  = legend_build$outer_right,
        outer_bottom = legend_build$outer_bottom,
        mcol         = legend_build$mcol_flag,
        user_inset   = legend_build$user_inset,
        gradient     = gradient
      ),
      margins = list(
        lmar = NULL,
        omar = NULL,
        ooma = NULL
      ),
      dims = NULL,
      layout = list(
        inset = NULL,
        rasterbox = NULL
      ),
      meta = list(
        type = type,
        has_sub = has_sub,
        new_plot = new_plot,
        dynmar = dynmar,
        topmar_epsilon = 0.1
      )
    ),
    class = "legend_spec"
  )

  # Initialize margins
  spec$margins$omar = par("mar")
  spec$margins$ooma = par("oma")
  spec$margins$lmar = tpar("lmar")

  # Adjust margins for outer placement
  margin_result = legend_outer_margins_prepare(spec)
  spec$margins = modifyList(spec$margins, margin_result)

  # Measure dimensions with fake legend
  spec$dims = measure_fake_legend(spec)

  if (!draw) {
    return(spec$dims)
  }

  # Calculate and apply soma (outer margin adjustment)
  margin_result = legend_outer_margins_apply(spec)
  spec$margins$ooma = margin_result$ooma
  spec$margins$omar = margin_result$omar

  # Calculate inset
  spec$layout$inset = measure_legend_inset(spec)

  # Refresh plot area for exact inset spacing
  oldhook = getHook("before.plot.new")
  setHook("before.plot.new", function() par(new = TRUE), action = "append")
  setHook("before.plot.new", function() par(mar = spec$margins$omar), action = "append")
  plot.new()
  setHook("before.plot.new", oldhook, action = "replace")

  # Set the inset in args
  spec$args[["inset"]] = if (spec$flags$user_inset) {
    spec$args[["inset"]] + spec$layout$inset
  } else {
    spec$layout$inset
  }

  # Draw wrapped in recordGraphics() to preserve spacing if plot is resized
  recordGraphics(
    {
      if (spec$flags$gradient) {
        # Ensure col is set correctly for gradients
        if (!more_than_n_unique(spec$args[["col"]], 1)) {
          if (!is.null(spec$args[["pt.bg"]]) && length(spec$args[["pt.bg"]]) == 100) {
            spec$args[["col"]] = spec$args[["pt.bg"]]
          }
        }

        draw_gradient_swatch(
          legend_args = spec$args,
          fklgnd = spec$dims,
          lmar = spec$margins$lmar,
          outer_side = spec$flags$outer_side,
          outer_end = spec$flags$outer_end,
          outer_right = spec$flags$outer_right,
          outer_bottom = spec$flags$outer_bottom,
          user_inset = spec$flags$user_inset
        )
      } else {
        do.call("legend", spec$args)
      }
    },
    list = list(spec = spec),
    env = getNamespace("tinyplot")
  )
}
