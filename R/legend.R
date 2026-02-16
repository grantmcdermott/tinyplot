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
## Helper Functions -----
#

# Unit conversion helpers (used extensively throughout legend positioning)
lines_to_npc_x = function(val) {
  grconvertX(val, from = "lines", to = "npc") - grconvertX(0, from = "lines", to = "npc")
}

lines_to_user_x = function(val) {
  grconvertX(val, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user")
}

lines_to_user_y = function(val) {
  grconvertY(val, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
}


# Adjust margins for outer legend placement, measure, and apply soma
legend_outer_margins = function(legend_env, apply = TRUE) {
  omar = legend_env$omar
  ooma = legend_env$ooma
  lmar = legend_env$lmar

  # Step 1: Prepare margins before measuring
  if (legend_env$outer_side) {
    # Extra bump for spineplot if outer_right legend (to accommodate secondary y-axis)
    if (identical(legend_env$type, "spineplot")) {
      lmar[1] = lmar[1] + 1.1
    }

    # Set inner margins before fake legend is drawn
    if (legend_env$outer_right) {
      omar[4] = 0
    } else {
      # For outer left we have to account for the y-axis label too
      omar[2] = par("mgp")[1] + 1 * par("cex.lab")
    }
    par(mar = omar)

    if (legend_env$new_plot) {
      plot.new()
      # For themed + dynamic plots, reinstate adjusted plot margins
      if (legend_env$dynmar) {
        omar = par("mar")
        if (legend_env$outer_right) {
          omar[4] = 0
        } else {
          omar[2] = par("mgp")[1] + 1 * par("cex.lab")
        }
        par(mar = omar)
      }
    }

  } else if (legend_env$outer_end) {
    # Set inner margins before fake legend is drawn
    if (legend_env$outer_bottom) {
      omar[1] = par("mgp")[1] + 1 * par("cex.lab")
      if (legend_env$has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]] == 1)) {
        omar[1] = omar[1] + 1 * par("cex.sub")
      }
    } else {
      # For "top!", expand existing inner margin rather than outer margin
      ooma[3] = ooma[3] + legend_env$topmar_epsilon
      par(oma = ooma)
    }
    par(mar = omar)

    if (legend_env$new_plot) {
      plot.new()
      # For themed + dynamic plots, reinstate adjusted plot margins
      if (legend_env$dynmar) {
        omar = par("mar")
        if (legend_env$outer_bottom) {
          omar[1] = theme_dynamic$mgp[1] + 1 * par("cex.lab")
          if (legend_env$has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]] == 1)) {
            omar[1] = omar[1] + 1 * par("cex.sub")
          }
        } else {
          ooma[3] = ooma[3] + legend_env$topmar_epsilon
          par(oma = ooma)
        }
        par(mar = omar)
      }
    }
  } else {
    if (legend_env$new_plot) plot.new()
  }

  # Update legend environment with prepared margins
  legend_env$omar = omar
  legend_env$ooma = ooma
  legend_env$lmar = lmar

  # Step 2: Measure legend dimensions
  legend_env$dims = measure_fake_legend(legend_env)

  # Step 3: Apply soma if drawing
  if (apply) {
    soma = if (legend_env$outer_side) {
      grconvertX(legend_env$dims$rect$w, to = "lines") - grconvertX(0, to = "lines")
    } else if (legend_env$outer_end) {
      grconvertY(legend_env$dims$rect$h, to = "lines") - grconvertY(0, to = "lines")
    } else {
      0
    }
    soma = soma + sum(legend_env$lmar)

    if (legend_env$outer_side) {
      legend_env$ooma[if (legend_env$outer_right) 4 else 2] = soma
    } else if (legend_env$outer_end) {
      if (legend_env$outer_bottom) {
        legend_env$ooma[1] = soma
      } else {
        legend_env$omar[3] = legend_env$omar[3] + soma - legend_env$topmar_epsilon
        par(mar = legend_env$omar)
      }
    }
    par(oma = legend_env$ooma)
  }
}


# Calculate legend inset for outer placement
measure_legend_inset = function(legend_env) {
  if (legend_env$outer_side) {
    inset_val = lines_to_npc_x(legend_env$lmar[1])
    # Extra space needed for "left!" because of lhs inner margin
    if (!legend_env$outer_right) {
      inset_val = inset_val + lines_to_npc_x(par("mar")[2])
    }
    c(1 + inset_val, 0)

  } else if (legend_env$outer_end) {
    # Note: Y-direction uses grconvertY (not lines_to_npc_x which is X-only)
    inset_val = grconvertY(legend_env$lmar[1], from = "lines", to = "npc") -
                grconvertY(0, from = "lines", to = "npc")
    if (legend_env$outer_bottom) {
      # Extra space needed for "bottom!" because of lhs inner margin
      inset_bump = grconvertY(par("mar")[1], from = "lines", to = "npc") -
                   grconvertY(0, from = "lines", to = "npc")
      inset_val = inset_val + inset_bump
    } else {
      epsilon_bump = grconvertY(legend_env$topmar_epsilon, from = "lines", to = "npc") -
                     grconvertY(0, from = "lines", to = "npc")
      inset_val = inset_val + epsilon_bump
    }
    c(0, 1 + inset_val)

  } else {
    0
  }
}


# Internal workhorse function for legend rendering
# This function is called inside recordGraphics() so that all coordinate-dependent
# calculations are re-executed when the plot window is resized
tinylegend = function(legend_env) {
  # Reset to base values (before soma/inset were applied)
  # This is necessary because legend_env is passed by reference and modifications
  # persist across recordGraphics() replays
  legend_env$omar = legend_env$omar_base
  legend_env$ooma = legend_env$ooma_base
  legend_env$args[["inset"]] = legend_env$inset_base

  # Re-measure legend dimensions (device size may have changed on resize)
  legend_env$dims = measure_fake_legend(legend_env)

  # Calculate and apply soma (outer margin adjustment based on legend size)
  soma = if (legend_env$outer_side) {
    grconvertX(legend_env$dims$rect$w, to = "lines") - grconvertX(0, to = "lines")
  } else if (legend_env$outer_end) {
    grconvertY(legend_env$dims$rect$h, to = "lines") - grconvertY(0, to = "lines")
  } else {
    0
  }
  soma = soma + sum(legend_env$lmar)

  if (legend_env$outer_side) {
    legend_env$ooma[if (legend_env$outer_right) 4 else 2] = soma
  } else if (legend_env$outer_end) {
    if (legend_env$outer_bottom) {
      legend_env$ooma[1] = soma
    } else {
      legend_env$omar[3] = legend_env$omar[3] + soma - legend_env$topmar_epsilon
      par(mar = legend_env$omar)
    }
  }
  par(oma = legend_env$ooma)

  # Calculate inset for legend positioning
  legend_env$inset = measure_legend_inset(legend_env)

  # Refresh plot area for exact inset spacing
  # (Uses hook to preserve existing plot with par(new = TRUE))
  oldhook = getHook("before.plot.new")
  setHook("before.plot.new", function() par(new = TRUE), action = "append")
  setHook("before.plot.new", function() par(mar = legend_env$omar), action = "append")
  plot.new()
  setHook("before.plot.new", oldhook, action = "replace")

  # Set the inset in legend args
  legend_env$args[["inset"]] = if (legend_env$user_inset) {
    legend_env$args[["inset"]] + legend_env$inset
  } else {
    legend_env$inset
  }

  # Draw the legend
  if (legend_env$gradient) {
    # Ensure col is set correctly for gradients
    if (!more_than_n_unique(legend_env$args[["col"]], 1)) {
      if (!is.null(legend_env$args[["pt.bg"]]) && length(legend_env$args[["pt.bg"]]) == 100) {
        legend_env$args[["col"]] = legend_env$args[["pt.bg"]]
      }
    }

    draw_gradient_swatch(
      legend_args = legend_env$args,
      fklgnd = legend_env$dims,
      lmar = legend_env$lmar,
      outer_side = legend_env$outer_side,
      outer_end = legend_env$outer_end,
      outer_right = legend_env$outer_right,
      outer_bottom = legend_env$outer_bottom,
      user_inset = legend_env$user_inset
    )
  } else {
    do.call("legend", legend_env$args)
  }
}


# Measure legend dimensions using a fake (non-plotted) legend
measure_fake_legend = function(legend_env) {
  fklgnd.args = modifyList(
    legend_env$args,
    list(plot = FALSE),
    keep.null = TRUE
  )

  if (legend_env$gradient) {
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
    if (legend_env$outer_end) {
      fklgnd.args = modifyList(
        fklgnd.args,
        list(title = NULL),
        keep.null = TRUE
      )
    }
  }

  do.call("legend", fklgnd.args)
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
      "add",
      "bubble",
      "bubble_cex",
      "by",
      "by_continuous",
      "cex_dep",
      "cex_fct_adj",
      "col",
      "datapoints",
      "legend",
      "legend_args",
      "ngrps",
      "null_by",
      "sub",
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
  has_sub = text_line_count(sub) > 0L

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


#' Build legend arguments list
#'
#' @description Constructs and configures the legend_args list by:
#'   - Sanitizing legend input
#'   - Setting defaults for all legend parameters
#'   - Computing positioning flags from original position (before transformation)
#'   - Adjusting position anchors for outer legends
#'   - Adjusting for special cases (gradient, horizontal, multi-column)
#'   - Populating legend_env with args and positioning flags
#'
#' @param legend_env Legend environment to populate
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
#' @returns NULL (modifies legend_env in-place)
#'
#' @keywords internal
build_legend_args = function(
  legend_env,

  # Legend specification
  legend,
  legend_args,

  # Labels and grouping
  by_dep,
  lgnd_labs,
  labeller = NULL,

  # Visual aesthetics
  type,
  pch,
  lty,
  lwd,
  col,
  bg,
  cex,

  # Configuration
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

  # Determine positioning flags for anchor adjustment
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
  mcol_flag = !is.null(legend_args[["ncol"]]) && legend_args[["ncol"]] > 1
  user_inset = !is.null(legend_args[["inset"]])

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

  # Populate legend environment with args and flags
  legend_env$args = legend_args
  legend_env$mcol = mcol_flag
  legend_env$user_inset = user_inset
  legend_env$outer_side = outer_side
  legend_env$outer_end = outer_end
  legend_env$outer_right = outer_right
  legend_env$outer_bottom = outer_bottom
}


#' Build legend environment
#'
#' @description Creates the legend environment by:
#'   - Initializing environment with metadata
#'   - Calling build_legend_args() to construct legend arguments
#'   - Populating environment with arguments and positioning flags
#'   - Initializing margins and dimensions
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
#' @param lmar Legend margins (inner, outer)
#' @param has_sub Logical indicating presence of sub-caption
#' @param new_plot Logical indicating if plot.new should be called
#'
#' @returns Environment with complete legend specification
#'
#' @keywords internal
build_legend_env = function(
  # Legend specification
  legend,
  legend_args,

  # Labels and grouping
  by_dep,
  lgnd_labs,
  labeller = NULL,

  # Visual aesthetics
  type,
  pch,
  lty,
  lwd,
  col,
  bg,
  cex,

  # Configuration
  gradient,
  lmar,
  has_sub = FALSE,
  new_plot = TRUE
) {
  # Create legend environment
  legend_env = new.env(parent = emptyenv())

  # Initialize metadata
  legend_env$gradient = gradient
  legend_env$type = type
  legend_env$has_sub = has_sub
  legend_env$new_plot = new_plot
  legend_env$dynmar = isTRUE(.tpar[["dynmar"]])
  legend_env$topmar_epsilon = 0.1

  # Build legend arguments (modifies legend_env in-place)
  build_legend_args(
    legend_env = legend_env,
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

  # Initialize margins
  legend_env$omar = par("mar")
  legend_env$ooma = par("oma")
  legend_env$lmar = lmar

  # Initialize dimensions and layout
  legend_env$dims = NULL
  legend_env$inset = NULL
  legend_env$rasterbox = NULL

  return(legend_env)
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

  # Restore margin defaults
  dynmar = isTRUE(.tpar[["dynmar"]])
  restore_margin_outer()
  if (!dynmar) {
    restore_margin_inner(par("oma"), topmar_epsilon = 0.1)
  }

  # Build legend environment
  legend_env = build_legend_env(
    # Legend specification
    legend = legend,
    legend_args = legend_args,

    # Labels and grouping
    by_dep = by_dep,
    lgnd_labs = lgnd_labs,
    labeller = labeller,

    # Visual aesthetics
    type = type,
    pch = pch,
    lty = lty,
    lwd = lwd,
    col = col,
    bg = bg,
    cex = cex,

    # Configuration
    gradient = gradient,
    lmar = lmar,
    has_sub = has_sub,
    new_plot = new_plot
  )

  # Initial setup: adjust margins, call plot.new, and measure (but don't apply soma yet)
  legend_outer_margins(legend_env, apply = FALSE)

  if (!draw) {
    return(legend_env$dims)
  }

  # Store base values AFTER legend_outer_margins setup (before soma/inset are applied)
  # These are needed so tinylegend() can reset to them on each recordGraphics replay
  legend_env$omar_base = legend_env$omar
  legend_env$ooma_base = legend_env$ooma
  legend_env$inset_base = legend_env$args[["inset"]]

  # Wrap margin application, inset calculation, and drawing in recordGraphics()
  # so everything recalculates correctly when the plot window is resized
  recordGraphics(
    tinylegend(legend_env),
    list = list(legend_env = legend_env),
    env = getNamespace("tinyplot")
  )
}
