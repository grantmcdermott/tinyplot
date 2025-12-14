# LEGEND SYSTEM
#
# This file consolidates all legend-related functionality for tinyplot.
# Previously spread across 5 files, now organized into logical sections:
#
# 1. Input Sanitization
# 2. Legend Context & Preparation
# 3. Single Legend Rendering
# 4. Gradient Legend Rendering
# 5. Multi-Legend Rendering


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

#' Create legend specification object
#'
#' @description Creates a structured specification object that flows through
#'   the legend rendering pipeline, eliminating redundant state and parameter passing.
#'
#' @param legend_args List of legend arguments (x, legend, col, etc.)
#' @param type Plot type (for type-specific adjustments)
#' @param gradient Logical indicating if this is a gradient legend
#' @param has_sub Logical indicating if plot has subtitle
#' @param new_plot Logical indicating if new plot should be created
#' @param dynmar Logical indicating if dynamic margins are enabled
#'
#' @returns A legend_spec object containing args, flags, margins, dims, layout, and meta
#'
#' @keywords internal
create_legend_spec = function(legend_args, type, gradient, has_sub, new_plot, dynmar) {
  structure(
    list(
      # User-facing arguments
      args = legend_args,

      # Positioning flags (set during build phase)
      flags = list(
        outer_side   = FALSE,
        outer_end    = FALSE,
        outer_right  = FALSE,
        outer_bottom = FALSE,
        mcol         = FALSE,
        user_inset   = FALSE,
        gradient     = gradient
      ),

      # Margins (set during margin adjustment phase)
      margins = list(
        lmar = NULL,
        omar = NULL,
        ooma = NULL
      ),

      # Dimensions from fake legend (set during measure phase)
      dims = NULL,

      # Calculated layout (set during layout phase)
      layout = list(
        inset = NULL,
        rasterbox = NULL
      ),

      # Metadata
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
}

# Adjust margins for outer legend placement
adjust_margins_for_outer_legend = function(outer_side, outer_end, outer_right,
                                           outer_bottom, omar, ooma, has_sub,
                                           topmar_epsilon, type, lmar, new_plot, dynmar) {
  if (outer_side) {
    # Extra bump for spineplot if outer_right legend (to accommodate secondary y-axis)
    if (identical(type, "spineplot")) {
      lmar[1] = lmar[1] + 1.1
    }

    # Set inner margins before fake legend is drawn
    if (outer_right) {
      omar[4] = 0
    } else {
      # For outer left we have to account for the y-axis label too
      omar[2] = par("mgp")[1] + 1 * par("cex.lab")
    }
    par(mar = omar)

    if (new_plot) {
      plot.new()
      # For themed + dynamic plots, reinstate adjusted plot margins
      if (dynmar) {
        omar = par("mar")
        if (outer_right) {
          omar[4] = 0
        } else {
          omar[2] = par("mgp")[1] + 1 * par("cex.lab")
        }
        par(mar = omar)
      }
    }

  } else if (outer_end) {
    # Set inner margins before fake legend is drawn
    if (outer_bottom) {
      omar[1] = par("mgp")[1] + 1 * par("cex.lab")
      if (has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]] == 1)) {
        omar[1] = omar[1] + 1 * par("cex.sub")
      }
    } else {
      # For "top!", expand existing inner margin rather than outer margin
      ooma[3] = ooma[3] + topmar_epsilon
      par(oma = ooma)
    }
    par(mar = omar)

    if (new_plot) {
      plot.new()
      # For themed + dynamic plots, reinstate adjusted plot margins
      if (dynmar) {
        omar = par("mar")
        if (outer_bottom) {
          omar[1] = theme_clean$mgp[1] + 1 * par("cex.lab")
          if (has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]] == 1)) {
            omar[1] = omar[1] + 1 * par("cex.sub")
          }
        } else {
          ooma[3] = ooma[3] + topmar_epsilon
          par(oma = ooma)
        }
        par(mar = omar)
      }
    }
  } else {
    if (new_plot) plot.new()
  }

  list(omar = omar, ooma = ooma, lmar = lmar)
}

# Calculate legend inset for outer placement
calculate_legend_inset = function(outer_side, outer_end, outer_right, outer_bottom,
                                  lmar, omar, topmar_epsilon) {
  if (outer_side) {
    inset_val = lines_to_npc(lmar[1])
    # Extra space needed for "left!" because of lhs inner margin
    if (!outer_right) {
      inset_val = inset_val + lines_to_npc(par("mar")[2])
    }
    c(1 + inset_val, 0)

  } else if (outer_end) {
    # Note: Y-direction uses grconvertY (not lines_to_npc which is X-only)
    inset_val = grconvertY(lmar[1], from = "lines", to = "npc") -
                grconvertY(0, from = "lines", to = "npc")
    if (outer_bottom) {
      # Extra space needed for "bottom!" because of lhs inner margin
      inset_bump = grconvertY(par("mar")[1], from = "lines", to = "npc") -
                   grconvertY(0, from = "lines", to = "npc")
      inset_val = inset_val + inset_bump
    } else {
      epsilon_bump = grconvertY(topmar_epsilon, from = "lines", to = "npc") -
                     grconvertY(0, from = "lines", to = "npc")
      inset_val = inset_val + epsilon_bump
    }
    c(0, 1 + inset_val)

  } else {
    0
  }
}

# Prepare fake legend arguments for dimension measurement
prepare_fake_legend_args = function(legend_args, gradient, outer_end) {
  fklgnd.args = modifyList(
    legend_args,
    list(plot = FALSE),
    keep.null = TRUE
  )

  if (gradient) {
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
    if (outer_end) {
      fklgnd.args = modifyList(
        fklgnd.args,
        list(title = NULL),
        keep.null = TRUE
      )
    }
  }

  fklgnd.args
}

# Calculate and apply soma (outer margin size) based on legend dimensions
calculate_and_apply_soma = function(fklgnd, outer_side, outer_end, outer_right,
                                    outer_bottom, lmar, ooma, omar, topmar_epsilon) {
  # Calculate size
  soma = if (outer_side) {
    grconvertX(fklgnd$rect$w, to = "lines") - grconvertX(0, to = "lines")
  } else if (outer_end) {
    grconvertY(fklgnd$rect$h, to = "lines") - grconvertY(0, to = "lines")
  } else {
    0
  }
  soma = soma + sum(lmar)

  # Apply to appropriate margin
  if (outer_side) {
    ooma[if (outer_right) 4 else 2] = soma
  } else if (outer_end) {
    if (outer_bottom) {
      ooma[1] = soma
    } else {
      omar[3] = omar[3] + soma - topmar_epsilon
      par(mar = omar)
    }
  }
  par(oma = ooma)

  list(ooma = ooma, omar = omar)
}

# Draw vertical gradient legend labels, ticks, and title
draw_gradient_labels_vertical = function(rasterbox, lgnd_labs, legend_args, inner, outer_right) {
  labs_idx = !is.na(lgnd_labs)
  lgnd_labs[labs_idx] = paste0(" ", format(lgnd_labs[labs_idx]))

  # Determine anchors based on position
  if (!inner && !outer_right) {
    lbl_x_anchor = rasterbox[1]
    ttl_x_anchor = rasterbox[1] + max(strwidth(lgnd_labs[labs_idx]))
    lbl_adj = c(0, 0.5)
    ttl_adj = c(1, 0)
  } else {
    lbl_x_anchor = rasterbox[3]
    ttl_x_anchor = rasterbox[1]
    lbl_adj = c(0, 0.5)
    ttl_adj = c(0, 0)
  }

  # Draw labels
  text(
    x = lbl_x_anchor,
    y = seq(rasterbox[2], rasterbox[4], length.out = length(lgnd_labs)),
    labels = lgnd_labs,
    xpd = NA,
    adj = lbl_adj
  )

  # Draw tick marks (white dashes)
  lgnd_ticks = lgnd_labs
  lgnd_ticks[labs_idx] = "-   -"
  text(
    x = lbl_x_anchor,
    y = seq(rasterbox[2], rasterbox[4], length.out = length(lgnd_labs)),
    labels = lgnd_ticks,
    col = "white",
    xpd = NA,
    adj = c(1, 0.5)
  )

  # Draw title
  text(
    x = ttl_x_anchor,
    y = rasterbox[4] + lines_to_user_y(1),
    labels = legend_args[["title"]],
    xpd = NA,
    adj = ttl_adj
  )
}

# Draw horizontal gradient legend labels, ticks, and title
draw_gradient_labels_horizontal = function(rasterbox, lgnd_labs, legend_args) {
  # Legend labels
  text(
    x = seq(rasterbox[1], rasterbox[3], length.out = length(lgnd_labs)),
    y = rasterbox[4],
    labels = lgnd_labs,
    xpd = NA,
    adj = c(0.5, 1.25)
  )

  # Legend tick marks (white dashes)
  lgnd_ticks = lgnd_labs
  lgnd_ticks[!is.na(lgnd_ticks)] = "-   -"
  text(
    x = seq(rasterbox[1], rasterbox[3], length.out = length(lgnd_labs)),
    y = rasterbox[4],
    labels = lgnd_ticks,
    col = "white",
    xpd = NA,
    adj = c(0, 0.5),
    srt = 90
  )

  # Legend title
  text(
    x = rasterbox[1],
    y = rasterbox[4],
    labels = paste0(legend_args[["title"]], " "),
    xpd = NA,
    adj = c(1, -0.5)
  )
}


#
## Legend Spec Pipeline -----
#

#' Apply margin adjustments for outer legends
#'
#' @description Second stage of pipeline: initializes margins and adjusts
#'   them for outer legend placement.
#'
#' @param spec Legend specification object
#'
#' @returns Modified spec with margins populated
#'
#' @keywords internal
legend_spec_apply_margins = function(spec) {
  # Get current margins
  spec$margins$omar = par("mar")
  spec$margins$ooma = par("oma")
  spec$margins$lmar = tpar("lmar")

  # Adjust for outer placement
  margin_result = adjust_margins_for_outer_legend(
    spec$flags$outer_side,
    spec$flags$outer_end,
    spec$flags$outer_right,
    spec$flags$outer_bottom,
    spec$margins$omar,
    spec$margins$ooma,
    spec$meta$has_sub,
    spec$meta$topmar_epsilon,
    spec$meta$type,
    spec$margins$lmar,
    spec$meta$new_plot,
    spec$meta$dynmar
  )

  spec$margins = modifyList(spec$margins, margin_result)
  spec
}

#' Calculate legend layout (inset and rasterbox)
#'
#' @description Fourth stage of pipeline: calculates inset for positioning
#'   and rasterbox coordinates for gradient legends.
#'
#' @param spec Legend specification object
#' @param draw Logical indicating if this is for actual drawing (vs measurement)
#'
#' @returns Modified spec with layout populated
#'
#' @keywords internal
legend_spec_layout = function(spec, draw = TRUE) {
  if (!draw) {
    return(spec)
  }

  # Calculate and apply soma (outer margin adjustment)
  margin_result = calculate_and_apply_soma(
    spec$dims,
    spec$flags$outer_side,
    spec$flags$outer_end,
    spec$flags$outer_right,
    spec$flags$outer_bottom,
    spec$margins$lmar,
    spec$margins$ooma,
    spec$margins$omar,
    spec$meta$topmar_epsilon
  )
  spec$margins$ooma = margin_result$ooma
  spec$margins$omar = margin_result$omar

  # Calculate inset
  spec$layout$inset = calculate_legend_inset(
    spec$flags$outer_side,
    spec$flags$outer_end,
    spec$flags$outer_right,
    spec$flags$outer_bottom,
    spec$margins$lmar,
    spec$margins$omar,
    spec$meta$topmar_epsilon
  )

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

  spec
}

#' Draw legend from specification
#'
#' @description Final stage of pipeline: draws the actual legend.
#'
#' @param spec Legend specification object
#'
#' @returns NULL (called for side effect of drawing legend)
#'
#' @keywords internal
legend_spec_draw = function(spec) {
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
prepare_legend_context = function(settings) {
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


#' Prepare multi-legend specifications
#'
#' @description Sets up multiple legend specifications for multi-legends
#'   (e.g., color grouping + bubble size). Creates `lgby` and `lgbub` objects
#'   that will be passed to draw_multi_legend().
#'
#' @param settings Settings environment from tinyplot
#'
#' @returns NULL (modifies settings environment in-place)
#'
#' @keywords internal
prepare_multi_legend = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "legend",
      "legend_args",
      "by_dep",
      "lgnd_labs",
      "type",
      "pch",
      "lty",
      "lwd",
      "col",
      "bg",
      "by_continuous",
      "lgnd_cex",
      "cex_dep",
      "bubble_cex",
      "cex_fct_adj",
      "bubble_alpha",
      "bubble_bg_alpha",
      "has_sub"
    )
  )

  legend_args = sanitize_legend(legend, legend_args)

  # Legend for grouping variable (by)
  lgby = list(
    legend_args = modifyList(
      legend_args,
      list(x.intersp = 1, y.intersp = 1),
      keep.null = TRUE
    ),
    by_dep = by_dep,
    lgnd_labs = lgnd_labs,
    type = type,
    pch = pch,
    lty = lty,
    lwd = lwd,
    col = col,
    bg = bg,
    gradient = by_continuous,
    cex = lgnd_cex,
    has_sub = has_sub
  )

  # Legend for bubble sizes
  lgbub = list(
    legend_args = modifyList(
      legend_args,
      list(title = cex_dep, ncol = 1),
      keep.null = TRUE
    ),
    lgnd_labs = names(bubble_cex),
    type = type,
    pch = pch,
    lty = lty,
    lwd = lwd,
    col = adjustcolor(par("col"), alpha.f = bubble_alpha),
    bg = adjustcolor(par("col"), alpha.f = bubble_bg_alpha),
    cex = bubble_cex * cex_fct_adj,
    has_sub = has_sub,
    draw = FALSE
  )

  env2env(environment(), settings, c("legend_args", "lgby", "lgbub"))
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
  spec = create_legend_spec(
    legend_args = legend_build$legend_args,
    type = type,
    gradient = gradient,
    has_sub = has_sub,
    new_plot = new_plot,
    dynmar = dynmar
  )

  # Populate flags from build_legend_spec output (which already parsed positioning)
  spec$flags$outer_side = legend_build$outer_side
  spec$flags$outer_end = legend_build$outer_end
  spec$flags$outer_right = legend_build$outer_right
  spec$flags$outer_bottom = legend_build$outer_bottom
  spec$flags$mcol = legend_build$mcol_flag
  spec$flags$user_inset = legend_build$user_inset

  # Run pipeline stages (skip build since build_legend_spec already did that work)
  spec = legend_spec_apply_margins(spec)

  # Measure dimensions with fake legend
  fklgnd_args = prepare_fake_legend_args(
    spec$args,
    spec$flags$gradient,
    spec$flags$outer_end
  )
  spec$dims = do.call("legend", fklgnd_args)

  if (!draw) {
    return(spec$dims)
  }

  spec = legend_spec_layout(spec, draw = draw)

  # Draw wrapped in recordGraphics() to preserve spacing if plot is resized
  recordGraphics(
    legend_spec_draw(spec),
    list = list(spec = spec),
    env = getNamespace("tinyplot")
  )
}


#
## Gradient Legend Rendering -----
#

#' Draw gradient (continuous) legend swatch
#'
#' @description For gradient legends, we draw a custom color swatch using
#'   grDevices::as.raster and add labels, tick marks, and title manually.
#'
#' @param legend_args Legend arguments list
#' @param fklgnd Fake legend object (from drawing with plot=FALSE)
#' @param lmar Legend margins
#' @param outer_side Logical flag for outer side placement
#' @param outer_end Logical flag for outer end placement
#' @param outer_right Logical flag for outer right placement
#' @param outer_bottom Logical flag for outer bottom placement
#' @param user_inset Logical flag indicating user-supplied inset
#'
#' @returns NULL (draws gradient legend as side effect)
#'
#' @keywords internal
draw_gradient_swatch = function(
  legend_args,
  fklgnd,
  lmar,
  outer_side,
  outer_end,
  outer_right,
  outer_bottom,
  user_inset = FALSE
) {
  pal = legend_args[["col"]]
  lgnd_labs = legend_args[["legend"]]
  if (!is.null(legend_args[["horiz"]])) {
    horiz = legend_args[["horiz"]]
  } else {
    horiz = FALSE
  }

  # Create raster color swatch
  if (isTRUE(horiz)) {
    rasterlgd = as.raster(matrix(pal, nrow = 1))
  } else {
    rasterlgd = as.raster(matrix(rev(pal), ncol = 1))
  }

  corners = par("usr")
  rasterbox = rep(NA_real_, 4)

  # Determine positioning flags
  inner = !any(c(outer_side, outer_end))
  inner_right = inner_bottom = FALSE
  if (inner) {
    if (!is.null(legend_args[["x"]]) && grepl("left$|right$", legend_args[["x"]])) {
      inner_right = grepl("right$", legend_args[["x"]])
    }
    if (!is.null(legend_args[["x"]]) && grepl("^bottoml|^top", legend_args[["x"]])) {
      inner_bottom = grepl("^bottom", legend_args[["x"]])
    }
  }

  # Calculate raster box coordinates based on position
  if (inner) {
    fklgnd$rect$h = fklgnd$rect$h - lines_to_user_y(1.5 + 0.4)

    rasterbox[1] = fklgnd$rect$left
    if (isFALSE(inner_right)) {
      rasterbox[1] = rasterbox[1] + lines_to_user_x(0.2)
    }
    rasterbox[2] = fklgnd$rect$top - fklgnd$rect$h - lines_to_user_y(1.5 + 0.2)
    rasterbox[3] = rasterbox[1] + lines_to_user_x(1.25)
    rasterbox[4] = rasterbox[2] + fklgnd$rect$h

  } else if (outer_side) {
    rb1_adj = lines_to_user_x(lmar[1] + 0.2)
    rb3_adj = lines_to_user_x(1.25)
    rb2_adj = (corners[4] - corners[3] - lines_to_user_y(5 + 1 + 2.5)) / 2
    # Override if top or bottom
    if (!is.null(legend_args[["x"]])) {
      if (grepl("^bottom", legend_args[["x"]])) {
        rb2_adj = corners[3]
      }
      if (grepl("^top", legend_args[["x"]])) {
        rb2_adj = corners[4] - lines_to_user_y(5 + 1 + 2.5)
      }
    }
    if (user_inset) {
      rb2_adj = rb2_adj + legend_args[["inset"]][2] + 0.05
    }
    rb4_adj = lines_to_user_y(5 + 1)

    if (outer_right) {
      rasterbox[1] = corners[2] + rb1_adj
      if (user_inset) {
        rasterbox[1] = rasterbox[1] - (corners[2] - legend_args[["inset"]][1]) / 2
      }
      rasterbox[2] = rb2_adj
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] + rb4_adj
    } else {
      rb1_adj = rb1_adj + lines_to_user_x(par("mar")[2] + 1)
      rasterbox[1] = corners[1] - rb1_adj
      rasterbox[2] = rb2_adj
      rasterbox[3] = rasterbox[1] - rb3_adj
      rasterbox[4] = rasterbox[2] + rb4_adj
    }

  } else if (outer_end) {
    rb1_adj = (corners[2] - corners[1] - lines_to_user_x(5 + 1)) / 2
    rb3_adj = lines_to_user_x(5 + 1)
    rb2_adj = lines_to_user_y(lmar[1])
    rb4_adj = lines_to_user_y(1.25)

    if (outer_bottom) {
      rb2_adj = rb2_adj + lines_to_user_y(par("mar")[2])
      rasterbox[1] = rb1_adj
      rasterbox[2] = corners[3] - rb2_adj
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] - rb4_adj
    } else {
      rb2_adj = rb2_adj + lines_to_user_y(1.25 + 1)
      rasterbox[1] = rb1_adj
      rasterbox[2] = corners[4] + rb2_adj
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] - rb4_adj
    }
  }

  # Draw the gradient swatch
  rasterImage(
    rasterlgd,
    rasterbox[1], #x1
    rasterbox[2], #y1
    rasterbox[3], #x2
    rasterbox[4], #y2
    xpd = NA
  )

  # Add labels, tick marks, and title
  if (isFALSE(horiz)) {
    draw_gradient_labels_vertical(rasterbox, lgnd_labs, legend_args, inner, outer_right)
  } else {
    draw_gradient_labels_horizontal(rasterbox, lgnd_labs, legend_args)
  }
}


#
## Multi-Legend Rendering -----
#

#' Draw multiple legends with automatic positioning
#'
#' @description Handles multiple legends (e.g., color grouping + bubble size) by:
#'   1. Extracting dimensions from fake legends
#'   2. Calculating sub-positioning based on dimensions
#'   3. Drawing legends in ascending order of width (widest last)
#'
#' @md
#' @param legend_list A list of legend arguments, where each element is itself a
#'   list of arguments that can be passed on to [draw_legend]. Legends will be
#'   drawn vertically (top to bottom) in the order that they are provided. Note
#'   that we currently only support 2 legends, i.e. the top-level list has a
#'   maximum length of 2.
#' @param position String indicating the base keyword position for the
#'   multi-legend. Currently only `"right!"` and `"left!"` are supported.
#'
#' @returns No return value, called for side effect of drawing multiple legends.
#'
#' @seealso [draw_legend]
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' oldmar = par("mar")
#'
#' # Multi-legend example (color + bubble)
#'
#' l1 = list(
#'   lgnd_labs = c("Red", "Blue", "Green"),
#'   legend_args = list(title = "Colors"),
#'   pch = 16,
#'   col = c("red", "blue", "green"),
#'   type = "p"
#' )
#'
#' l2 = list(
#'   lgnd_labs = c("Tiny", "Small", "Medium", "Large", "Huge"),
#'   legend_args = list(title = "Size"),
#'   pch = 16,
#'   col = "black",
#'   cex = seq(0.5, 2.5, length.out = 5),
#'   type = "p"
#' )
#'
#' # Draw together
#' draw_multi_legend(list(l1, l2), position = "right!")
#'
#' par(mar = oldmar)
#' }
#'
#' @keywords internal
draw_multi_legend = function(
    legend_list,
    position = "right!"
) {

  # Validate inputs
  if (!is.list(legend_list) || length(legend_list) != 2) {
    stop("Currently only 2 legends are supported in multi-legend mode")
  }

  # Currently only support right!/left! positioning
  if (!grepl("right!$|left!$", position)) {
    warning(
      '\nMulti-legends currently only work with "right!" or "left!" keyword positioning.\n',
      'Reverting to "right!" default\n'
    )
    position = "right!"
  }

  # Determine sub-positions based on main position
  if (grepl("right!$", position)) {
    sub_positions = c("bottomright!", "topright!")
  } else if (grepl("left!$", position)) {
    sub_positions = c("bottomleft!", "topleft!")
  }

  # Assign positions of individual legends
  for (ll in seq_along(legend_list)) {
    legend_list[[ll]][["legend"]] = sub_positions[ll]
    legend_list[[ll]][["legend_args"]][["x"]] = NULL
  }

  #
  ## Step 1: Extract legend dimensions (by drawing fake legends)
  #

  legend_dims = vector("list", length(legend_list))
  for (ll in seq_along(legend_list)) {
    legend_ll = legend_list[[ll]]
    legend_ll$new_plot = ll == 1  # Only draw new plot for first legend
    legend_ll$draw = FALSE
    legend_dims[[ll]] = do.call(draw_legend, legend_ll)
  }

  #
  ## Step 2: Calculate sub-positioning based on dimensions
  #

  # Extract dimensions
  lwidths = sapply(legend_dims, function(x) x$rect$w)
  lheights = sapply(legend_dims, function(x) x$rect$h)
  # For inset adjustment, default to 0.5 unless one or more of the two legends
  # is bigger than half the plot height.
  linset = if (any(lheights > 0.5)) lheights[2] / sum(lheights) else 0.5

  #
  ## Step 3: Reposition (via adjusted inset arg) and draw legends
  #

  # Note: we draw the legends in ascending order of width (i.e., widest legend
  #   last) in order to correctly set the overall plot dimensions.
  width_order = order(lwidths)

  # Quick idx for original order (needed for vertical legend placement)
  for (i in seq_along(legend_list)) legend_list[[i]]$idx = i

  for (o in seq_along(width_order)) {
    io = width_order[o]
    legend_o = legend_list[[io]]
    legend_o$new_plot = FALSE
    legend_o$draw = TRUE
    legend_o$legend_args$inset = c(0, 0)
    legend_o$legend_args$inset[1] = if (o == 1) -abs(diff(lwidths)) / 2 else 0
    legend_o$legend_args$inset[2] = if (legend_o$idx == 1) linset + 0.01 else 1 - linset + 0.01
    legend_o$idx = NULL
    do.call(draw_legend, legend_o)
  }

  invisible(NULL)
}
