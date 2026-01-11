#
## orchestration function -----
#

by_aesthetics = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "datapoints", "by", "type", "null_by", "pch", "bg", "lty", "lwd",
      "bubble", "cex", "alpha", "col", "fill", "ribbon.alpha"
    )
  )

  # Detect grouping characteristics
  by_ordered = FALSE
  by_continuous = !null_by && inherits(datapoints$by, c("numeric", "integer"))
  if (isTRUE(by_continuous) && type %in% c("l", "b", "o", "ribbon", "polygon", "polypath", "boxplot")) {
    warning("\nContinuous legends not supported for this plot type. Reverting to discrete legend.")
    by_continuous = FALSE
  } else if (!null_by) {
    by_ordered = is.ordered(by)
  }

  ngrps = if (null_by) {
    1L
  } else if (is.factor(by)) {
    nlevels(by)
  } else if (by_continuous) {
    100L
  } else {
    length(unique(by))
  }

  pch = by_pch(ngrps = ngrps, type = type, pch = pch)
  lty = by_lty(ngrps = ngrps, type = type, lty = lty)
  lwd = by_lwd(ngrps = ngrps, type = type, lwd = lwd)
  cex = by_cex(ngrps = ngrps, type = type, bubble = bubble, cex = cex)

  col = by_col(
    col = col,
    palette = settings$palette, # must use unevaluated palette
    alpha = alpha,
    by_ordered = by_ordered,
    by_continuous = by_continuous,
    ngrps = ngrps,
    adjustcolor = adjustcolor
  )

  bg = by_bg(
    bg = bg,
    fill = fill,
    col = col,
    palette = settings$palette, # must use unevaluated palette
    alpha = alpha,
    by_ordered = by_ordered,
    by_continuous = by_continuous,
    ngrps = ngrps,
    type = type,
    by = by,
    ribbon.alpha = ribbon.alpha,
    adjustcolor = adjustcolor
  )

  # update settings
  env2env(
    environment(),
    settings,
    c("by_continuous", "by_ordered", "ngrps", "pch", "lty", "lwd", "cex", "col", "bg")
  )
}


#
## helper functions -----
#


apply_alpha = function(cols, alpha, adjustcolor) {
  if (is.null(cols) || is.null(alpha) || identical(alpha, 0)) {
    return(cols)
  }
  adjustcolor(cols, alpha.f = alpha)
}

is_by_keyword = function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && identical(x, "by")
}

warn_recycle_colors = function(ncols, ngrps) {
  warning(
    "\nFewer colours (", ncols, ") provided than there are groups (",
    ngrps, "). Recycling to make up the shortfall."
  )
}

expand_colors_to_ngrps = function(values, ngrps, gradient) {
  if (length(values) == 1) {
    return(rep(values, ngrps))
  }
  if (length(values) >= ngrps) {
    return(values[seq_len(ngrps)])
  }
  if (gradient) {
    return(colorRampPalette(colors = values, alpha = TRUE)(ngrps))
  }
  warn_recycle_colors(length(values), ngrps)
  rep_len(values, ngrps)
}

assert_len_1_or_ngrps = function(x, ngrps, name, allow_character = FALSE) {
  types = if (allow_character) "numeric or character" else "numeric"
  valid_type = is.numeric(x) || (allow_character && is.character(x))
  valid = is.atomic(x) && is.vector(x) && valid_type && (length(x) == 1 || length(x) == ngrps)
  if (!valid) {
    stop(sprintf("`%s` must be `NULL`, or a %s vector of length 1 or %s.", name, types, ngrps), call. = FALSE)
  }
}

match_palette_name = function(name, candidates) {
  normalize = function(x) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", x))
  charmatch(normalize(name), normalize(candidates))
}

resolve_manual_colors = function(col, ngrps, gradient, ordered, alpha, adjustcolor) {
  # Returns NULL if not manual colors, otherwise returns the resolved colors
  if (is.null(col) || !is.atomic(col) || !is.vector(col)) {
    return(NULL)
  }

  cols = col
  if (length(cols) == 1) {
    cols = rep(cols, ngrps)
  } else if (length(cols) < ngrps) {
    cols = expand_colors_to_ngrps(cols, ngrps, gradient)
  }

  # Map numeric indices to palette colors (unless ordered)
  if (!ordered && is.numeric(cols)) {
    base_pal = grDevices::palette()
    cols = if (ngrps <= length(base_pal)) {
      base_pal[cols]
    } else {
      grDevices::hcl.colors(max(cols))[cols]
    }
  }

  if (gradient) cols = rev(cols)
  apply_alpha(cols, alpha, adjustcolor)
}

resolve_palette_colors = function(palette, theme_palette, ngrps, ordered, gradient, alpha, adjustcolor) {
  palette_choice = palette

  # Pick theme palette if no explicit palette provided
  if (is.null(palette_choice) && !is.null(theme_palette)) {
    palette_choice = theme_palette
    if (length(theme_palette) == 1) {
      # Check if theme palette needs to switch to sequential
      use_sequential = FALSE
      idx = match_palette_name(theme_palette, palette.pals())
      if (!is.na(idx) && idx >= 1L) {
        max_colors = length(palette.colors(palette = palette.pals()[idx]))
        use_sequential = ngrps >= max_colors || ordered
      } else {
        idx = match_palette_name(theme_palette, hcl.pals())
        use_sequential = !is.na(idx) && idx >= 1L && gradient
      }
      if (use_sequential) {
        palette_choice = get_tpar("palette.sequential", default = NULL)
      }
    }
  }

  if (is.null(palette_choice)) {
    # Default palette selection (alpha applied at end)
    base_pal = grDevices::palette()
    if (ngrps <= length(base_pal) && !ordered && !gradient) {
      cols = base_pal[seq_len(ngrps)]
    } else if (ngrps <= 8 && !ordered) {
      cols = grDevices::palette.colors(n = ngrps, palette = "R4")
    } else if (!gradient && !ordered) {
      cols = grDevices::hcl.colors(n = ngrps, palette = "Viridis")
    } else {
      # Restricted viridis for gradient/ordered (excludes extreme ends)
      cols = colorRampPalette(
        grDevices::hcl.colors(n = 100, palette = "Viridis")[11:90],
        alpha = TRUE
      )(ngrps)
    }
    cols = apply_alpha(cols, alpha, adjustcolor)
  } else {
    cols = resolve_palette_spec(
      palette = palette_choice,
      ngrps = ngrps,
      gradient = gradient,
      ordered = ordered,
      alpha = alpha,
      adjustcolor = adjustcolor
    )
  }

  if (gradient || ordered) cols = rev(cols)
  cols
}

resolve_palette_spec = function(palette, ngrps, gradient, ordered, alpha, adjustcolor) {
  cols = NULL
  palette_fun = NULL
  args = NULL

  # Determine colors or palette function based on spec type
  # Note: alpha is NOT passed to palette functions; it's applied uniformly at the end
  if (is.character(palette) && length(palette) > 1) {
    # Direct color vector
    cols = palette
  } else if (is.character(palette)) {
    # Named palette string
    palette_fun = resolve_palette_function(palette, gradient = gradient, alpha = NULL, n = ngrps)
    args = list(n = ngrps, palette = palette, alpha = NULL)
  } else if (inherits(palette, c("call", "name"))) {
    # Expression or symbol
    if (inherits(palette, "name")) {
      eval_palette = tryCatch(eval(palette), error = function(e) NULL)
      if (is.character(eval_palette)) {
        cols = eval_palette
      }
    }
    if (is.null(cols)) {
      args = as.list(palette)
      palette_fun = paste(args[[1]])
      args[[1]] = NULL
      if (palette_fun %in% c("c", "list")) {
        cols = unlist(args, recursive = TRUE, use.names = FALSE)
      } else {
        args[["n"]] = ngrps
        if (any(names(args) == "")) args[which(names(args) == "")] = NULL
      }
    }
  } else if (inherits(palette, "function")) {
    palette_fun = palette
    args = list()
  } else {
    stop(
      "\nInvalid palette argument. Must be a recognized keyword, or a ",
      "palette-generating function with named arguments.\n"
    )
  }

  # Generate colors from palette function if needed
  if (is.null(cols) && !is.null(palette_fun)) {
    cols = tryCatch(
      do.call(palette_fun, args),
      error = function(e) do.call(eval(palette), args)
    )
  }

  # Uniform post-processing
  cols = expand_colors_to_ngrps(cols, ngrps, gradient)
  apply_alpha(cols, alpha, adjustcolor)
}

# Resolve a palette string to its function, handling fuzzy matching and recycling
resolve_palette_function = function(pal, gradient = FALSE, alpha = NULL, n = NULL) {
  # Try palette.pals() first (discrete palettes)
  discrete_pals = palette.pals()
  idx = match_palette_name(pal, discrete_pals)

  if (!is.na(idx)) {
    if (idx < 1L) stop("'palette' is ambiguous")
    matched_name = discrete_pals[idx]
    max_colors = length(palette.colors(palette = matched_name))

    if (gradient) {
      return(function(n, palette, alpha) {
        colorRampPalette(palette.colors(palette = matched_name, alpha = alpha))(n)
      })
    }
    if (!is.null(n) && n >= max_colors) {
      warn_recycle_colors(max_colors, n)
      return(function(n, palette, alpha) {
        palette.colors(n = n, palette = matched_name, alpha = alpha, recycle = TRUE)
      })
    }
    return(palette.colors)
  }

  # Try hcl.pals() (continuous palettes)
  hcl_pals = hcl.pals()
  idx = match_palette_name(pal, hcl_pals)

  if (!is.na(idx)) {
    if (idx < 1L) stop("'palette' is ambiguous")
    return(hcl.colors)
  }

  stop(
    "\nPalette string not recognized. Must be a value produced by either ",
    "`palette.pals()` or `hcl.pals()`.\n",
    call. = FALSE
  )
}


#
## subsidiary functions -----
#

by_col = function(col, palette, alpha, by_ordered, by_continuous, ngrps, adjustcolor) {
  ordered = if (is.null(by_ordered)) FALSE else by_ordered
  gradient = if (is.null(by_continuous)) FALSE else by_continuous
  assert_logical(ordered)
  assert_logical(gradient)

  if (is.null(alpha)) alpha = 1
  if (gradient) ngrps = 100L

  if (is_by_keyword(col)) col = NULL

  cols = resolve_manual_colors(col, ngrps, gradient, ordered, alpha, adjustcolor)
  if (!is.null(cols)) return(cols)

  pal_theme = get_tpar("palette.qualitative", default = NULL)
  cols = resolve_palette_colors(
    palette = palette,
    theme_palette = pal_theme,
    ngrps = ngrps,
    ordered = ordered,
    gradient = gradient,
    alpha = alpha,
    adjustcolor = adjustcolor
  )

  cols
}


by_bg = function(bg, fill, col, palette, alpha, by_ordered, by_continuous, ngrps, type, by, ribbon.alpha, adjustcolor) {
  if (is.null(bg) && !is.null(fill)) bg = fill
  if (!is.null(bg) && length(bg) == 1 && is.numeric(bg) && bg >= 0 && bg <= 1) {
    alpha = bg
    bg = "by"
  }
  if (!is.null(bg) && length(bg) == 1 && is_by_keyword(bg)) {
    bg = resolve_palette_colors(
      palette = palette,
      theme_palette = get_tpar("palette.qualitative", default = NULL),
      ngrps = ngrps,
      ordered = if (is.null(by_ordered)) FALSE else by_ordered,
      gradient = if (is.null(by_continuous)) FALSE else by_continuous,
      alpha = if (is.null(alpha)) 1 else alpha,
      adjustcolor = adjustcolor
    )
  } else if (length(bg) != ngrps) {
    bg = rep(bg, ngrps)
  }
  if (type == "ribbon" || (type == "boxplot" && !is.null(by))) {
    if (!is.null(bg)) {
      bg = adjustcolor(bg, ribbon.alpha)
    } else if (!is.null(col)) {
      bg = adjustcolor(col, ribbon.alpha)
    }
  }

  bg
}


by_pch = function(ngrps, type, pch = NULL) {
  no_pch = FALSE
  if (identical(type, "text")) {
    pch = rep(15, ngrps)
  } else if (!type %in% c("p", "b", "o", "pointrange", "errorbar", "boxplot", "qq")) {
    no_pch = TRUE
    pch = NULL

    # special "by" convenience keyword
  } else if (is_by_keyword(pch)) {
    no_pch = TRUE # skip checks below
    pch = 1:ngrps + par("pch") - 1
    # correctly recycle if over max pch type
    if (max(pch) > 25L) {
      pch_below = pch[pch <= 25L]
      pch_above = pch[pch > 25L]
      pch_above = rep_len(0:25, length(pch_above))
      pch = c(pch_below, pch_above)
    }

    # return NULL if not a valid point type
  } else if (is.null(pch)) {
    pch = par("pch")
  }

  if (!no_pch) {
    assert_len_1_or_ngrps(pch, ngrps, "pch", allow_character = TRUE)
    if (length(pch) == 1) pch = rep(pch, ngrps)
  }

  return(pch)
}


by_lty = function(ngrps, type, lty = NULL) {
  # We only care about line types, otherwise return NULL
  if (!type %in% c("l", "b", "o", "c", "h", "s", "S", "ribbon", "barplot", "boxplot", "rect", "segments", "qq", "abline", "hline", "vline")) {
    lty = NULL

    # special "by" convenience keyword
  } else if (is_by_keyword(lty)) {
    lty_dict = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    par_lty = par("lty")

    if (!par_lty %in% lty_dict) {
      warning(
        "\nBespoke lty specifications (i.e., using string combinations) are not ",
        "currently supported alongside the lty='by' keyword argument. ",
        "Defaulting to 1 and looping from there.\n"
      )
      par_lty = 1
    } else {
      par_lty = which(par_lty == lty_dict)
    }
    lty = 1:ngrps + par_lty - 1
    # correctly recycle if over max lty type
    if (max(lty) > 6L) {
      lty_below = lty[lty <= 6L]
      lty_above = lty[lty > 6L]
      lty_above = rep_len(1:6, length(lty_above))
      lty = c(lty_below, lty_above)
    }

    # NULL -> solid (or default) line
  } else if (is.null(lty)) {
    if (!identical(type, "boxplot")) {
      lty = rep(par("lty"), ngrps)
    }

    # atomic vector: sanity check length
  } else if (is.atomic(lty) && is.vector(lty)) {
    assert_len_1_or_ngrps(lty, ngrps, "lty")
    if (length(lty) == 1) lty = rep(lty, ngrps)
  }

  lty
}


by_lwd = function(ngrps, type, lwd = NULL) {
  lwd_base = par("lwd")
  lwd_floor = lwd_base / min(5, max((ngrps - 1), 1))
  lwd_ceiling = lwd_base * min(5, ngrps)

  no_lwd = FALSE
  # special "by" convenience keyword
  if (is_by_keyword(lwd)) {
    no_lwd = TRUE # skip checks below
    lwd = seq(lwd_floor, lwd_ceiling, length.out = ngrps)
  } else if (is.null(lwd)) {
    no_lwd = TRUE
    lwd = NULL
  }

  if (!no_lwd) {
    assert_len_1_or_ngrps(lwd, ngrps, "lwd")
    if (length(lwd) == 1) lwd = rep(lwd, ngrps)
  }

  return(lwd)
}


by_cex = function(ngrps, type, bubble = FALSE, cex = NULL) {
  no_cex = FALSE
  # special "by" convenience keyword
  if (is_by_keyword(cex)) {
    no_cex = TRUE # skip checks below
    cex = rescale_num(c(1:ngrps), to = c(1, 2.5))
  } else if (is.null(cex)) {
    no_cex = TRUE
    # cex = NULL
    # can't leave cex as NULL otherwise JIT cex_fct_adj adjustment in
    # draw_legend() won't work later
    cex = 1
    cex = rep(cex, ngrps)
  }

  # placehodler
  if (bubble) no_cex = TRUE

  if (!no_cex) {
    assert_len_1_or_ngrps(cex, ngrps, "cex")
    if (length(cex) == 1) cex = rep(cex, ngrps)
  }

  return(cex)
}
