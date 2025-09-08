by_col = function(ngrps = 1L, col = NULL, palette = NULL, gradient = NULL, ordered = NULL, alpha = NULL) {
  if (is.null(alpha)) alpha = 1
  if (is.null(ordered)) ordered = FALSE
  if (is.null(gradient)) gradient = FALSE
  assert_logical(ordered)
  assert_logical(gradient)
  if (gradient) {
    ngrps = 100L
  }
  
  # pal_qual = get_tpar("palette.qualitative", default = NULL)
  pal_theme = get_tpar("palette.qualitative", default = NULL)
  theme_flag = !is.null(pal_theme)

  # palette = substitute(palette, env = parent.env(environment()))

  # special "by" convenience keyword (will treat as NULL & handle grouping below)
  if (!anyNA(col) && !is.null(col) && length(col) == 1 && col == "by") col = NULL

  #
  ## Base case: If no color or palette provided, pass colors as a sequence of
  ## numbers (will inherit from / cycle over the user's default palette)
  
  if (is.null(col) && (is.null(palette) && !theme_flag)) {
    if (ngrps <= length(palette())  && !ordered) {
      col = palette()[seq_len(ngrps)]
      if (alpha) col = adjustcolor(col, alpha.f = alpha)
    } else {
      # fallback to restricted viridis palette
      col = colorRampPalette(
        hcl.colors(n = 100, palette = "Viridis", alpha = alpha)[(100 * 0.1 + 1):(100 * 0.9)],
        alpha = TRUE
      )(ngrps)
    }
    if (gradient || ordered) col = rev(col)
    return(col)
  }

  #
  ## Next simplest case: No palette, but color(s) provided directly. We do
  ## some simple sanity checks, apply alpha transparency and return as-is.

  if (is.atomic(col) && is.vector(col)) {
    if (length(col) == 1) {
      col = rep(col, ngrps)
      if (alpha) col = adjustcolor(col, alpha.f = alpha)
      return(col)
    } else if (length(col) < ngrps) {
      # if (!gradient) {
      #   stop(sprintf("`col` must be of length 1, or greater than or equal to %s.", ngrps), call. = FALSE)
      # } else {
      #   # interpolate gradient colors
      #   col = colorRampPalette(colors = col, alpha = TRUE)(ngrps)
      # }
      # if manual colours < ngrps, either (1) interpolate for gradient
      # colors, or (2) recycle for discrete colours
      if (gradient) {
        col = colorRampPalette(colors = col, alpha = TRUE)(ngrps)
      } else {
        ncolsstr = paste0("(", length(col), ")")
        ngrpsstr = paste0("(", ngrps, ")")
        warning(
          "\nFewer colours ", ncolsstr, " provided than than there are groups ",
          ngrpsstr, ". Recycling to make up the shortfall."
        )
        col = rep_len(col, ngrps)
      }
  
    }
    if (gradient) {
      col = rev(col)
    } else if (!ordered && is.numeric(col)) {
      # col = palette()[col]
      if (ngrps <= length(palette())) {
        col = palette()[col]
        # if (alpha) col = adjustcolor(col, alpha.f = alpha)
      } else {
        col = hcl.colors(max(col), alpha = alpha)[col]
      }
    }
    if (anyNA(col) || is.character(col)) {
      if (alpha) col = adjustcolor(col, alpha.f = alpha)
      return(col)
    }
  }
  
  
  #
  ## Theme case: No palette provided, but fallback to tinytheme palette

  # we need to fix palette string, determine if in palette.pals() and then
  # determine no. of groups, before kicking over to sequential
  if (is.null(palette) && theme_flag) {
    if (length(pal_theme) == 1) {
      qual_match = match_pal(pal_theme, palette.pals())
      if (!is.na(qual_match)) {
        if (ngrps >= get_pal_lens(pal_theme) || ordered) {
          pal_theme = get_tpar("palette.sequential", default = NULL)
        }
      # mostly a catch for x-gradient fills, e.g. type_ridge
      } else if (gradient && !is.na(match_pal(pal_theme, hcl.pals()))) {
          pal_theme = get_tpar("palette.sequential", default = NULL)
      }
    }
    if (length(pal_theme) == 1) {
      palette_fun = gen_pal_fun(pal = pal_theme, gradient = gradient, alpha = alpha)
      args = list(n = ngrps, palette = pal_theme, alpha = alpha)
    }
    palette = pal_theme
  }
  
  if (is.null(palette)) {
    if (ngrps <= length(palette()) && !ordered && !gradient) {
      palette_fun = function(alpha) adjustcolor(palette(), alpha) # must be function to avoid arg ambiguity
      args = list(alpha = alpha)
    } else {
      if (ngrps <= 8 && !ordered) { # ngrps < 100 so we know gradient is FALSE too
        palette = "R4"
        palette_fun = palette.colors
      } else {
        palette = "Viridis"
        if (!gradient && !ordered) {
          palette_fun = hcl.colors
        } else {
          palette_fun_gradient = function(n, palette, from = 0.1, to = 0.9, alpha = 1) {
            colorRampPalette(
              hcl.colors(n = 100, palette = palette, alpha = alpha)[(100 * from + 1):(100 * to)],
              alpha = TRUE
            )(n)
          }
          palette_fun = palette_fun_gradient
        }
      }
      args = list(n = ngrps, palette = palette, alpha = alpha)
    }
  } else {
    if (is.character(palette)) {
      # special case: if vector of character strings, we assume that the user
      # must have passed a vector of colours (e.g., c("red", "blue")) rather
      # than a known/named colour palette (e.g. "Harmonic")
      if (length(palette) > 1) {
        palette_fun = "c"
        if (!is.null(alpha)) palette = adjustcolor(palette, alpha.f = alpha)
        args = as.list(palette)
        if (length(args) < ngrps && length(args) != 1) {
          # if manual colours < ngrps, either (1) interpolate for gradient
          # colors, or (2) recycle for discrete colours
          if (gradient) {
            args = list(colorRampPalette(args, alpha = TRUE)(ngrps))
          } else {
            ncolsstr = paste0("(", length(args), ")")
            ngrpsstr = paste0("(", ngrps, ")")
            warning(
              "\nFewer colours ", ncolsstr, " provided than than there are groups ",
              ngrpsstr, ". Recycling to make up the shortfall."
            )
            args = rep_len(args, ngrps)
          }
        }
      } else {
        palette_fun = gen_pal_fun(palette, gradient = gradient, alpha = alpha, n = ngrps)
        args = list(n = ngrps, palette = palette, alpha = alpha)
      }
    } else if (inherits(palette, c("call", "name"))) {
      # catch for when using passes palette as named object (e.g,
      # pal26 = palette.colors("Alphabet"))
      if (inherits(palette, "name") && is.character(eval(palette))) {
        args = as.list(eval(palette))
        palette_fun = "c"
      } else {
        args = as.list(palette)
        palette_fun = paste(args[[1]])
        args[[1]] = NULL
      }
      # catch for direct vector or list
      if (palette_fun %in% c("c", "list")) {
        if (palette_fun == "list") palette_fun = "c"
        if (!is.null(alpha)) args = lapply(args, function(a) adjustcolor(a, alpha.f = alpha))
        if (length(args) < ngrps && length(args) != 1) {
          # if manual colours < ngrps, either (1) interpolate for gradient
          # colors, or (2) recycle for discrete colours
          if (gradient) {
            args = list(colorRampPalette(args, alpha = TRUE)(ngrps))
          } else {
            ncolsstr = paste0("(", length(args), ")")
            ngrpsstr = paste0("(", ngrps, ")")
            warning(
              "\nFewer colours ", ncolsstr, " provided than than there are groups ",
              ngrpsstr, ". Recycling to make up the shortfall."
            )
            args = rep_len(args, ngrps)
          }
        }
      } else {
        args[["n"]] = ngrps
        # remove unnamed arguments to prevent unintentional argument sliding
        if (any(names(args) == "")) args[[which(names(args) == "")]] = NULL
      }
    } else if (inherits(palette, "function")) {
      args = list()
      palette_fun = palette
    } else {
      stop(
        "\nInvalid palette argument. Must be a recognized keyword, or a ",
        "palette-generating function with named arguments.\n"
      )
    }
  }

  cols = tryCatch(
    do.call(palette_fun, args),
    error = function(e) do.call(eval(palette), args) # catch for bespoke palette generating funcs
  )

  if (length(cols) > ngrps) cols = cols[1:ngrps]

  # For gradient and ordered colors, we'll run high to low
  if (gradient || ordered) cols = rev(cols)

  return(cols)
}

# Some utility functions for palette matching, etc.

match_pal = function(pal, pals) {
  fx = function(x) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", x))
  charmatch(fx(pal), fx(pals))
}

get_pal_lens = function(pal) {
  pal_lens = c(
    R3 = 8L, R4 = 8L, ggplot2 = 8L, `Okabe-Ito` = 9L, Accent = 8L,
    `Dark 2` = 8L, Paired = 12L, `Pastel 1` = 9L, `Pastel 2` = 8L, 
    `Set 1` = 9L, `Set 2` = 8L, `Set 3` = 12L, `Tableau 10` = 10L, 
    `Classic Tableau` = 10L, `Polychrome 36` = 36L, Alphabet = 26L
  )
  pal_lens[pal]
}

# take a character string, match to either palette.pals() pr hcl.pals(), and
# generate the corresponding function factor with alpha transparency
gen_pal_fun = function(pal, gradient = FALSE, alpha = NULL, n = NULL) {
  pal_match = match_pal(pal, palette.pals())
  if (!is.na(pal_match)) {
    if (pal_match < 1L) stop("'palette' is ambiguous")
    pal_fun = palette.colors
    if (!is.null(n) && n >= get_pal_lens(pal_match)) {
      warning(
        "\nFewer colours ", get_pal_lens(pal_match), " provided than than there are groups ",
        n, ". Recycling to make up the shortfall."
      )
      pal_fun = function(n, palette, alpha) palette.colors(n = n, palette = pal, alpha = alpha, recycle = TRUE)
    }
    if (gradient) {
      pal_fun = function(n, palette, alpha) colorRampPalette(palette.colors(palette = pal, alpha = alpha))(n)
    }
  } else {
    pal_match = match_pal(pal, hcl.pals())
    if (!is.na(pal_match)) {
      if (pal_match < 1L) stop("'palette' is ambiguous")
      pal_fun = hcl.colors
    } else {
      stop(
        "\nPalette string not recogized. Must be a value produced by either",
        "`palette.pals()` or `hcl.pals()`.\n",
        call. = FALSE
      )
    }
  }
  return(pal_fun)
}


by_pch = function(ngrps, type, pch = NULL) {
  no_pch = FALSE
  if (identical(type, "text")) {
    pch <- rep(15, ngrps)
  } else if (!type %in% c("p", "b", "o", "pointrange", "errorbar", "boxplot", "qq")) {
    no_pch = TRUE
    pch = NULL

    # special "by" convenience keyword
  } else if (!is.null(pch) && length(pch) == 1 && pch == "by") {
    no_pch = TRUE # skip checks below
    pch = 1:ngrps + par("pch") - 1
    # correctly recycle if over max pch type
    pch_ceiling = 25 # see ?pch
    if (max(pch) > pch_ceiling) {
      pch_below = pch[pch <= pch_ceiling]
      pch_above = pch[pch > pch_ceiling]
      pch_above = rep_len(0:pch_ceiling, length(pch_above))
      pch = c(pch_below, pch_above)
    }

    # return NULL if not a valid point type
  } else if (is.null(pch)) {
    pch = par("pch")
  }

  if (!no_pch) {
    if (!is.atomic(pch) || !is.vector(pch) || !(is.numeric(pch) || is.character(pch)) || (length(pch) != 1 && length(pch) != ngrps)) {
    # if (!is.atomic(pch) || !is.vector(pch) || !is.numeric(pch) || (length(pch) != 1 && length(pch) != ngrps)) {
      stop(sprintf("`pch` must be `NULL`, or a numeric or character vector of length 1 or %s.", ngrps), call. = FALSE)
    }

    if (length(pch) == 1) {
      pch = rep(pch, ngrps)
    }
  }

  return(pch)
}


by_lty = function(ngrps, type, lty = NULL) {
  # We only care about line types, otherwise return NULL
  if (!type %in% c("l", "b", "o", "c", "h", "s", "S", "ribbon", "barplot", "boxplot", "rect", "segments", "qq", "abline", "hline", "vline")) {
    out = NULL

    # special "by" convenience keyword
  } else if (!is.null(lty) && length(lty) == 1 && lty == "by") {
    lty_dict = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    par_lty = par("lty")

    if (!par_lty %in% lty_dict) {
      warning(
        "\nBesoke lty specifications (i.e., using string combinations) are not",
        "currently supported alongside the lty='by' keyword argument.",
        "Defaulting to 1 and looping from there.\n"
      )
      par_lty = 1
    } else {
      par_lty = which(par_lty == lty_dict)
    }
    out = 1:ngrps + par_lty - 1
    # correctly recycle if over max pch type
    lty_ceiling = 6 # see ?pch
    if (max(out) > lty_ceiling) {
      lty_below = out[out <= lty_ceiling]
      lty_above = out[out > lty_ceiling]
      lty_above = rep_len(1:lty_ceiling, length(lty_above))
      out = c(lty_below, lty_above)
    }

    # NULL -> solid (or default) line
  } else if (is.null(lty)) {
    if (identical(type, "boxplot")) {
      out = NULL
    } else {
      out = rep(par("lty"), ngrps)
    }

    # atomic vector: sanity check length
  } else if (is.atomic(lty) && is.vector(lty)) {
    if (length(lty) == 1) {
      out = rep(lty, ngrps)
    } else {
      if (length(lty) != ngrps) {
        stop(sprintf("`lty` must be `NULL` or a numeric vector of length 1 or %s.", ngrps), call. = FALSE)
      }
      out = lty
    }
  }

  return(out)
}


by_lwd = function(ngrps, type, lwd = NULL) {
  lwd_base = par("lwd")
  lwd_floor = lwd_base / min(5, max((ngrps - 1), 1))
  lwd_ceiling = lwd_base * min(5, ngrps)

  no_lwd = FALSE
  # special "by" convenience keyword
  if (!is.null(lwd) && length(lwd) == 1 && lwd == "by") {
    no_lwd = TRUE # skip checks below
    lwd = seq(lwd_floor, lwd_ceiling, length.out = ngrps)
  } else if (is.null(lwd)) {
    no_lwd = TRUE
    lwd = NULL
  }

  if (!no_lwd) {
    if (!is.atomic(lwd) || !is.vector(lwd) || !is.numeric(lwd) || (length(lwd) != 1 && length(lwd) != ngrps)) {
      stop(sprintf("`lwd` must be `NULL` or a numeric vector of length 1 or %s.", ngrps), call. = FALSE)
    }
    if (length(lwd) == 1) {
      lwd = rep(lwd, ngrps)
    }
  }

  return(lwd)
}


by_cex = function(ngrps, type, bubble = FALSE, cex = NULL) {

  no_cex = FALSE
  # special "by" convenience keyword
  if (!is.null(cex) && length(cex) == 1 && cex == "by") {
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
    if (!is.atomic(cex) || !is.vector(cex) || !is.numeric(cex) || (length(cex) != 1 && length(cex) != ngrps)) {
      stop(sprintf("`cex` must either be `NULL`, or a numeric vector of length 1 or %s (no. of groups).", ngrps), call. = FALSE)
    }
    if (length(cex) == 1) {
      cex = rep(cex, ngrps)
    }
  }

  return(cex)
}



by_bg = function(
    adjustcolor,
    alpha,
    bg,
    by,
    by_continuous,
    by_ordered,
    col,
    fill,
    ngrps,
    palette,
    ribbon.alpha,
    type) {
  if (is.null(bg) && !is.null(fill)) bg = fill
  if (!is.null(bg) && length(bg) == 1 && is.numeric(bg) && bg >= 0 && bg <= 1) {
    alpha = bg
    bg = "by"
  }
  if (!is.null(bg) && length(bg) == 1 && bg == "by") {
    bg = by_col(
      ngrps = ngrps,
      col = NULL,
      palette = palette,
      gradient = by_continuous,
      ordered = by_ordered,
      alpha = alpha
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

  return(bg)
}
