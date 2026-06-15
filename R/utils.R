## Null coalescing operator
if (getRversion() <= "4.4.0") {
  `%||%` = function(x, y) if (is.null(x)) y else x
}

# Count text lines. Returns 0 for absent text and 1 for expression objects.
text_line_count = function(x) {
  if (is.null(x)) return(0L)
  if (identical(x, NA) || identical(x, NA_character_)) return(0L)
  if (!is.character(x)) return(1L)
  if (!length(x)) return(0L)
  keep = !is.na(x) & nzchar(x)
  if (!any(keep)) return(0L)
  x = x[which(keep)[1L]]
  as.integer(1L + nchar(gsub("[^\n]", "", x)))
}

# Compute additive margin "build" for a given side under dynmar.
# Starts from zero and adds only what the plot actually needs. The tick
# row and the axis-label row occupy overlapping vertical space (tick row
# ends at ~|tcl| + mgp[2] + descent, axis label baseline sits at mgp[1]),
# so the margin is the max of:
#   - tick-row height (|tcl| + mgp[2] + descent), when the axis is drawn
#   - axis-label extent (mgp[1] + (N - 1) * cex + descent), when present
# The descent term = 0.4*cex + 0.6: text at the mgp reference line extends
# ~0.4*cex lines below baseline (empirically measured character cell descent),
# plus 0.6 lines fixed buffer for descender characters and spacing.
# Main/sub sit above/below the plot box on the top/bottom side and add to the
# margin additively.
# Tick-label *width* for sides 2/4 (and *height* for 1/3 under las 2:3) is
# handled separately by the existing whtsbp logic in draw_facet_window().
# The caller is expected to take max(theme_mar[side], dynmar_side(...)) so
# that the theme's starting `mar` acts as a baseline padding.
dynmar_side = function(side, label, main = NULL, sub = NULL, cap = NULL,
                      side.sub = 3, axis_on = TRUE, tpars = NULL) {
  mgp = get_tpar("mgp", tpar_list = tpars)
  tcl = get_tpar("tcl", tpar_list = tpars, default = par("tcl"))
  tick_extent = if (side %in% 1:2 && isTRUE(axis_on)) {
    cex_axis = get_tpar("cex.axis", tpar_list = tpars, default = 1)
    max(0, -tcl) + mgp[2] + 0.4 * cex_axis + 0.6
  } else 0
  label_extent = 0
  lines = text_line_count(label)
  if (side %in% 1:2 && lines >= 1L) {
    cex_lab = get_tpar(
      if (side == 1L) c("cex.xlab", "cex.lab") else c("cex.ylab", "cex.lab"),
      tpar_list = tpars, default = 1
    )
    # Side 2: rotated ylab baseline is shifted by ylab_cex_shift, so far edge
    # = mgp[1] + 0.1*cex_lab + 0.5; descent = 0.1*cex + 0.9 gives constant
    # 0.4-line buffer (and exactly 1.0 at cex=1, preserving existing behavior).
    # Side 1: horizontal xlab ink extends ~0.2*cex below center; +0.8 buffer.
    descent = if (side == 2L) 0.1 * cex_lab + 0.9 else 0.2 * cex_lab + 0.8
    label_extent = mgp[1] + (lines - 1) * cex_lab + descent
    # Expressions (e.g., ylab = expression(mm^{1/2})) can be taller than a
    # plain text line due to superscripts, subscripts, fractions, etc. Measure
    # the actual rendered height and add the excess over a normal text line.
    if (is.language(label)) {
      expr_lines = strheight(label, units = "inches", cex = cex_lab) / par("csi")
      text_lines = strheight("X", units = "inches", cex = cex_lab) / par("csi")
      excess = expr_lines - text_lines
      if (excess > 0) label_extent = label_extent + excess
    }
  }
  mar = max(tick_extent, label_extent)
  if (side == 3L) {
    mlines = text_line_count(main)
    if (mlines >= 1L) {
      cex_main = get_tpar("cex.main", tpar_list = tpars, default = 1.2)
      gap_main = get_tpar("gap.main", tpar_list = tpars, default = 0.7)
      mar = mar + gap_main + (mlines - 1 + 0.6) * cex_main
    }
  }
  slines = text_line_count(sub)
  if (slines >= 1L && side == side.sub && side %in% c(1L, 3L)) {
    cex_sub = get_tpar("cex.sub", tpar_list = tpars, default = 1.2)
    gap_sub = get_tpar("gap.sub", tpar_list = tpars, default = 0.7)
    has_main_here = side == 3L && text_line_count(main) >= 1L
    asc = if (has_main_here) 0 else 0.6 * cex_sub
    mar = mar + gap_sub + (slines - 1 + 0.6) * cex_sub + asc
  }
  clines = text_line_count(cap)
  if (clines >= 1L && side == 1L) {
    cex_cap = get_tpar("cex.cap", tpar_list = tpars, default = 1)
    mar = mar + (cex_cap + 0.2) + (clines - 1) * cex_cap
  }
  mar
}


## Function that computes an appropriate bandwidth kernel based on a string
## input
bw_fun = function(kernel, x) {
  kernel = tolower(kernel)
  switch(kernel,
    nrd0 = bw.nrd0(x),
    nrd  = bw.nrd(x),
    ucv  = bw.ucv(x),
    bcv  = bw.bcv(x),
    sj   = bw.SJ(x),
    stop("Invalid `bw` string. Choose from 'nrd0', 'nrd', 'ucv', 'bcv', or 'SJ'.")
  )
}


# Assign (inject) elements from one environment into another
env2env = function(source_env, target_env, keys = NULL) {
  if (is.null(keys)) {
    keys = ls(source_env, all.names = TRUE)
  }
  for (nm in keys) {
    assign(nm, source_env[[nm]], envir = target_env)
  }
}


## Function for efficiently checking whether a vector has more than n unique
## values (uses a hash set approach for large vectors to check sequentially)
more_than_n_unique = function(x, n, small_vec_len = 1e3L) {
  len_x = length(x)
  # For "small" vectors, just use direct length(unique(x)) approach
  if (len_x <= small_vec_len) {
    return(length(unique(x)) > n)
  } else {
    # For larger vectors, use the hash set approach
    env = new.env(hash = TRUE, size = n)
    count = 0
    for (val in x) {
      if (!exists(as.character(val), env)) {
        assign(as.character(val), TRUE, env)
        count = count + 1
        if (count > n) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
}


# Rescale numeric (used for continuous legends, etc.)
rescale_num = function(x, from = NULL, to = NULL) {
  if (is.null(from)) from = range(x)
  if (is.null(to)) to = c(0, 1)
  (x - from[1]) / diff(from) * diff(to) + to[1]
}


# Convenience function for swapping variables (e.g., use in flipped plots)
swap_variables = function(env, ...) {
  pairs = list(...)
  for (p in pairs) {
    tmp = get(p[1], envir = env)
    assign(p[1], get(p[2], envir = env), envir = env)
    assign(p[2], tmp, envir = env)
  }
}

# Convenience function for swapping columns (e.g., use in flipped plots)
swap_columns = function(dp, a, b) {
  va = dp[[a]]
  vb = dp[[b]]
  dp[[a]] = if (!is.null(vb)) vb else NULL
  dp[[b]] = if (!is.null(va)) va else NULL
  dp
}


#' Restore outer margin defaults
#'
#' @description Resets the outer margin display (omd) to default full device.
#'   Used to clean up after legend drawing that may have adjusted margins.
#'
#' @returns NULL (called for side effect of resetting par("omd"))
#'
#' @keywords internal
restore_margin_outer = function() {
  par(omd = c(0, 1, 0, 1))
}


#' Restore inner margin defaults
#'
#' @description Resets inner margins that may have been adjusted for legend
#'   placement. Handles special cases for each margin side and checks for
#'   custom mfrow layouts.
#'
#' @param ooma Outer margins (from par("oma"))
#' @param topmar_epsilon Small epsilon value for top margin adjustment (default 0.1)
#'
#' @returns NULL (called for side effect of resetting par("mar"))
#'
#' @keywords internal
restore_margin_inner = function(ooma, topmar_epsilon = 0.1) {
  ooma = par("oma")
  omar = par("mar")

  # omd reset (from restore_margin_outer) clears oma as a side-effect, so
  # detect dirty margins directly from mar values rather than relying on oma.
  top_dirty = omar[3] > 4.1
  right_dirty = omar[4] == 0
  left_dirty = ooma[2] != 0 && omar[2] == par("mgp")[1] + 1 * par("cex.lab")
  bottom_dirty = ooma[1] != 0 && omar[1] == par("mgp")[1] + 1 * par("cex.lab")

  if (!any(ooma != 0) && !top_dirty && !right_dirty) return(invisible(NULL))

  # Restore inner margin defaults (in case affected by preceding tinyplot call)
  if (bottom_dirty) omar[1] = 5.1
  if (left_dirty) omar[2] = 4.1
  if (top_dirty) omar[3] = 4.1
  if (right_dirty) omar[4] = 2.1
  par(mar = omar)

  # Restore outer margin defaults (with a catch for custom mfrow plots)
  if (all(par("mfrow") == c(1, 1))) {
    par(omd = c(0, 1, 0, 1))
  }
}


# Convert colour(s) to HCL-like (Luv) coordinates, preserving alpha. Helper for
# seq_palette(). (Originally lived in type_spineplot.R.)
#' @importFrom grDevices col2rgb convertColor hcl
to_hcl = function(x) {
    x = t(col2rgb(x, alpha = TRUE)/255)
    alpha = x[, 4]
    x = x[, 1:3]
    x = convertColor(x, from = "sRGB", to = "Luv")
    x = cbind(H = atan2(x[, 3L], x[, 2L]) * 180/pi, C = sqrt(x[, 2L]^2 + x[, 3L]^2), L = x[, 1L])
    x[is.na(x[, 1L]), 1L] = 0
    x[x[, 1L] < 0, 1L] = x[x[, 1L] < 0, 1L] + 360
    attr(x, "alpha") = alpha
    return(x)
}

# Build an n-step sequential ramp from colour `x` toward near-white, in HCL
# space (reduces chroma, increases lightness). Used for single-group fills
# (boxplot/violin/barplot/histogram), ridge fills, legend swatches, and
# spineplot shading. `seq_palette(col, 3)[3]` is thus a lighter-but-*opaque*
# tint of `col`. When `grayscale = TRUE`, returns a neutral grey ramp via
# gray.colors() instead (used by spineplot when no colour grouping is active).
#' @importFrom grDevices gray.colors
seq_palette = function(x, n, power = 1.5, grayscale = FALSE) {
    if (isTRUE(grayscale)) return(gray.colors(n))
    x = drop(to_hcl(x[1L]))
    alpha = attr(x, "alpha")
    hcl(
      h = x[1L],
      c = seq.int(from = x[2L]^(1/power), to = 0, length.out = n + 1)[1L:n]^power,
      l = 100 - seq.int(from = (100 - x[3L])^(1/power), to = pmin(8, (100 - x[3L])/2)^(1/power), length.out = n)^power,
      alpha = alpha
    )[1L:n]
}
