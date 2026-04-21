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
# ends at ~|tcl| + mgp[2] + 1, axis label baseline sits at mgp[1]), so the
# margin is the max of:
#   - tick-row height (|tcl| + mgp[2] + 1), when the axis is drawn
#   - axis-label extent (mgp[1] + (N - 1) * cex + 1 line for asc/desc), when present
# Main/sub sit above/below the plot box on the top/bottom side and add to the
# margin additively.
# Tick-label *width* for sides 2/4 (and *height* for 1/3 under las 2:3) is
# handled separately by the existing whtsbp logic in draw_facet_window().
# The caller is expected to take max(theme_mar[side], dynmar_side(...)) so
# that the theme's starting `mar` acts as a baseline padding.
dynmar_side = function(side, label, main = NULL, sub = NULL,
                      side.sub = 3, axis_on = TRUE, tpars = NULL) {
  mgp = get_tpar("mgp", tpar_list = tpars)
  tcl = get_tpar("tcl", tpar_list = tpars, default = par("tcl"))
  tick_extent = if (side %in% 1:2 && isTRUE(axis_on)) {
    max(0, -tcl) + mgp[2] + 1
  } else 0
  label_extent = 0
  lines = text_line_count(label)
  if (side %in% 1:2 && lines >= 1L) {
    cex_lab = get_tpar(
      if (side == 1L) c("cex.xlab", "cex.lab") else c("cex.ylab", "cex.lab"),
      tpar_list = tpars, default = 1
    )
    # Last-line baseline sits at mgp[1] + (N-1)*cex (after line-shift in
    # draw_title); add a full line to cover ascender+descender so the text
    # doesn't clip against the device edge.
    label_extent = mgp[1] + (lines - 1) * cex_lab + 1
  }
  mar = max(tick_extent, label_extent)
  if (side == 3L) {
    mlines = text_line_count(main)
    if (mlines >= 1L) {
      cex_main = get_tpar("cex.main", tpar_list = tpars, default = 1.2)
      # Main last-line baseline sits 0.7 lines above the box. Additional
      # lines stack upward at cex_main per row. The top line's visible
      # extent reaches ~0.6 * cex_main above its baseline (empirical from
      # strheight("X") / csi).
      mar = mar + 0.7 + (mlines - 1 + 0.6) * cex_main
    }
  }
  slines = text_line_count(sub)
  if (slines >= 1L && side == side.sub && side %in% c(1L, 3L)) {
    cex_sub = get_tpar("cex.sub", tpar_list = tpars, default = 1.2)
    # First sub row gets a 0.2-line extra bump; extra lines add cex_sub.
    # The top line's visible extent reaches ~0.6 * cex_sub above baseline.
    # If main is ALSO present on the same side, its ascender already covers
    # the top — the sub contribution is just the stacked sub row height.
    # Otherwise, add the sub's own ascender.
    has_main_here = side == 3L && text_line_count(main) >= 1L
    asc = if (has_main_here) 0 else 0.6 * cex_sub
    mar = mar + (cex_sub + 0.2) + (slines - 1) * cex_sub + asc
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

  if (!any(ooma != 0)) return(invisible(NULL))

  # Restore inner margin defaults (in case affected by preceding tinyplot call)
  if (any(ooma != 0)) {
    if (ooma[1] != 0 && omar[1] == par("mgp")[1] + 1 * par("cex.lab")) {
      omar[1] = 5.1
    }
    if (ooma[2] != 0 && omar[2] == par("mgp")[1] + 1 * par("cex.lab")) {
      omar[2] = 4.1
    }
    if (ooma[3] == topmar_epsilon && omar[3] != 4.1) {
      omar[3] = 4.1
    }
    if (ooma[4] != 0 && omar[4] == 0) {
      omar[4] = 2.1
    }
    par(mar = omar)
  }
  # Restore outer margin defaults (with a catch for custom mfrow plots)
  if (all(par("mfrow") == c(1, 1))) {
    par(omd = c(0, 1, 0, 1))
  }
}
