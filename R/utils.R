## Null coalescing operator
if (getRversion() <= "4.4.0") {
  `%||%` = function(x, y) if (is.null(x)) y else x
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
