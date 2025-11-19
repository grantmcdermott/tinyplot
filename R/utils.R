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
