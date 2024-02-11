rescale_num = function (x, from = NULL, to = NULL) {
  if (is.null(from)) from = range(x)
  if (is.null(to)) to = c(0, 1)
  (x - from[1])/diff(from) * diff(to) + to[1]
}


more_than_n_unique= function(x, n, small_vec_len = 1e4L) {
  len_x = length(x)
  # For "small" vectors, just use direct length(unique(x)) approach
  if (len_x <= small_vec_len) {
    return(length(unique(x)) > 5L)
  } else {
    # For larger vectors, use the hash set approach
    env = new.env(hash = TRUE, size = 5L)
    count = 0
    for (val in x) {
      if (!exists(as.character(val), env)) {
        assign(as.character(val), TRUE, env)
        count = count + 1
        if (count > 5) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
}

