rescale_num = function (x, from = NULL, to = NULL) {
  if (is.null(from)) from = range(x)
  if (is.null(to)) to = c(0, 1)
  (x - from[1])/diff(from) * diff(to) + to[1]
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
