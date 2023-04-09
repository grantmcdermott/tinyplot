by_pch = function(ngrps, pch) {
  if (is.null(pch)) pch = 1

  if (!is.atomic(pch) || !is.vector(pch) || !is.numeric(pch) || (length(pch) != 1 && length(pch) != ngrps)) {
    stop(sprintf("`pch` must be `NULL` or a numeric vector of length 1 or %s.", ngrps), call. = FALSE)
  }
  
  if (length(pch) == 1) {
    pch = rep(pch, ngrps)
  }
  
  return(pch)
}