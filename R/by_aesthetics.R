by_col = function(ngrps, col = NULL, palette = NULL, palette.args = NULL) {
  
  if (is.null(col)) {
    col = seq_len(ngrps)
  }

  if (is.atomic(col) && is.vector(col)) {
    if (length(col) == 1) {
        col = rep(col, ngrps)
    } else if (length(col) != ngrps) {
        stop(sprintf("`col` must be of length 1 or %s.", ngrps), call. = FALSE)
    }
    if (is.character(col)) return(col)
  }

  if (is.null(palette)) {
    if (ngrps<=9) {
      palette = "Okabe-Ito"
      palette_fun = palette.colors
    } else {
      palette = "Viridis"
      palette_fun = hcl.colors
    }
  } else if (palette %in% palette.pals()) {
    palette_fun = palette.colors
  } else if (palette %in% hcl.pals()) {
    palette_fun = hcl.colors
  } else {
    warning(
      "\nPalette string not recogized. Must be a value produced by either",
      "`palette.pals()` or `hcl.pals()`.",
      "\nUsing default option instead.\n",
      call. = FALSE
      )
    if (ngrps <= 9) {
      palette = "Okabe-Ito"
      palette_fun = palette.colors
    } else {
      palette = "Viridis"
      palette_fun = hcl.colors
    }
  }

  # n is a required argument for viridis and other palettes
  if (!"n" %in% names(palette.args)) palette.args[["n"]] = max(col)

  out = do.call(
    function(...) Map(palette_fun, palette = palette, ...), 
    args = palette.args
    )[[1]]
  
  out = out[col]
 
  return(out)

}


by_pch = function(ngrps, type, pch) {
  
  no_pch = FALSE
  # return NULL if not a valid point type
  if (!type %in% c("p", "b", "o")) {
    no_pch = TRUE
    pch = NULL
  } else if (is.null(pch)) {
    pch = 1
  }

  if (!no_pch) {
    
    if (!is.atomic(pch) || !is.vector(pch) || !is.numeric(pch) || (length(pch) != 1 && length(pch) != ngrps)) {
      stop(sprintf("`pch` must be `NULL` or a numeric vector of length 1 or %s.", ngrps), call. = FALSE)
    }
    
    if (length(pch) == 1) {
      pch = rep(pch, ngrps)
    }
  }
  
  return(pch)
}


by_lty = function(ngrps, type, lty) {

  # don't care about line type, return NULL
  if (!type %in% c("l", "b", "o")) {
    out = NULL

  # NULL -> solid line
  } else if (is.null(lty)) {
    out = rep(1, ngrps)
  
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