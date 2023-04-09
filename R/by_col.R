by_col = function(ngrps, col = NULL, palette = NULL, palette.args = NULL) {
    
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

  out = do.call(
    function(...) Map(palette_fun, palette = palette, ...), 
    args = palette.args
    )[[1]]
 
  if (is.numeric(col)) {
    if (max(col) > length(out)) {
        stop(sprintf("The palette only has %s colors.", length(out)), call. = FALSE)
    }
    out = out[col]
  }
 
  return(out)

}
