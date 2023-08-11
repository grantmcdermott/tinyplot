by_col = function(ngrps = 1L, col = NULL, palette = NULL) {
  
  # palette = substitute(palette, env = parent.env(environment()))
  
  # special "by" convenience keyword (will treat as NULL & handle grouping below)
  if (!anyNA(col) && !is.null(col) && length(col)==1 && col=="by") col = NULL

  if (is.null(col) && is.null(palette)) {
    col = seq_len(ngrps)
  }
  
  if (is.atomic(col) && is.vector(col)) {
    if (length(col) == 1) {
      col = rep(col, ngrps)
    } else if (length(col) != ngrps) {
      stop(sprintf("`col` must be of length 1 or %s.", ngrps), call. = FALSE)
    }
    if (anyNA(col) || is.character(col)) {
      return(col)
    }
  }

  if (is.null(palette)) {

    if (ngrps <= length(palette())) {
      palette_fun = function() palette() # must be function to avoid arg ambiguity
      args = list()
    } else {
      if (ngrps <= 8) {
        palette = "R4"
        palette_fun = palette.colors
      } else {
        palette = "Viridis"
        palette_fun = hcl.colors
      }
      args = list(n = ngrps, palette = palette)
    }

  } else {

    if (is.character(palette)) {
      if (tolower(palette) %in% tolower(palette.pals())) {
        palette_fun = palette.colors
      } else if (tolower(palette) %in% tolower(hcl.pals())) {
        palette_fun = hcl.colors
      } else {
        stop(
          "\nPalette string not recogized. Must be a value produced by either",
          "`palette.pals()` or `hcl.pals()`.\n",
          call. = FALSE
        )
      }
      args = list(n = ngrps, palette = palette)
    } else if (class(palette) %in% c("call", "name")) {
      args = as.list(palette)
      palette_fun = paste(args[[1]])
      args[[1]] = NULL
      args[["n"]] = ngrps
      # remove unnamed arguments to prevent unintentional argument sliding
      if (any(names(args)=="")) args[[which(names(args)=="")]] = NULL
    } else {
      stop(
        "\nInvalid palette argument. Must be a recognized keyword, or a",
        "palette-generating function with named arguments.\n"
        )
    }
  }

  cols = tryCatch(
    do.call(palette_fun, args),
    error = function(e) do.call(eval(palette), args) # catch for bespoke palette generating funcs
  )

  if (length(cols) > ngrps) cols = cols[1:ngrps]

  return(cols)

}


by_pch = function(ngrps, type, pch=NULL) {
  
  no_pch = FALSE
  if (!type %in% c("p", "b", "o", "pointrange", "errorbar")) {
    no_pch = TRUE
    pch = NULL
    
    # special "by" convenience keyword
  } else if (!is.null(pch) && length(pch)==1 && pch=="by") {
    no_pch = TRUE # skip checks below
    pch = 1:ngrps + par("pch") - 1
    # correctly recycle if over max pch type
    pch_ceiling = 25 # see ?pch
    if (max(pch)>pch_ceiling) {
      pch_below = pch[pch<=pch_ceiling]
      pch_above = pch[pch>pch_ceiling]
      pch_above = rep_len(0:pch_ceiling, length(pch_above))
      pch = c(pch_below, pch_above)
    }
    
    # return NULL if not a valid point type
  } else if (is.null(pch)) {
    pch = par("pch")
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


by_lty = function(ngrps, type, lty=NULL) {

  # don't care about line type, return NULL
  if (!type %in% c("l", "b", "o", "c", "h", "s", "S", "ribbon")) {
    out = NULL
    
    # special "by" convenience keyword
  } else if (!is.null(lty) && length(lty)==1 && lty=="by") {
    
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
      par_lty = which(par_lty==lty_dict)
    }
    out = 1:ngrps + par_lty - 1
    # correctly recycle if over max pch type
    lty_ceiling = 6 # see ?pch
    if (max(out)>lty_ceiling) {
      lty_below = out[out<=lty_ceiling]
      lty_above = out[out>lty_ceiling]
      lty_above = rep_len(1:lty_ceiling, length(lty_above))
      out = c(lty_below, lty_above)
    }
    
    # NULL -> solid (or default) line
  } else if (is.null(lty)) {
    out = rep(par("lty"), ngrps)
  
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