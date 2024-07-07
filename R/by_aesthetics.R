by_col = function(ngrps = 1L, col = NULL, palette = NULL, gradient = NULL, ordered = NULL, alpha = NULL) {
  
  if (is.null(ordered)) ordered = FALSE
  if (is.null(alpha)) alpha = 1
  if (is.null(gradient)) gradient = FALSE
  if (isTRUE(gradient)) {
    ngrps = 100L
  }
  
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
      if (isFALSE(gradient)) {
        stop(sprintf("`col` must be of length 1 or %s.", ngrps), call. = FALSE)
      } else {
        # interpolate gradient colors
        col = colorRampPalette(colors = col, alpha = TRUE)(ngrps)
      }
    } 
    if (isTRUE(gradient)) {
      col = rev(col)
    }
    if (anyNA(col) || is.character(col)) {
      return(col)
    }
  }

  if (is.null(palette)) {

    if (ngrps <= length(palette()) && isFALSE(ordered) && isFALSE(gradient)) {
      palette_fun = function(alpha) adjustcolor(palette(), alpha) # must be function to avoid arg ambiguity
      args = list(alpha = alpha)
    } else {
      if (ngrps <= 8 && isFALSE(ordered)) { # ngrps < 100 so we know gradient is FALSE too
        palette = "R4"
        palette_fun = palette.colors
      } else {
        palette = "Viridis"
        if (isFALSE(gradient) && isFALSE(ordered)) {
          palette_fun = hcl.colors
        } else {
          palette_fun_gradient = function(n, palette, from = 0.1, to = 0.9, alpha = 1)  {
            colorRampPalette(
              hcl.colors(n = 100, palette = palette, alpha = alpha)[(100 * from + 1):(100 * to)],
              alpha = TRUE
            )(n)
          }
          palette_fun = palette_fun_gradient
        }
        
      } 
      args = list(n = ngrps, palette = palette, alpha = alpha)
    }

  } else {

    if (is.character(palette)) {

      fx = function(x) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", x))
      pal_match = charmatch(fx(palette), fx(palette.pals()))
      if (!is.na(pal_match)) {
        if (pal_match < 1L) stop("'palette' is ambiguous")
        palette_fun = palette.colors
        if (isTRUE(gradient)) {
          palette_fun2 = function(n, palette, alpha) colorRampPalette(palette.colors(palette = palette, alpha = alpha))(n)
          palette_fun = palette_fun2
        }
      } else {
        pal_match = charmatch(fx(palette), fx(hcl.pals()))
        if (!is.na(pal_match)) {
          if (pal_match < 1L) stop("'palette' is ambiguous")
          palette_fun = hcl.colors
        } else {
          stop(
            "\nPalette string not recogized. Must be a value produced by either",
            "`palette.pals()` or `hcl.pals()`.\n",
            call. = FALSE
          )
        }
      }
      args = list(n = ngrps, palette = palette, alpha = alpha)

    } else if (class(palette) %in% c("call", "name")) {

      args = as.list(palette)
      palette_fun = paste(args[[1]])
      args[[1]] = NULL
      args[["n"]] = ngrps
      # remove unnamed arguments to prevent unintentional argument sliding
      if (any(names(args) == "")) args[[which(names(args) == "")]] = NULL

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
  
  # For gradient and ordered colors, we'll run high to low
  if (isTRUE(gradient) || isTRUE(ordered)) cols = rev(cols)

  return(cols)

}


by_pch = function(ngrps, type, pch=NULL) {
  
  no_pch = FALSE
  if (!type %in% c("p", "b", "o", "pointrange", "errorbar", "boxplot")) {
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

  # We only care about line types, otherwise return NULL
  if (!type %in% c("l", "b", "o", "c", "h", "s", "S", "ribbon", "boxplot", "rect", "segments")) {
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
    if (type == "boxplot") {
      out = NULL
    } else {
      out = rep(par("lty"), ngrps)
    }
  
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


by_lwd = function(ngrps, type, lwd=NULL) {

  lwd_base = par("lwd")
  lwd_floor = lwd_base / min(5, max((ngrps-1), 1))
  lwd_ceiling = lwd_base * min(5, ngrps)
  
  no_lwd = FALSE
  # special "by" convenience keyword
  if (!is.null(lwd) && length(lwd)==1 && lwd=="by") {
    no_lwd = TRUE # skip checks below
    lwd = seq(lwd_floor, lwd_ceiling, length.out = ngrps)
  } else if (is.null(lwd)) {
    no_lwd = TRUE
    lwd = NULL
  }

  if (!no_lwd) {
    if (!is.atomic(lwd) || !is.vector(lwd) || !is.numeric(lwd) || (length(lwd) != 1 && length(lwd) != ngrps)) {
      stop(sprintf("`lwd` must be `NULL` or a numeric vector of length 1 or %s.", ngrps), call. = FALSE)
    }
    if (length(lwd) == 1) {
      lwd = rep(lwd, ngrps)
    }
  }
  
  return(lwd)
}
