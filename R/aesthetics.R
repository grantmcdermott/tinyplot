
# utility function to convert user input about aesthetics into useable formats, including by-groups.
aesthetics = function(...) {
  list2env(list(...), environment())

  ngrps = length(split_data)

  pch = by_pch(ngrps = ngrps, type = type, pch = pch)

  lty = by_lty(ngrps = ngrps, type = type, lty = lty)

  lwd = by_lwd(ngrps = ngrps, type = type, lwd = lwd)

  col = by_col(
    ngrps = ngrps,
    col = col,
    palette = palette,
    gradient = by_continuous,
    ordered = by_ordered,
    alpha = alpha
  )
  if (is.null(bg) && !is.null(fill)) bg = fill
  if (!is.null(bg) && length(bg) == 1 && is.numeric(bg) && bg >= 0 && bg <= 1) {
    alpha = bg
    bg = "by"
  }
  if (!is.null(bg) && length(bg) == 1 && bg == "by") {
    bg = by_col(
      ngrps = ngrps,
      col = NULL,
      palette = palette,
      gradient = by_continuous,
      ordered = by_ordered,
      alpha = alpha
    )
  } else if (length(bg) != ngrps) {
    bg = rep(bg, ngrps)
  }
  if (type == "ribbon" || (type == "boxplot" && !is.null(by))) {
    if (!is.null(bg)) {
      bg = adjustcolor(bg, ribbon.alpha)
    } else if (!is.null(col)) {
      bg = adjustcolor(col, ribbon.alpha)
    }
  }

  ncolors = length(col)
  lgnd_labs = rep(NA, times = ncolors)
  if (isTRUE(by_continuous)) {
    ## Identify the pretty break points for our labels
    nlabs = 5
    ncolors = length(col)
    ubyvar = unique(by)
    byvar_range = range(ubyvar)
    pbyvar = pretty(byvar_range, n = nlabs)
    pbyvar = pbyvar[pbyvar >= byvar_range[1] & pbyvar <= byvar_range[2]]
    # optional thinning
    if (length(ubyvar) == 2 && all(ubyvar %in% pbyvar)) {
      pbyvar = ubyvar
    } else if (length(pbyvar) > nlabs) {
      pbyvar = pbyvar[seq_along(pbyvar) %% 2 == 0]
    }
    ## Find the (approximate) location of our pretty labels
    pidx = rescale_num(c(byvar_range, pbyvar), to = c(1, ncolors))[-c(1:2)]
    pidx = round(pidx)
    lgnd_labs[pidx] = pbyvar
  }

  return(as.list(environment()))
}

