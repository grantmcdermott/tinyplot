## auxiliary functions for formula/facet parsing

tinyformula = function(formula, facet = NULL) {
  ## input
  ## - formula:       y ~ x or y ~ x | z or ~ x or ~ x | z
  ## - facet:         ~ a or ~ a + b or b ~ a
  ##
  ## output:
  ## - x:      ~ x
  ## - y:      NULL or ~ y
  ## - by:     NULL or ~ z or ~ z1 + z2 + ... (use interaction of all)
  ## - xfacet: NULL or ~ a or ~ a + b etc.
  ## - yfacet: NULL or ~ b
  ## - full:   e.g. ~ x + y + z + a + b

  ## preliminaries
  if (!inherits(formula, "formula")) formula = as.formula(formula)
  nf = length(formula)

  ## basic formula types
  x     = ~ x
  y     = if (nf == 2L) NULL else ~ y
  by    = if (!inherits(formula[[nf]], "call") || formula[[nf]][[1L]] != as.name("|")) NULL else ~ z
  if (is.null(facet) || !inherits(facet, "formula")) {
    xfacet = NULL
    yfacet = NULL
  } else {
    xfacet = ~ a
    yfacet = if (length(facet) == 2L) NULL else ~ b
  }

  ## fill with actual terms
  environment(x) = environment(formula)
  if (!is.null(y)) {
    environment(y) = environment(formula)
    y[[2L]] = formula[[2L]]
  }
  if (is.null(by)) {
    x[[2L]] = formula[[nf]]
  } else {
    environment(by) = environment(formula)
    by[[2L]] = formula[[nf]][[3L]]
    x[[2L]] = formula[[nf]][[2L]]
  }
  if (!is.null(xfacet)) {
    environment(xfacet) = environment(formula)
    xfacet[[2L]] = facet[[length(facet)]]
  }
  if (!is.null(yfacet)) {
    environment(yfacet) = environment(formula)
    yfacet[[2L]] = facet[[2L]]
  }

  ## combine everything
  full = x
  if (!is.null(y))      full[[2L]] = call("+", full[[2L]], y[[2L]])
  if (!is.null(by))     full[[2L]] = call("+", full[[2L]], by[[2L]])
  if (!is.null(xfacet)) full[[2L]] = call("+", full[[2L]], xfacet[[2L]])
  if (!is.null(yfacet)) full[[2L]] = call("+", full[[2L]], yfacet[[2L]])

  ## return list of all formulas
  return(list(
    x = x,
    y = y,
    by = by,
    xfacet = xfacet,
    yfacet = yfacet,
    full = full
  ))
}

tinyframe = function(formula, data, drop = FALSE) {
  ## input
  ## - formula: (sub-)formula
  ## - data: model.frame from full formula
  if (is.null(formula)) return(NULL)
  names = sapply(attr(terms(formula), "variables")[-1L], deparse, width.cutoff = 500L)
  data[, names, drop = drop]
}
