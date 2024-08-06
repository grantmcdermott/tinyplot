## auxiliary functions for formula/facet parsing

tinyformula = function(formula, facet = NULL) {
  ## input
  ## - formula:       y ~ x or y ~ x | z or ~ x or ~ x | z
  ## - facet:         ~ a or ~ a + b or a ~ b
  ##
  ## output:
  ## - x:     ~ x
  ## - y:     NULL or ~ y
  ## - by:    NULL or ~ z or ~ z1 + z2 + ... (use interaction of all)
  ## - facet: NULL or ~ a or ~ a + b etc.
  ## - full:  e.g. ~ x + y + z + a + b

  ## preliminaries
  if (!inherits(formula, "formula")) formula = as.formula(formula)
  nf = length(formula)
  orig_facet = facet

  ## basic formula types
  x     = ~ x
  y     = if (nf == 2L) NULL else ~ y
  by    = if (!inherits(formula[[nf]], "call") || formula[[nf]][[1L]] != as.name("|")) NULL else ~ z
  facet = if (is.null(orig_facet) || !inherits(orig_facet, "formula")) NULL else ~ a + b

  ## fill with actual terms
  environment(x) = parent.frame()
  if (!is.null(y)) {
    environment(y) = parent.frame()
    y[[2L]] = formula[[2L]]
  }
  if (is.null(by)) {
    x[[2L]] = formula[[nf]]
  } else {
    environment(by) = parent.frame()
    by[[2L]] = formula[[nf]][[3L]]
    x[[2L]] = formula[[nf]][[2L]]
  }
  if (!is.null(facet)) {
    environment(facet) = parent.frame()
    if (length(orig_facet) == 3L) {
      facet[[2L]][[3L]] <- orig_facet[[3L]]
      facet[[2L]][[2L]] <- orig_facet[[2L]]    
    } else {
      facet[[2L]] <- orig_facet[[2L]]
    }
  }

  ## combine everything
  full = x
  if (!is.null(y))     full[[2L]] = call("+", full[[2L]], y[[2L]])
  if (!is.null(by))    full[[2L]] = call("+", full[[2L]], by[[2L]])
  if (!is.null(facet)) full[[2L]] = call("+", full[[2L]], facet[[2L]])

  ## return list of all formulas
  return(list(
    x = x,
    y = y,
    by = by,
    facet = facet,
    full = full
  ))
}

tinyframe <- function(formula, data, drop = FALSE) {
  ## input
  ## - formula: (sub-)formula
  ## - data: model.frame from full formula
  if (is.null(formula)) return(NULL)
  names = sapply(attr(terms(formula), "variables")[-1L], deparse, width.cutoff = 500L)
  data[, names, drop = drop]
}
