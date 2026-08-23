## Derive a factor's level order from the data, per a type's `*ord` argument.
## This is the computed counterpart to sanitize_xlevels(), which respecifies
## levels literally. Accepts:
##
##   - NULL:       keep the existing factor levels (the default)
##   - "asis":     the categories in the order they appear in the data
##   - "rev":      the existing factor levels, reversed
##   - "start":    rank by the group's y value at the smallest x
##   - "end":      rank by the group's y value at the largest x
##   - "total":    rank by the group's summed y across every x
##   - "minvar":   rank by the group's variance, least variable first
##
## ... and, for anything else, a function that is handed each group's y values
## (ordered by x) and returns a single number to sort *ascending* on. So
## `function(y) -sum(y)` reproduces "total", and `function(y) sum(y)` reverses
## it. This is the escape hatch for the reverse direction, and for statistics we
## don't have a keyword for (`function(y) -median(y)`, etc.).
##
## A function that declares a formal named `x` also receives that group's x
## values, which is what any statistic depending on the spacing between
## observations needs -- a slope, say: `function(y, x) coef(lm(y ~ x))[2]`.
## Keying on the *name* rather than the number of formals is deliberate: it
## keeps a tuning parameter carrying a default, e.g. `function(y, p = 0.9)`,
## from being silently handed x. x is passed by name, so the two arguments may
## be declared in either order.
##
## "asis" and "rev" are the two keywords that consult no data at all -- they
## just permute the levels -- so they work when y is absent or non-numeric.
## "rev" is also the one thing a ranking function cannot express: a function is
## handed only its own group's y values, never its group identity or level
## index, so it has no way to say "put me where I already am, backwards". Note
## that it reverses the *existing* level order only; to reverse what another
## keyword computed, negate it with a function instead (`function(y) sum(y)` is
## the reverse of "total").
##
## The three size keywords rank largest first, i.e. into the first level, which
## is the bottom band of a stacked area. "minvar" ranks the *other* way --
## smallest first -- because there the stable baseline is the calm group, not
## the big one. Both directions serve the same end.
##
## Explicit level names or indexes are deliberately *not* accepted here -- that
## is what sanitize_xlevels() is for, and letting both arguments take the same
## inputs would collapse the distinction between them. Types that expose only
## `*ord` point users at factor() instead; see the error below.
##
## Ranking pools over facets. `by` levels are global -- one legend, one colour
## mapping -- so ordering each facet separately would desync the legend from the
## groups it labels. Absent groups (and NA values) count as zero, matching how
## stack_area() completes a ragged grid. Ties keep their existing relative
## order. Only factors are touched, so the argument is inert for continuous
## groupings.
##
## As with sanitize_xlevels()'s "asis", the keywords win over a same-named
## category: in the degenerate case of a group literally called "end", set the
## factor levels beforehand instead.

ord_keywords = c("asis", "rev", "start", "end", "total", "minvar")

sanitize_ord = function(v, y, x, ord, arg = "ord") {
  if (is.null(ord) || !is.factor(v)) {
    return(v)
  }

  keyword = is.character(ord) && length(ord) == 1L && ord %in% ord_keywords
  if (!keyword && !is.function(ord)) {
    stop(
      sprintf(
        "`%s` must be NULL, one of %s, or a function.\n  To set the level order explicitly, use factor(levels = ) on the variable beforehand.",
        arg, paste(sprintf('"%s"', ord_keywords), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # "asis" and "rev" need no y, and must work when y is absent or non-numeric.
  # factor() defaults `ordered` to is.ordered(v), so an ordered grouping stays
  # ordered (and keeps its sequential palette) through either.
  if (identical(ord, "asis")) {
    return(factor(v, levels = unique(v)))
  }
  if (identical(ord, "rev")) {
    return(factor(v, levels = rev(levels(v))))
  }

  if (identical(ord, "minvar")) {
    # Ascending, i.e. *not* negated like the size keywords below: a stacked
    # baseline is steadiest when the least variable group sits on it, since
    # every band above inherits its movement. Groups too short to have a
    # variance give NA and sort last (to the top), which is the right place
    # for them anyway.
    stat = tapply(y, v, function(z) var(z, na.rm = TRUE), default = NA_real_)
  } else if (keyword) {
    if (identical(ord, "total")) {
      keep = rep.int(TRUE, length(x))
    } else {
      edge = if (identical(ord, "start")) min(x, na.rm = TRUE) else max(x, na.rm = TRUE)
      keep = !is.na(x) & x == edge
    }
    stat = tapply(y[keep], v[keep], function(z) sum(z, na.rm = TRUE), default = 0)
    stat = -stat # largest group first, i.e. the bottom band
  } else {
    xord = order(x)
    grps = split(y[xord], v[xord])
    # Hand over x too, but only to functions that ask for it by name; see the
    # note at the top of this file.
    want_x = "x" %in% names(formals(ord))
    xgrps = if (want_x) split(x[xord], v[xord]) else NULL
    stat = vapply(
      seq_along(grps),
      function(i) {
        z = grps[[i]]
        if (length(z) == 0L) return(NA_real_)
        as.numeric(if (want_x) ord(z, x = xgrps[[i]]) else ord(z))
      },
      numeric(1)
    )
  }

  # seq_along() breaks ties on the existing level order; empty groups sort last
  o = order(stat, seq_along(stat), na.last = TRUE)
  factor(v, levels = levels(v)[o])
}
