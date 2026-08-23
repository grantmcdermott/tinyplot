## Derive a factor's level order from the data, per a type's `*ord` argument.
## This is the computed counterpart to sanitize_xlevels(), which respecifies
## levels literally. Accepts:
##
##   - NULL:       keep the existing factor levels (the default)
##   - "asis":     the categories in the order they appear in the data
##   - "start":    rank by the group's y value at the smallest x
##   - "end":      rank by the group's y value at the largest x
##   - "total":    rank by the group's summed y across every x
##
## ... and, for anything else, a function that is handed each group's y values
## (ordered by x) and returns a single number to sort *ascending* on. So
## `function(y) -sum(y)` reproduces "total", and `function(y) sum(y)` reverses
## it. This is the escape hatch for the reverse direction, and for statistics we
## don't have a keyword for (`function(y) -median(y)`, etc.).
##
## The three size keywords rank largest first, i.e. into the first level, which
## is the bottom band of a stacked area.
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

ord_keywords = c("asis", "start", "end", "total")

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

  # "asis" needs no y, and must work when y is absent or non-numeric
  if (identical(ord, "asis")) {
    return(factor(v, levels = unique(v)))
  }

  if (keyword) {
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
    stat = vapply(
      grps,
      function(z) if (length(z) == 0L) NA_real_ else as.numeric(ord(z)),
      numeric(1)
    )
  }

  # seq_along() breaks ties on the existing level order; empty groups sort last
  o = order(stat, seq_along(stat), na.last = TRUE)
  factor(v, levels = levels(v)[o])
}
