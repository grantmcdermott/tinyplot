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
## Only types whose categories span a real secondary axis pass one (`x = NULL`
## otherwise). Handing a barplot's ranking function the `by` level index would
## let `lm(y ~ x)` quietly return a number that means nothing, so asking for `x`
## where there is none is an error instead.
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

## The three sets below track what a type's categories actually are, since that
## is what decides which keywords can mean anything:
##
##   ord_keywords               a series along a secondary axis  (byord)
##   ord_keywords_distribution  a distribution, but no axis      (points, ...)
##   ord_keywords_scalar        a single value                   (bars, spines)
##
## "start"/"end" name a position along a *secondary* axis, so they only mean
## what they say for categories that span one -- the `by` groups of a stacked
## area, say. Elsewhere they would silently collapse onto "total" when there is
## no grouping, and silently re-read as "first/last `by` level" when there is.
ord_keywords_distribution = setdiff(ord_keywords, c("start", "end"))

## "minvar" then needs each category to carry a spread of its own: the scatter
## of points at an x position, the width of a ridge. A bar is a single
## aggregate and a spine a proportion of a count, so ranking either by variance
## would measure something the reader never sees -- cell values across
## `by`/facets for a bar (which stacking sums away and `beside` splits into
## separate bars), the supplied weights for a spine.
ord_keywords_scalar = setdiff(ord_keywords_distribution, "minvar")

sanitize_ord = function(v, y, x, ord, arg = "ord", keywords = ord_keywords) {
  # nlevels < 2 has exactly one ordering, so skip the work (and the degeneracy
  # check below, which a single level would otherwise trip).
  if (is.null(ord) || !is.factor(v) || nlevels(v) < 2L) {
    return(v)
  }

  keyword = is.character(ord) && length(ord) == 1L && ord %in% keywords
  if (!keyword && !is.function(ord)) {
    hint = if (is.character(ord) && length(ord) == 1L && ord %in% ord_keywords) {
      sprintf("\n  \"%s\" is not available for this plot type.", ord)
    } else {
      "\n  To set the level order explicitly, use factor(levels = ) on the variable beforehand."
    }
    stop(
      sprintf(
        "`%s` must be NULL, one of %s, or a function.%s",
        arg, paste(sprintf('"%s"', keywords), collapse = ", "), hint
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

  # Everything below ranks on numbers. Reaching here with a non-numeric is
  # almost always a transposed formula (e.g. a ridge plot called with the
  # continuous variable on the categorical side), so say that rather than
  # letting var()/sum() fail with something cryptic about factors.
  if (!is.numeric(y)) {
    stop(
      sprintf(
        "`%s = \"%s\"` ranks on a numeric variable, but was given %s.\n  Only \"asis\" and \"rev\" work without one.",
        arg, ord, class(y)[1L]
      ),
      call. = FALSE
    )
  }

  if (identical(ord, "minvar")) {
    # Ascending, i.e. *not* negated like the size keywords below: a stacked
    # baseline is steadiest when the least variable group sits on it, since
    # every band above inherits its movement. Groups too short to have a
    # variance give NA and sort last (to the top), which is the right place
    # for them anyway.
    stat = tapply(y, v, function(z) var(z, na.rm = TRUE), default = NA_real_)
    # A variance that is NA everywhere (one observation per group) or identical
    # everywhere (constant weights) cannot order anything, and would otherwise
    # return the input untouched -- a silent no-op is the worst outcome here.
    if (length(unique(stat)) < 2L) {
      stop(
        sprintf(
          "`%s = \"minvar\"` cannot order these groups: %s.",
          arg,
          if (all(is.na(stat))) {
            "each has fewer than two observations, so there is no variance to rank on"
          } else {
            "every group has the same variance"
          }
        ),
        call. = FALSE
      )
    }
  } else if (keyword) {
    if (identical(ord, "total")) {
      keep = rep.int(TRUE, length(y))
    } else {
      edge = if (identical(ord, "start")) min(x, na.rm = TRUE) else max(x, na.rm = TRUE)
      keep = !is.na(x) & x == edge
    }
    stat = tapply(y[keep], v[keep], function(z) sum(z, na.rm = TRUE), default = 0)
    stat = -stat # largest group first, i.e. the bottom band
  } else {
    xord = if (is.null(x)) seq_along(y) else order(x)
    grps = split(y[xord], v[xord])
    # Hand over x too, but only to functions that ask for it by name; see the
    # note at the top of this file.
    want_x = "x" %in% names(formals(ord))
    if (want_x && is.null(x)) {
      stop(
        sprintf(
          "the `%s` function asks for `x`, but this plot type has no secondary axis to supply.\n  Its categories are a flat set, so drop the `x` argument and rank on `y` alone.",
          arg
        ),
        call. = FALSE
      )
    }
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
