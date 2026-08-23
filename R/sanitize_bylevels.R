## Reorder the levels of a `by` grouping variable, per a type's `bylevels`
## argument. Accepts everything that sanitize_xlevels() does, and defers to it
## for those cases:
##
##   - NULL:       keep the existing factor levels (the default)
##   - "asis":     the categories in the order they appear in the data
##   - character:  the levels in the desired order
##   - numeric:    indexes into the existing levels, e.g. 3:1
##
## ... plus three data-dependent keywords that rank the groups by size, largest
## first (i.e. into the first level, which is the bottom band of a stacked area):
##
##   - "start":    the group's y value at the smallest x
##   - "end":      the group's y value at the largest x
##   - "total":    the group's summed y across every x
##
## ... and, for anything else, a function that is handed each group's y values
## (ordered by x) and returns a single number to sort *ascending* on. So
## `function(y) -sum(y)` reproduces "total", and `function(y) sum(y)` reverses
## it. This is the escape hatch for the reverse direction, and for statistics we
## don't have a keyword for (`function(y) -median(y)`, etc.).
##
## Ranking pools over facets. `by` levels are global -- one legend, one colour
## mapping -- so ordering each facet separately would desync the legend from the
## groups it labels. Absent groups (and NA values) count as zero, matching how
## stack_area() completes a ragged grid. Ties keep their existing relative
## order. Only factors are touched, so the argument is inert for continuous
## groupings.
##
## As with sanitize_xlevels()'s "asis", the keywords win over a same-named
## category: in the degenerate case of a group literally called "end", pass the
## levels explicitly instead.

## Only the size keywords. "asis" belongs to the xlevels vocabulary and is
## delegated below; routing it through here would silently treat it as "end".
bylevels_size_keywords = c("start", "end", "total")

sanitize_bylevels = function(by, y, x, bylevels, arg = "bylevels") {
  if (is.null(bylevels) || !is.factor(by)) {
    return(by)
  }

  size_keyword = is.character(bylevels) && length(bylevels) == 1L &&
    bylevels %in% bylevels_size_keywords

  # Static respecifications are the shared xlevels vocabulary; only the
  # data-dependent cases need the machinery below.
  if (!size_keyword && !is.function(bylevels)) {
    return(sanitize_xlevels(by, bylevels, arg = arg))
  }

  if (size_keyword) {
    if (identical(bylevels, "total")) {
      keep = rep.int(TRUE, length(x))
    } else {
      edge = if (identical(bylevels, "start")) min(x, na.rm = TRUE) else max(x, na.rm = TRUE)
      keep = !is.na(x) & x == edge
    }
    stat = tapply(y[keep], by[keep], function(z) sum(z, na.rm = TRUE), default = 0)
    stat = -stat # largest group first, i.e. the bottom band
  } else {
    xord = order(x)
    grps = split(y[xord], by[xord])
    stat = vapply(
      grps,
      function(z) if (length(z) == 0L) NA_real_ else as.numeric(bylevels(z)),
      numeric(1)
    )
  }

  # seq_along() breaks ties on the existing level order; empty groups sort last
  ord = order(stat, seq_along(stat), na.last = TRUE)
  factor(by, levels = levels(by)[ord])
}
