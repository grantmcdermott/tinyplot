## Reorder the levels of a categorical variable, per a type's `xlevels` (or
## `ylevels`) argument. Shared by every type that exposes such an argument;
## the accepted inputs are:
##
##   - NULL:      keep the existing factor levels (the default everywhere
##                except type_errorbar()/type_pointrange())
##   - character: the levels in the desired order
##   - numeric:   indexes into the existing levels, e.g. 3:1
##
## Only affects factors (character variables have already been coerced by
## sanitize_datapoints() when this runs inside a type_data() function); any
## other class is returned untouched, so the argument is inert for numeric
## variables.
##
## Data-derived orderings -- "asis", "rev", ranking by size or variance -- are
## deliberately *not* handled here. They belong to the sibling `*ord` arguments
## and sanitize_ord(); keeping the two vocabularies disjoint is what makes each
## argument name mean one thing. The two compose, `*levels` first.
##
## Site-specific follow-ups -- re-syncing `by` when it aliases the releveled
## variable (spineplot, ridge), or converting the factor to integer positions
## (points, lines, pointrange) -- remain at the call sites.
sanitize_xlevels = function(x, xlevels, arg = "xlevels") {
  if (is.null(xlevels) || !is.factor(x)) {
    return(x)
  }
  if (is.numeric(xlevels)) {
    xlevels = levels(x)[xlevels]
  }
  v = substr(arg, 1, 1)
  if (anyNA(xlevels) || !all(xlevels %in% levels(x))) {
    warning(sprintf(
      "not all '%s' correspond to levels of '%s'",
      arg, v
    ))
  }
  # Naming a strict subset silently sends every other level to NA, which drops
  # those rows from the plot without a word. Ordering is all these arguments
  # claim to do, so treat a shortfall as a mistake worth flagging -- and a
  # complete miss (no supplied level matches at all) as fatal, since the
  # all-NA factor it produces only surfaces later as an unrelated error about
  # zero-length ranges.
  kept = intersect(levels(x), xlevels)
  if (length(kept) == 0L) {
    stop(sprintf(
      "'%s' matches none of the levels of '%s'.\n  Expected some of: %s",
      arg, v, paste(sprintf('"%s"', levels(x)), collapse = ", ")
    ), call. = FALSE)
  }
  dropped = setdiff(levels(x), xlevels)
  if (length(dropped) > 0L) {
    warning(sprintf(
      "'%s' omits %d of the %d levels of '%s' (%s); those observations will not be plotted",
      arg, length(dropped), nlevels(x), v,
      paste(sprintf('"%s"', dropped), collapse = ", ")
    ))
  }
  factor(x, levels = xlevels)
}
