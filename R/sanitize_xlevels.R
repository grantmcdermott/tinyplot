## Reorder the levels of a categorical variable, per a type's `xlevels` (or
## `ylevels`) argument. Shared by every type that exposes such an argument;
## the accepted inputs are:
##
##   - NULL:      keep the existing factor levels (the default everywhere
##                except type_errorbar()/type_pointrange())
##   - "asis":    take the categories in the order they appear in the data,
##                i.e. skip the alphabetical sorting that factor() applies
##                when coercing a character variable (cf. read.table's
##                `as.is` argument)
##   - character: the levels in the desired order
##   - numeric:   indexes into the existing levels, e.g. 3:1
##
## Only affects factors (character variables have already been coerced by
## sanitize_datapoints() when this runs inside a type_data() function); any
## other class is returned untouched, so the argument is inert for numeric
## variables. A length-1 "asis" is always read as the keyword: in the
## degenerate case of a category literally named "asis", set the factor
## levels beforehand instead.
##
## Site-specific follow-ups -- re-syncing `by` when it aliases the releveled
## variable (spineplot, ridge), or converting the factor to integer positions
## (points, lines, pointrange) -- remain at the call sites.
sanitize_xlevels = function(x, xlevels, arg = "xlevels") {
  if (is.null(xlevels) || !is.factor(x)) {
    return(x)
  }
  if (identical(xlevels, "asis")) {
    return(factor(x, levels = unique(x)))
  }
  if (is.numeric(xlevels)) {
    xlevels = levels(x)[xlevels]
  }
  if (anyNA(xlevels) || !all(xlevels %in% levels(x))) {
    warning(sprintf(
      "not all '%s' correspond to levels of '%s'",
      arg, substr(arg, 1, 1)
    ))
  }
  factor(x, levels = xlevels)
}
