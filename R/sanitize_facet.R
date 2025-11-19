sanitize_facet = function(settings) {
  env2env(settings, environment(), c("facet", "by", "null_facet", "facet_attr", "facet_by"))

  # flag if facet=="by" (i.e., facet matches the grouping variable)
  facet_by = FALSE
  if (!is.null(facet) && length(facet) == 1 && facet == "by") {
    by = as.factor(by) ## if by==facet, then both need to be factors
    facet = by
    facet_by = TRUE
  } else if (!is.null(facet) && inherits(facet, "formula")) {
    facet = get_facet_fml(facet, data = data)
    if (isTRUE(attr(facet, "facet_grid"))) {
      facet.args[["nrow"]] = attr(facet, "facet_nrow")
    }
  }
  facet_attr = attributes(facet) # TODO: better way to restore facet attributes?
  null_facet = is.null(facet)
  env2env(environment(), settings, c("facet", "null_facet", "facet_attr", "facet_by", "by"))
}
