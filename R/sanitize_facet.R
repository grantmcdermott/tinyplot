sanitize_facet = function(settings) {
  list2env(settings[c("facet", "by", "null_facet", "facet_attr", "facet_by")], environment())

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
  settings = update_settings(settings, 
    facet = facet,
    null_facet = null_facet,
    facet_attr = facet_attr,
    facet_by = facet_by,
    by = by)
  return(settings)
}
