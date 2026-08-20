sanitize_facet = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "facet", "by", "null_facet", "facet_attr", "facet_by",
      "by_dep", "facet_dep", "legend_args"
    )
  )

  # flag if facet=="by" (i.e., facet matches the grouping variable)
  facet_by = FALSE
  # the facet variable(s) behind each side of the titles: a list of levels,
  # keyed by variable name. The names drive the optional "varname = value"
  # prefixes, and the levels let facet_titles() restore a value's type after
  # splitting a composite title apart. See facet_var_list().
  facet_vars = NULL
  if (!is.null(facet) && length(facet) == 1 && facet == "by") {
    by = as.factor(by) ## if by==facet, then both need to be factors
    facet = by
    facet_by = TRUE
    # facet titles inherit the "by" variable name (same as the legend title)
    facet_vars = list(x = facet_var_list(by, legend_args[["title"]] %||% by_dep))
  } else if (!is.null(facet) && inherits(facet, "formula")) {
    facet = get_facet_fml(facet, data = data)
    if (isTRUE(attr(facet, "facet_grid"))) {
      facet.args[["nrow"]] = attr(facet, "facet_nrow")
    }
    facet_vars = attr(facet, "facet_vars")
  } else if (!is.null(facet)) {
    # recorded by tinyplot.formula(), else fall back to the deparsed input of
    # the default method, e.g. facet = dat$fvar. (When called via
    # tinyplot.formula(), facet_dep is just the forwarded "facet" placeholder,
    # but that method has already recorded the real name.)
    facet_vars = attr(facet, "facet_vars")
    if (is.null(facet_vars) && !is.null(facet_dep) && !facet_dep %in% c("facet", "NULL")) {
      facet_vars = list(x = facet_var_list(facet, facet_dep))
    }
  }
  # The variables travel as an attribute so that they survive the handover from
  # tinyplot.formula(), but they get stripped here: `facet` flows on into
  # `datapoints`, where a stray attribute would break identity checks against
  # `by` (e.g. type_violin()).
  if (!is.null(facet)) attr(facet, "facet_vars") = NULL

  facet_attr = attributes(facet) # TODO: better way to restore facet attributes?
  null_facet = is.null(facet)

  # update settings
  env2env(
    environment(),
    settings,
    c("facet", "null_facet", "facet_attr", "facet_by", "facet_vars", "by")
  )
}
