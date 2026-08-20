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
  # facet variable name(s), for optional "varname = value" facet titles
  facet_names = NULL
  if (!is.null(facet) && length(facet) == 1 && facet == "by") {
    by = as.factor(by) ## if by==facet, then both need to be factors
    facet = by
    facet_by = TRUE
    facet_names = list(x = legend_args[["title"]] %||% by_dep) # same as legend
  } else if (!is.null(facet) && inherits(facet, "formula")) {
    facet = get_facet_fml(facet, data = data)
    if (isTRUE(attr(facet, "facet_grid"))) {
      facet.args[["nrow"]] = attr(facet, "facet_nrow")
    }
    facet_names = attr(facet, "facet_names")
  } else if (!is.null(facet)) {
    # names recorded by tinyplot.formula(), else fall back to the deparsed
    # input of the default method, e.g. facet = dat$fvar. (When called via
    # tinyplot.formula(), facet_dep is just the forwarded "facet" placeholder,
    # but that method has already recorded the real name(s).)
    facet_names = attr(facet, "facet_names")
    if (is.null(facet_names) && !is.null(facet_dep) && !facet_dep %in% c("facet", "NULL")) {
      facet_names = list(x = facet_dep)
    }
  }
  # The name(s) travel as an attribute so that they survive the handover from
  # tinyplot.formula(), but they get stripped here: `facet` flows on into
  # `datapoints`, where a stray attribute would break identity checks against
  # `by` (e.g. type_violin()).
  if (!is.null(facet)) attr(facet, "facet_names") = NULL

  facet_attr = attributes(facet) # TODO: better way to restore facet attributes?
  null_facet = is.null(facet)
  
  # update settings
  env2env(
    environment(),
    settings,
    c("facet", "null_facet", "facet_attr", "facet_by", "facet_names", "by")
  )
}
