# calculate limits of each plot

lim_args = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "xaxb", "xlabs", "xlim", "null_xlim", "rev_x",
      "yaxb", "ylabs", "ylim", "null_ylim", "rev_y",
      "datapoints", "type"
    )
  )

  # For cases where x/yaxb is provided and corresponding x/ylabs is not null...
  # We can subset these here to provide breaks
  if (!is.null(xaxb) && !is.null(xlabs)) {
    xlabs = xlabs[names(xlabs) %in% xaxb]
    xaxb = NULL # don't need this any more
  }
  if (!is.null(yaxb) && !is.null(ylabs)) {
    ylabs = ylabs[names(ylabs) %in% yaxb]
    yaxb = NULL # don't need this any more
  }

  if (is.null(xlim)) {
    xlim = range(as.numeric(c(
      datapoints[["x"]], datapoints[["xmin"]],
      datapoints[["xmax"]])), finite = TRUE)
  } else if (length(xlim) != 2L || anyNA(xlim)) {
    xdrng = range(as.numeric(c(
      datapoints[["x"]], datapoints[["xmin"]],
      datapoints[["xmax"]])), finite = TRUE)
    xlim = resolve_lim(xlim, xdrng, "xlim")
  }
  if (is.null(ylim)) {
    ylim = range(as.numeric(c(
      datapoints[["y"]], datapoints[["ymin"]],
      datapoints[["ymax"]])), finite = TRUE)
  } else if (length(ylim) != 2L || anyNA(ylim)) {
    ydrng = range(as.numeric(c(
      datapoints[["y"]], datapoints[["ymin"]],
      datapoints[["ymax"]])), finite = TRUE)
    ylim = resolve_lim(ylim, ydrng, "ylim")
  }

  if (identical(type, "boxplot")) {
    xlim = xlim + c(-0.5, 0.5)
  }

  if (null_xlim && !is.null(xaxb) && type != "spineplot") xlim = range(c(xlim, xaxb))
  if (null_ylim && !is.null(yaxb) && type != "spineplot") ylim = range(c(ylim, yaxb))

  # reverse axis direction last, once the range is otherwise finalized
  if (isTRUE(rev_x)) xlim = rev(xlim)
  if (isTRUE(rev_y)) ylim = rev(ylim)

  # update settings
  env2env(
    environment(),
    settings,
    c("xlim", "ylim", "xlabs", "ylabs", "xaxb", "yaxb")
  )
}


#
# x/ylim helpers ----
#

# Resolve a user-supplied x/ylim that may be a scalar or contains a single NA.
# `lim`  : raw user value (already known to be non-NULL)
# `drng` : data range, 2-element numeric, i.e. range(..., finite = TRUE)
# Returns a 2-element numeric vector (raw; window padding applied downstream).
resolve_lim = function(lim, drng, arg = "xlim") {
  if (!is.numeric(lim)) {
    stop(sprintf("`%s` must be numeric (a scalar or length-2 vector).", arg), call. = FALSE)
  }
  n = length(lim)
  if (n == 1L) {
    if (is.na(lim)) stop(sprintf("`%s` cannot be a single `NA`.", arg), call. = FALSE)
    # scalar: ensure the value is covered alongside the data
    return(range(c(drng, lim)))
  }
  if (n == 2L) {
    nas = is.na(lim)
    if (all(nas)) {
      stop(sprintf("`%s` cannot be `c(NA, NA)`; supply at least one finite limit.", arg), call. = FALSE)
    }
    if (!any(nas)) return(lim) # full override: unchanged (reversed axis OK)
    # exactly one NA: fill that side from the data range, pin the other verbatim
    if (nas[1L]) lim[1L] = drng[1L] else lim[2L] = drng[2L]
    return(lim)
  }
  stop(sprintf("`%s` must be length 1 or 2, not length %d.", arg, n), call. = FALSE)
}

# Resolve an axis-reversal keyword passed to x/ylim, e.g. xlim = "reverse" (or
# the "rev" abbreviation). Returns a list with the (possibly NULL-ified) limit
# and a logical flag. When a keyword is detected we strip the limit back to NULL
# so that all the usual auto-range machinery (data range, breaks, free facets,
# type-specific limits) runs untouched; the flag is consumed later to rev() the
# finalized range.
sanitize_lim_rev = function(lim, arg = "xlim") {
  if (is.character(lim)) {
    rev_ok = length(lim) == 1L && !is.na(lim) &&
      nchar(lim) >= 3L && pmatch(tolower(lim), "reverse", nomatch = 0L) == 1L
    if (!rev_ok) {
      stop(sprintf('If `%s` is a character string it must be "reverse" (or "rev").', arg), call. = FALSE)
    }
    return(list(lim = NULL, rev = TRUE))
  }
  list(lim = lim, rev = FALSE)
}
