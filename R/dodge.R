#' Dodge positions for grouped data
#'
#' Adjusts x-coordinates (and optionally xmin/xmax) to dodge overlapping points
#' or ranges in grouped plots.
#'
#' @param datapoints Data frame containing plot data with at least `x` and `by`
#'   columns.
#' @param dodge Numeric value in the range `[0,1)`, or logical. If numeric,
#'   values are scaled relative to x-axis break spacing (e.g., `dodge = 0.1`
#'   places outermost groups one-tenth of the way to adjacent breaks;
#'   `dodge = 0.5` places them midway between breaks; etc.). Values < 0.5 are
#'   recommended. If `TRUE`, dodge width is calculated automatically based on
#'   the number of groups (0.1 per group for 2-4 groups, 0.45 for 5+ groups). If
#'   `FALSE` or 0, no dodging is performed. Default is 0.
#' @param fixed.pos Logical indicating whether dodged groups should retain a
#'   fixed relative position based on their group value. Relevant for `x`
#'   categories that only have a subset of the total number of groups. Defaults
#'   to `FALSE`, in which case dodging is based on the number of unique groups
#'   present in that `x` category alone. See Examples.
#' @param cols Character vector of column names to dodge. If `NULL` (default),
#'   automatically detects and dodges `x`, `xmin`, and `xmax` if they exist.
#' @param settings Environment containing plot settings. If `NULL` (default),
#'   retrieved from the calling environment.
#'
#' @return Modified `datapoints` data frame with dodged positions.
#'
#' @details
#' When `fixed.pos = TRUE`, all groups are dodged by the same amount across all
#' x values, which is useful when x is categorical. When `fixed.pos = FALSE`,
#' dodging is calculated independently for each x value, which is useful when
#' the number of groups varies across x values.
#'
#' If `cols` is not specified, the function automatically dodges `x` and any
#' `xmin`/`xmax` columns that exist in the data.
#'
#' @keywords internal
dodge_positions = function(
  datapoints,
  dodge,
  fixed.pos = TRUE,
  cols = NULL,
  settings = NULL
) {
  if (is.null(settings)) {
    settings = get("settings", envir = parent.frame())
  }
  
  if (is.logical(dodge)) {
    if (isTRUE(dodge)) {
      n = nlevels(datapoints$by)
      dodge = if (n == 1) 0 else if (n <= 5) (n - 1) * 0.1 else 0.45
    } else {
      dodge = 0
    }
  }

  assert_numeric(dodge, len = 1, lower = 0, upper = 1)
  if (dodge >= 1) {
  stop("`dodge` must be in the range [0,1).", call. = FALSE)
  }
  assert_logical(fixed.pos)
  
  if (dodge == 0) {
    return(datapoints)
  } else if (dodge > 0.5) {
    warning(
      "Argument `dodge = ", dodge, "` exceeds 0.5. ",
      "Large dodge values may position outer groups closer to neighboring axis breaks."
    )
  }
  settings$dodge = dodge
  
  # Auto-detect columns to dodge if not specified
  if (is.null(cols)) {
    cols = c("x", "xmin", "xmax")
    cols = cols[cols %in% names(datapoints)]
  }

  if (fixed.pos) {
    n = nlevels(datapoints$by)
    d = cumsum(rep(dodge, n))
    d = d - mean(d)
    x_adj = d[as.integer(datapoints$by)]
    for (col in cols) {
      datapoints[[col]] = datapoints[[col]] + x_adj
    }
  } else {
    xuniq = unique(datapoints$x)
    for (i in seq_along(xuniq)) {
      idx = which(datapoints$x == xuniq[i])
      n = length(idx)
      d = cumsum(rep(dodge, n))
      d = d - mean(d)
      for (col in cols) {
        datapoints[[col]][idx] = datapoints[[col]][idx] + d
      }
    }
  }

  datapoints
}

