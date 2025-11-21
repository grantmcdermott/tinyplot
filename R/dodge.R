#' Dodge positions for grouped data
#'
#' Adjusts x-coordinates (and optionally xmin/xmax) to dodge overlapping points
#' or ranges in grouped plots.
#'
#' @param datapoints Data frame containing plot data with at least `x` and `by`
#'   columns.
#' @param dodge Numeric value specifying the dodge amount. If 0, no dodging is
#'   performed.
#' @param fixed.pos Logical. If `TRUE`, dodge positions are fixed based on the
#'   number of groups in `by`. If `FALSE`, dodge positions are calculated
#'   separately for each unique x value.
#' @param cols Character vector of column names to dodge. If `NULL` (default),
#'   automatically detects and dodges `x`, `xmin`, and `xmax` if they exist.
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
  cols = NULL
) {
  if (dodge == 0) {
    return(datapoints)
  }
  
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
