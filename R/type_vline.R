#' @param v x-value(s) for vertical line(s). Numeric of length 1, or equal to
#'   the number of groups or number of facets (or the product thereof).
#' @rdname type_abline
#' @export
type_vline = function(v = 0) {
  ablines_type(v = v, name = "vline")
}
