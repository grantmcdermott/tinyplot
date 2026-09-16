#' @rdname type_abline
#' @param h y-value(s) for horizontal line(s). Numeric of length 1, or equal to
#'   the number of groups or number of facets (or the product thereof).
#' @export
type_hline = function(h = 0) {
  ablines_type(h = h, name = "hline")
}
