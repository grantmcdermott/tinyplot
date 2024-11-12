#' Add straight lines to a plot
#'
#' @inheritParams graphics::abline
#' @inheritParams tinyplot
#' @examples
#' mod = lm(mpg ~ hp, data = mtcars)
#' y = mtcars$mpg
#' yhat = predict(mod)
#' tinyplot(y, yhat, xlim = c(0, 40), ylim = c(0, 40))
#' tinyplot_add(type = type_abline(a = 0, b = 1))
#' @export
type_abline = function(a = 0, b = 1, col = "black", lty = 1, lwd = 1) {
  assert_numeric(a, len = 1)
  assert_numeric(b, len = 1)
  assert_true(length(col) == 1)
  assert_true(length(lty) == 1)
  assert_true(length(lwd) == 1)
  draw_abline = function() {
    fun = function(...) {
      abline(a = a, b = b, col = col, lty = lty, lwd = lwd)
    }
    return(fun)
  }
  out = list(
    draw = draw_abline(),
    data = NULL,
    name = "abline"
  )
  class(out) = "tinyplot_type"
  return(out)
}

