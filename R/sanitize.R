sanitize_ribbon.alpha = function(ribbon.alpha) {
  assert_numeric(ribbon.alpha, len = 1, lower = 0, upper = 1, null.ok = TRUE)
  if (is.null(ribbon.alpha)) ribbon.alpha = .tpar[["ribbon.alpha"]]
  return(ribbon.alpha)
}
