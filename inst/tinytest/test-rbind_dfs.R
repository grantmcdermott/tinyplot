source("helpers.R")

## rbind_dfs() must reproduce do.call(rbind, ...) exactly (bar row names),
## both on its fast path and when it falls back to rbind
rbind_dfs = tinyplot:::rbind_dfs
ref = function(l) {
  out = do.call(rbind, unname(Filter(Negate(is.null), l)))
  if (!is.null(out)) rownames(out) = NULL
  out
}
d = function(...) data.frame(..., stringsAsFactors = FALSE)
lvls = c("a", "b")

cases = list(
  numeric = list(a = d(x = 1:2, y = c(.1, .2)), b = d(x = 3L, y = .3)),
  factor_union = list(d(g = factor(c("b", "a"))), d(g = factor(c("c", "a")))),
  factor_character = list(d(g = factor("a")), d(g = "z")),
  ordered = list(d(g = ordered("a", lvls)), d(g = ordered("b", lvls))),
  ordered_mixed = list(d(g = ordered("a")), d(g = ordered("b"))),
  date = list(d(x = as.Date("2020-01-01") + 0:1), d(x = as.Date("2021-01-01"))),
  posixct = list(
    d(x = as.POSIXct("2020-01-01", tz = "UTC")),
    d(x = as.POSIXct("2020-06-01", tz = "UTC"))
  ),
  zero_rows = list(
    d(x = numeric(0), g = factor(character(0), lvls)),
    d(x = 1, g = factor("b", lvls))
  ),
  nulls = list(NULL, d(x = 1), NULL, d(x = 2)),
  name_order = list(d(x = 1, y = 2), d(y = 3, x = 4))
)
for (nm in names(cases)) {
  expect_identical(rbind_dfs(cases[[nm]]), ref(cases[[nm]]), info = nm)
}
expect_null(rbind_dfs(list()))
