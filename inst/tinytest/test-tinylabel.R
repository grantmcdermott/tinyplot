source("helpers.R")
using("tinysnapshot")

s77 = transform(data.frame(state.x77), Illiteracy = Illiteracy / 100)

f = function() tinyplot(
    Life.Exp ~ Income | Illiteracy, data = s77,
    xaxl = '$',
    legend = list(labeller = '%'),
    theme = "clean"
)
expect_snapshot_plot(f, label = "tinylabel")

# Currency/comma formatters should use a consistent number of decimal places
# across the whole vector (#618). Currency formatters additionally follow the
# convention of showing at least two decimal places when any fractional
# component is present, while still keeping clean integers integer-valued.
revenue = seq(0, 2.5, length.out = 6)
expect_equal(
  tinylabel(revenue, "$"),
  c("$0.00", "$0.50", "$1.00", "$1.50", "$2.00", "$2.50")
)
# comma is not currency, so it uses the minimal consistent decimals
expect_equal(
  tinylabel(revenue, ","),
  c("0.0", "0.5", "1.0", "1.5", "2.0", "2.5")
)
# clean integers stay integer-valued
expect_equal(
  tinylabel(c(1000, 2000, 3000), "$"),
  c("$1,000", "$2,000", "$3,000")
)
expect_equal(
  tinylabel(c(1000, 2000, 3000), ","),
  c("1,000", "2,000", "3,000")
)
# NA values are left as-is by default (na.ignore = TRUE)
expect_equal(
  tinylabel(c(0, 0.5, NA, 1.5), "$"),
  c("$0.00", "$0.50", NA, "$1.50")
)
# negative currency values place the sign in front of the symbol
expect_equal(
  tinylabel(c(-1.5, 0, 2), "$"),
  c("-$1.50", "$0.00", "$2.00")
)
expect_equal(
  tinylabel(c(-1000, 2000), "$"),
  c("-$1,000", "$2,000")
)
