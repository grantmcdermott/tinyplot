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
# Non-numeric input (e.g. categorical axis labels reaching a numeric labeller
# via flip = TRUE) should be returned unchanged, without "NAs introduced by
# coercion" warnings (#622).
spp = c("Adelie", "Gentoo", "Chinstrap")
expect_equal(tinylabel(spp, ","), spp)

# `xaxl`/`yaxl` accept a dictionary mapping old labels to new ones. Unmatched
# labels are left alone, the mapping is by value rather than position (so it
# survives reordering), and a named list works as well as a named vector.
expect_equal(tinylabel(c("a", "b", "c"), c(b = "Bee")), c("a", "Bee", "c"))
expect_equal(tinylabel(c("c", "a", "b"), c(a = "A", b = "B", c = "C")), c("C", "A", "B"))
expect_equal(tinylabel(c("a", "b"), list(a = "Alpha", b = "Beta")), c("Alpha", "Beta"))

# names remove the keyword collision that a bare vector could never resolve
expect_equal(tinylabel(c("log", "x"), c(log = "Log scale")), c("Log scale", "x"))

# an unnamed multi-element vector is not positional replacement
expect_error(tinylabel(c("a", "b"), c("X", "Y")), pattern = "single formatting keyword")

# "abs_" must be stripped before a symbol keyword is resolved; a centered
# barplot prepends it itself, which previously made `yaxl = ","` unusable there
expect_equal(tinylabel(c(-1000, 2000), "abs_,"), c("1,000", "2,000"))
expect_equal(tinylabel(c(-1000, 2000), "abs_comma"), c("1,000", "2,000"))


#
## symmetric breaks must not blow up the precision -----

# A centered barplot's breaks are symmetric, so the "abs_" wrapper hands the
# formatter genuine duplicates -- but abs(-0.4) * 100 and 0.4 * 100 differ in
# their last bits, so unique() keeps both. consistent_decimals() was then asked
# for a precision that prints two identical numbers distinctly, failed at every
# candidate, and fell back to its 5-decimal maximum ("80.00000%" not "80%").
# seq() rather than typed-out constants: literals are exact, so abs() maps them
# onto bit-equal values that unique() collapses cleanly and the bug never fires.
# Real axis breaks are computed, and carry the noise that defeats unique(). The
# guard below asserts that condition holds, so this cannot quietly go vacuous.
brks = seq(-0.8, 0.8, by = 0.2)
expect_true(
  length(unique(abs(brks) * 100)) > length(unique(round(abs(brks) * 100, 5)))
)
expect_equal(
  tinylabel(brks, "abs_percent"),
  c("80%", "60%", "40%", "20%", "0%", "20%", "40%", "60%", "80%")
)

# ...while precision still adapts where the values genuinely need it
expect_equal(tinylabel(c(0.00011, 0.00012), "percent"), c("0.011%", "0.012%"))

# The deduplication runs at max_decimals rather than at a fixed number of
# significant digits: significance is relative while max_decimals is absolute,
# so any signif() threshold merges values still distinguishable at the 5th
# decimal once they grow large enough. This case fails under signif(., 6),
# (., 8) and (., 12) alike.
expect_equal(
  tinylabel(c(10000000.00001, 10000000.00002), "comma"),
  c("10,000,000.00001", "10,000,000.00002")
)

# ...and end to end, since the axis is where it surfaced: a centered stacked
# barplot is the simplest thing that produces breaks symmetric about zero
props = data.frame(
  g   = factor(rep(c("a", "b"), each = 2)),
  grp = factor(rep(c("up", "down"), 2)),
  v   = c(0.6, 0.4, 0.3, 0.7)
)
f = function() {
  tinyplot(
    v ~ g | grp, data = props,
    type = type_barplot(center = TRUE), yaxl = "percent",
    flip = TRUE
  )
}
expect_snapshot_plot(f, label = "tinylabel_center_percent")
