source("helpers.R")
using("tinysnapshot")

ucb = as.data.frame(UCBAdmissions)

# group B is small and steady; A and C are larger and wobblier, so that the
# default level order, the size keywords, and "minvar" each pick a different
# bottom band
dat = expand.grid(year = 2000:2020, grp = factor(c("A", "B", "C")))
dat$val = as.integer(dat$grp) +
  c(1.2, 0.1, 1.8)[dat$grp] * sin(dat$year / 3) +
  c(0.06, 0.02, 0.10)[dat$grp] * (dat$year - 2000)


#
## stacked areas -----

f = function() {
  tinyplot(
    Freq ~ Dept | Admit,
    data = ucb,
    facet = ~Gender, facet.args = list(ncol = 1),
    type = type_area(stack = TRUE),
    frame = FALSE
  )
}
expect_snapshot_plot(f, label = "area_stack_facet")

f = function() {
  tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE))
}
expect_snapshot_plot(f, label = "area_stack")

# stacked bands are opaque by default, but `alpha` still wins
f = function() {
  tinyplot(
    val ~ year | grp, data = dat,
    type = type_area(stack = TRUE, alpha = 0.4)
  )
}
expect_snapshot_plot(f, label = "area_stack_alpha")

f = function() {
  tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE), flip = TRUE)
}
expect_snapshot_plot(f, label = "area_stack_flip")

# facets with different x coverage must not inherit each other's x positions,
# or the bands ramp to zero across a range the facet never spanned
f = function() {
  d = dat
  d$half = ifelse(d$year < 2010, "early", "late")
  tinyplot(
    val ~ year | grp, data = d,
    facet = ~half, facet.args = list(ncol = 1),
    type = type_area(stack = TRUE)
  )
}
expect_snapshot_plot(f, label = "area_stack_facet_ragged")

# horizontal legends pad every label but the rightmost, so the key has to be
# reversed before that padding is applied
f = function() {
  tinyplot(
    val ~ year | grp, data = dat,
    type = type_area(stack = TRUE), legend = "bottom!"
  )
}
expect_snapshot_plot(f, label = "area_stack_legend_bottom")


#
## byord -----

f = function() {
  tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE, byord = "end"))
}
expect_snapshot_plot(f, label = "area_stack_byord_end")

# repeated cells must be collapsed *before* `byord` ranks them, or the
# ranking sorts on raw per-cell sums rather than the bands actually drawn
f = function() {
  tinyplot(weight ~ Time | Diet, ChickWeight,
           type = type_area(stack = TRUE, byord = "end"))
}
expect_snapshot_plot(f, label = "area_stack_byord_aggregated")


#
## unstacked areas -----

f = function() {
  tinyplot(val ~ year | grp, data = dat, type = "area")
}
expect_snapshot_plot(f, label = "area_grouped")

# categorical x should be labelled with its factor levels
f = function() {
  tinyplot(Freq ~ Dept, data = ucb[ucb$Admit == "Admitted" & ucb$Gender == "Male", ], type = "area")
}
expect_snapshot_plot(f, label = "area_factor_x")


# "minvar" ranks the other way to the size keywords -- least variable onto the
# baseline -- so it must pick a different bottom band here than "end" does
f = function() {
  tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE, byord = "minvar"))
}
expect_snapshot_plot(f, label = "area_stack_byord_minvar")

# "rev" flips the existing level order; unlike every other byord input it
# consults no data, so it also works when `y` is absent or non-numeric
f = function() {
  tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE, byord = "rev"))
}
expect_snapshot_plot(f, label = "area_stack_byord_rev")

# a ranking function that names an `x` argument receives the group's x values,
# without which a slope cannot be computed against uneven spacing
f = function() {
  tinyplot(val ~ year | grp, data = dat,
           type = type_area(stack = TRUE, byord = function(y, x) coef(lm(y ~ x))[2]))
}
expect_snapshot_plot(f, label = "area_stack_byord_fun_x")

#
## byord rejects explicit levels -----

# explicit level order belongs to factor(levels = ), not `byord`; accepting it
# here would collapse the distinction between the two vocabularies
expect_error(
  tinyplot(val ~ year | grp, data = dat,
           type = type_area(stack = TRUE, byord = c("C", "A", "B"))),
  pattern = "must be NULL"
)
expect_error(
  tinyplot(val ~ year | grp, data = dat,
           type = type_area(stack = TRUE, byord = 3:1)),
  pattern = "must be NULL"
)

# a one-argument function keeps working unchanged, and a second argument that
# is *not* named `x` (e.g. a tuning parameter with a default) must not be fed
# the x values by mistake
dp = function(byord) {
  d = data.frame(x = rep(1:4, 2), y = c(1, 2, 3, 100, 4, 4, 4, 4),
                 by = factor(rep(c("a", "b"), each = 4)), facet = "f")
  levels(tinyplot:::sanitize_ord(d$by, d$y, d$x, byord))
}
expect_equal(dp(function(y) -median(y)), c("b", "a"))
expect_equal(dp(function(y, p = 0.9) -as.numeric(quantile(y, p))), c("a", "b"))
expect_equal(dp(function(y, x) coef(lm(y ~ x))[2]), c("b", "a"))
