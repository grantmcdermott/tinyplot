source("helpers.R")
using("tinysnapshot")

ucb = as.data.frame(UCBAdmissions)

dat = expand.grid(year = 2000:2020, grp = factor(c("A", "B", "C")))
dat$val = abs(sin(dat$year / 3) + as.integer(dat$grp)) + 1


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
