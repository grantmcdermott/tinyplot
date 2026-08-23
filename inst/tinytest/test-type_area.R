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


#
## bylevels -----

f = function() {
  tinyplot(val ~ year | grp, data = dat, type = type_area(stack = TRUE, bylevels = "end"))
}
expect_snapshot_plot(f, label = "area_stack_bylevels_end")

# repeated cells must be collapsed *before* `bylevels` ranks them, or the
# ranking sorts on raw per-cell sums rather than the bands actually drawn
f = function() {
  tinyplot(weight ~ Time | Diet, ChickWeight,
           type = type_area(stack = TRUE, bylevels = "end"))
}
expect_snapshot_plot(f, label = "area_stack_bylevels_aggregated")


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
