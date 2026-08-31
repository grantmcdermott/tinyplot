source("helpers.R")
using("tinysnapshot")

#
## dodging

f = function() {
  tinyplot(
    len ~ dose | supp, data = ToothGrowth,
    type = type_summary(type = "b", dodge = TRUE),
    main = "dodged summary"
  )
}
expect_snapshot_plot(f, label = "summary_dodge")