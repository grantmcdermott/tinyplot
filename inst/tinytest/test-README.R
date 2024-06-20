source("helpers.R")
using("tinysnapshot")

aq = airquality
aq$Month = as.factor(aq$Month)

op = tpar()

f = function() {
  tpar(mfrow = c(1, 2))
  
  plot(0:10, main = "plot")
  tinyplot(0:10, main = "tinyplot")
}
expect_snapshot_plot(f, label = "readme_base_1")

f = function() {
  tpar(mfrow = c(2, 2))
  
  with(aq, plot(Day, Temp, main = "plot"))
  plot(Temp ~ Day, data = aq, main = "plot (formula)")
  with(aq, tinyplot(Day, Temp, main = "tinyplot"))
  tinyplot(Temp ~ Day, data = aq, main = "tinyplot (formula)")
  
}
expect_snapshot_plot(f, label = "readme_base_2")

#
# restore original par settings (NB)
#

tpar(op)

#
# continue with tests
#

f = function() with(aq, tinyplot(Day, Temp, by = Month))
expect_snapshot_plot(f, label = "readme_by")

f = function() tinyplot(Temp ~ Day | Month, data = aq)
expect_snapshot_plot(f, label = "readme_formula")

f = function() {
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    pch = 16
  )
}
expect_snapshot_plot(f, label = "readme_pch_16")

f = function() {
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "l"
  )
}
expect_snapshot_plot(f, label = "readme_type_l")

f = function() {
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "l",
    col = "black", # override automatic group colours
    lty = "by"     # change line type by group instead
  )
} 
expect_snapshot_plot(f, label = "readme_by_lty")

f = function() {
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "l",
    legend = legend("bottom!", title = "Month of the year", bty = "o")
  )
}
expect_snapshot_plot(f, label = "readme_legend_bottom")

if ((getRversion() <= "4.3.1")) {
  f = function() {
    with(
      aq,
      tinyplot(density(Temp), by = Month, legend = legend("topright", bty="o"))
    )
  }
  expect_snapshot_plot(f, label = "readme_density_topright")
}

f = function() {
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "l",
    palette = "Tableau 10"
  )
}
expect_snapshot_plot(f, label = "readme_palette_tableau")

f = function() {
  mod = lm(Temp ~ 0 + factor(Month), aq)
  coefs = data.frame(names(coef(mod)), coef(mod), confint(mod))
  coefs = setNames(coefs, c("term", "estimate", "ci_low", "ci_high"))
  with(
    coefs,
    tinyplot(
      x = term, y = estimate,
      ymin = ci_low, ymax = ci_high,
      type = "pointrange",
      pch = 19,
      main = "Effect on Temperature"
    )
  )
}
expect_snapshot_plot(f, label = "readme_pointrange")

f = function() {
  tpar(pch = 16, family = "HersheySans")
  
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "b",
    # palette = palette.colors(palette = "Tableau 10", alpha = 0.5),
    palette = "Tableau 10", alpha = 0.5,
    main = "Daily temperatures by month",
    frame = FALSE, grid = TRUE
  )
}
expect_snapshot_plot(f, label = "readme_hershey_plus")


#
# restore original par settings (NB)
#

tpar(op)

#
# continue with tests
#

exit_if_not(length(find.package("basetheme", quiet=TRUE)) > 0)
library(basetheme)
basetheme("royal") # or "clean", "dark", "ink", "brutal", etc.

f = function() {
  
  tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "b", pch = 15:19,
    palette = "Tropic",
    main = "Daily temperatures by month"
  )
  
}
expect_snapshot_plot(f, label = "readme_basetheme_royal")

# back to default theme
basetheme(NULL)
# dev.off()
