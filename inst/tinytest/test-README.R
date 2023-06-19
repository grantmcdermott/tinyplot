source("helpers.R")
using("tinysnapshot")

op = par(no.readonly = TRUE)

f = function() {
  par(mfrow = c(1, 2))
  
  plot(0:10, main = "plot")
  plot2(0:10, main = "plot2")
}
expect_snapshot_plot(f, label = "readme_base_1")

f = function() {
  par(mfrow = c(2, 2))
  
  plot(airquality$Day, airquality$Temp, main = "plot")
  plot(Temp ~ Day, data = airquality, main = "plot (formula)")
  plot2(airquality$Day, airquality$Temp, main = "plot2")
  plot2(Temp ~ Day, data = airquality, main = "plot2 (formula)")
  
}
expect_snapshot_plot(f, label = "readme_base_2")

#
# restore original par settings (NB)
#

par(op)

#
# continue with tests
#

f = function() plot2(airquality$Day, airquality$Temp, by = airquality$Month)
expect_snapshot_plot(f, label = "readme_by")

f = function() plot2(Temp ~ Day | Month, data = airquality)
expect_snapshot_plot(f, label = "readme_formula")

f = function() {
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    pch = 16
  )
}
expect_snapshot_plot(f, label = "readme_pch_16")

f = function() {
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    type = "l"
  )
}
expect_snapshot_plot(f, label = "readme_type_l")

f = function() {
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    type = "l",
    col = "black", # override automatic group colours
    lty = "by"     # change line type by group instead
  )
} 
expect_snapshot_plot(f, label = "readme_by_lty")

f = function() {
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    type = "l",
    legend.position = "bottom!", # "right!" (default), "none", or "topleft", etc.
    legend.args = list(title = "Month of the year", bty = "o")
  )
}
expect_snapshot_plot(f, label = "readme_legend_bottom")

if ((getRversion() <= "4.3.1")) {
  f = function() {
    plot2(
      density(airquality$Temp),
      by = airquality$Month, 
      legend.position = "topright",
      legend.args = list(title = "Month", bty="o")
    )
  }
  expect_snapshot_plot(f, label = "readme_density_topright")
}

f = function() {
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    type = "l",
    palette = "Tableau 10"
  )
}
expect_snapshot_plot(f, label = "readme_palette_tableau")

f = function() {
  par(pch = 16, family = "HersheySans")
  
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    type = "b",
    palette = palette.colors(palette = "Tableau 10", alpha = 0.5),
    main = "Daily temperatures by month",
    frame.plot = FALSE, grid = grid()
  )
}
expect_snapshot_plot(f, label = "readme_hershey_plus")

#
# restore original par settings (NB)
#

par(op)

#
# continue with tests
#

exit_if_not(length(find.package("basetheme", quiet=TRUE)) > 0)
library(basetheme)
basetheme("royal") # or "clean", "dark", "ink", "brutal", etc.

f = function() {
  
  plot2(
    Temp ~ Day | Month,
    data = airquality,
    type = "b", pch = 15:19,
    palette = "Tropic",
    main = "Daily temperatures by month"
  )
  
}
expect_snapshot_plot(f, label = "readme_basetheme_royal")

# back to default theme
basetheme(NULL)
dev.off()
