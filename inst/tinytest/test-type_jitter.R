source("helpers.R")
using("tinysnapshot")

f = function() {
  set.seed(42)
  tinyplot(Temp ~ Month | ordered(Month), airquality, type = "j", pch = 16)
}
expect_snapshot_plot(f, label = "type_j")

f = function() {
  set.seed(42)
  tinyplot(Species ~ Sepal.Length, data = iris, type = "j")
}
expect_snapshot_plot(f, label = "type_j_y")

f = function() {
  set.seed(42)
  data("airquality")
  airquality$Year = 1973
  airquality$Date = as.Date(paste0(
    airquality$Year,
    "-",
    airquality$Month,
    "-01"
  ))
  tinyplot(Temp ~ Date, data = airquality, type = "j", pch = 16)
}
expect_snapshot_plot(f, label = "type_j_date")
