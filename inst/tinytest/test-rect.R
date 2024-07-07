source("helpers.R")
using("tinysnapshot")

f = function() {
  i = 4*(0:10)
  plt(
    xmin = 100+i,
    ymin = 300+i,
    xmax = 150+i,
    ymax = 380+i,
    type = "rect"
  )
}
expect_snapshot_plot(f, label = "rect_simple")

f = function() {
  i = 4*(0:10)
  plt(
    xmin = 100+i,
    ymin = 300+i,
    xmax = 150+i,
    ymax = 380+i,
    by = i,
    type = "rect",
    fill = 0.2,
    palette = "agsunset"
  )
}
expect_snapshot_plot(f, label = "rect_by_fill")

