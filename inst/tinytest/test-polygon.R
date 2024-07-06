source("helpers.R")
using("tinysnapshot")

i = seq(0, 2*pi, by = 0.01)
x = 16*sin(i)^3
y = 13*cos(i) - 5*cos(2*i) - 2*cos(3*i) - cos(4*i)

f = function() {
  plt(
    x, y,
    type = "polygon",
    col = "red", fill = adjustcolor("firebrick", 0.8),
    axes = FALSE, xlab = NA, ylab = NA
  )
}
expect_snapshot_plot(f, label = "polygon_heart")

#
## polygon path
 
# (Note: these tests adapted from the ?polypath examples) 

for (rrule in c("winding", "evenodd")) {
  
  f = function() {
    plt(
      c(.1, .1, .9, .9, NA, .2, .2, .8, .8),
      c(.1, .9, .9, .1, NA, .2, .8, .8, .2),
      main = "Nested rectangles, both clockwise",
      type = "polypath",
      rule = rrule,
      fill = "grey",
      axes = FALSE, xlab = NA, ylab = NA
    )
    mtext(paste0("Rule:", rrule))
  }
  expect_snapshot_plot(f, label = paste0("polypath_nested_cc_", rrule))
  
  f = function() {
    plt(
      c(.1, .1, .9, .9, NA, .2, .8, .8, .2),
      c(.1, .9, .9, .1, NA, .2, .2, .8, .8),
      main = "Nested rectangles, outer clockwise, inner anti-clockwise",
      type = "polypath",
      rule = rrule,
      fill = "grey",
      axes = FALSE, xlab = NA, ylab = NA
    )
    mtext(paste0("Rule:", rrule))
  }
  expect_snapshot_plot(f, label = paste0("polypath_nested_ca_", rrule))
  
  f = function() {
    plt(
      c(.1, .1, .4, .4, NA, .6, .9, .9, .6),
      c(.1, .4, .4, .1, NA, .6, .6, .9, .9),
      main = "Disjoint rectangles",
      type = "polypath",
      rule = rrule,
      fill = "grey",
      axes = FALSE, xlab = NA, ylab = NA
    )
    mtext(paste0("Rule:", rrule))
  }
  expect_snapshot_plot(f, label = paste0("polypath_disjoint_", rrule))
  
  f = function() {
    plt(
      c(.1, .1, .6, .6, NA, .4, .4, .9, .9),
      c(.1, .6, .6, .1, NA, .4, .9, .9, .4),
      main = "Overlapping rectangles, both clockwise",
      type = "polypath",
      rule = rrule,
      fill = "grey",
      axes = FALSE, xlab = NA, ylab = NA
    )
    mtext(paste0("Rule:", rrule))
  }
  expect_snapshot_plot(f, label = paste0("polypath_overlapping_cc_", rrule))
  
  f = function() {
    plt(
      c(.1, .1, .6, .6, NA, .4, .9, .9, .4),
      c(.1, .6, .6, .1, NA, .4, .4, .9, .9),
      main = "Overlapping rectangles, one clockwise, other anti-clockwise",
      type = "polypath",
      rule = rrule,
      fill = "grey",
      axes = FALSE, xlab = NA, ylab = NA
    )
    mtext(paste0("Rule:", rrule))
  }
  expect_snapshot_plot(f, label = paste0("polypath_overlapping_ca_", rrule))
  
  
}
rm(rrule)

