source("helpers.R")
using("tinysnapshot")

# tinyplot.matrix() method

# basic: named columns -> grouped, named rows -> x-axis tick labels
f = function() tinyplot(VADeaths)
expect_snapshot_plot(f, label = "matrix_basic")

# row names should label the x-axis for line types too (not just points), and
# the "Index" title is dropped when row names are present (#645-adjacent fix)
f = function() tinyplot(VADeaths, type = "b")
expect_snapshot_plot(f, label = "matrix_type_b")

# faceting by column
f = function() tinyplot(VADeaths, type = "o", facet = "by")
expect_snapshot_plot(f, label = "matrix_facet")
