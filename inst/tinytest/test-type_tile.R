source("helpers.R")
using("tinysnapshot")

# shared fixture: correlation matrix of the base `attitude` dataset, in the same
# "long" form used by the type_tile() examples
catt = as.data.frame(as.table(cor(attitude)), responseName = "Correlation")

# "tile" and type_tile() are aliases and must produce an identical plot. Note
# that "heatmap" / type_heatmap() are *not* interchangeable with these, since
# they additionally reverse the y-axis (see the type_heatmap() section below).
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt, type = "tile",
    theme = "heatmap"
  )
}
expect_snapshot_plot(f, label = "tile_basic")

# type_tile() constructor (should be identical to the above)
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt, type = type_tile(),
    theme = "heatmap"
  )
}
expect_snapshot_plot(f, label = "tile_basic")

# fancy version, including gridded spacing and added labels
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt,
    type = type_tile(width = 0.9, height = 0.9),
    theme = "heatmap",
    col = "white",
    legend = FALSE,
    main = "Correlation matrix of base attitude dataset",
    xlab = NA, ylab = NA,
    ylim = "rev"
  )
  tinyplot_add(type = "text", labels = round(catt$Correlation, 2))
}
expect_snapshot_plot(f, label = "tile_fancy")

# numeric axes: no factor conversion, ticks stay numeric
volc = data.frame(
  x         = as.vector(row(volcano)),
  y         = as.vector(col(volcano)),
  elevation = as.vector(volcano)
)
f = function() {
  tinyplot(
    y ~ x | elevation, data = volc,
    type = "tile",
    theme = "void",
    xlab = NA, ylab = NA,
    main = "Maunga Whau volcano"
  )
}
expect_snapshot_plot(f, label = "tile_numeric_axes")

# faceting: categorical tick labels must survive on both axes in every panel
d = expand.grid(
  a = factor(c("x", "y", "z")),
  b = factor(c("p", "q")),
  g = factor(c("G1", "G2"))
)
d$v = seq_len(nrow(d))
f = function() {
  tinyplot(b ~ a | v, facet = ~g, data = d, type = "tile", theme = "heatmap")
}
expect_snapshot_plot(f, label = "tile_facet")


#
## type_heatmap(): scale/method, cf. base R `heatmap(scale=)`
#

# A raw data matrix is the motivating case: unscaled, `disp`/`hp` monopolise the
# colour ramp and the other nine columns are indistinguishable.
mt = as.data.frame(as.table(as.matrix(mtcars)))

f = function() {
  tinyplot(
    Var1 ~ Var2 | Freq, data = mt,
    type = type_heatmap(scale = "x"),
    theme = "heatmap",
    xlab = NA, ylab = NA
  )
}
expect_snapshot_plot(f, label = "heatmap_scale_x")

# `method = "rescale"` is the alternative to the z-score default
f = function() {
  tinyplot(
    Var1 ~ Var2 | Freq, data = mt,
    type = type_heatmap(scale = "x", method = "rescale"),
    theme = "heatmap",
    xlab = NA, ylab = NA
  )
}
expect_snapshot_plot(f, label = "heatmap_scale_x_rescale")

# `scale = "none"` is the default, so a bare type_heatmap() applies no rescaling.
# It does still reverse the y-axis, so pinning `ylim` back to normal is what
# recovers the plain type_tile() fixture exactly (asserted against that label).
f = function() {
  tinyplot(
    Var1 ~ Var2 | Correlation, data = catt, type = type_heatmap(scale = "none"),
    theme = "heatmap", ylim = c(0.5, 7.5)
  )
}
expect_snapshot_plot(f, label = "tile_basic")
