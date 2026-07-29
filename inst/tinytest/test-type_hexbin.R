source("helpers.R")
using("tinysnapshot")

set.seed(1234)
dat = data.frame(x = rnorm(20000), y = rnorm(20000))

# basic hexbin: fill encodes the cell count (continuous colourbar legend)
f = function() tinyplot(y ~ x, data = dat, type = "hexbin")
expect_snapshot_plot(f, label = "type_hexbin")

# discrete `by`: fill encodes the modal level per cell (discrete legend)
dat$g = cut(dat$x, breaks = c(-Inf, -1, 1, Inf), labels = c("lo", "mid", "hi"))
f = function() tinyplot(y ~ x | g, data = dat, type = "hexbin")
expect_snapshot_plot(f, label = "type_hexbin_discrete")
