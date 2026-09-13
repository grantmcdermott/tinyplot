source("helpers.R")

# No snapshots here: every assertion below is a coordinate, so it can be checked
# exactly rather than compared as an image. One sink device for the whole file --
# par("usr") needs a device open, but nothing needs to look at what is drawn, and
# opening one per call costs ~40x more than reusing this one.
pdf(NULL)


# `xpad`/`ypad` take over the axis expansion that base R hardcodes at 4%.
usr_for = function(...) {
  tinyplot(y ~ x, data = data.frame(x = c(1, 10), y = c(1, 10)), ...)
  par("usr")
}

# Left alone, nothing changes: base still applies its own 4%.
expect_equal(usr_for()[1:2], c(0.64, 10.36))

# 0 fits the axis tightly to the data; a larger value widens it proportionally.
expect_equal(usr_for(xpad = 0)[1:2], c(1, 10))
expect_equal(usr_for(xpad = 0.2)[1:2], c(-0.8, 11.8))

# Each axis is independent: `ypad` tightens y and leaves x on base's default.
expect_equal(usr_for(ypad = 0), c(0.64, 10.36, 1, 10))

# The padding follows its variable across a flip, the way `xlim` and `log` do,
# so it lands on whichever axis the x variable ended up on.
expect_equal(usr_for(xpad = 0, flip = TRUE)[3:4], c(1, 10))

# A logged axis is expanded in log space, not on the raw values. The pad has to
# be non-zero to tell the two apart: raw-scale padding of log10(c(1, 10)) would
# give c(0.1, 10.9) on the untransformed data, i.e. usr c(-1, 1.037).
expect_equal(usr_for(log = "x", xpad = 0.1)[1:2], c(-0.1, 1.1))

# tpar() sets the default; an explicit argument still wins.
tpar(xpad = 0)
expect_equal(usr_for()[1:2], c(1, 10))
expect_equal(usr_for(xpad = 0.2)[1:2], c(-0.8, 11.8))
tpar(xpad = NULL)
expect_equal(usr_for()[1:2], c(0.64, 10.36))

# A flipped boxplot is the one type flip_datapoints() leaves alone: it hands
# ylim to the physical x axis and xlim to the physical y instead of swapping
# the pair. The pad still belongs to its own variable, so `xpad` has to reach
# the categorical axis -- which lands on y -- rather than the numeric one.
box_usr = function(...) {
  d = data.frame(g = factor(rep(c("a", "b", "c"), 4)), y = rep(c(1, 5), 6))
  tinyplot(y ~ g, data = d, type = "boxplot", flip = TRUE, ...)
  par("usr")
}
expect_equal(box_usr(xpad = 0)[3:4], c(0.5, 3.5))   # categorical, tight
expect_equal(box_usr()[3:4], c(0.38, 3.62))         # categorical, base's 4%

# Free facets derive a range per panel, so the padding has to reach them too.
fusr_for = function(...) {
  d = data.frame(x = c(1, 10, 1, 10), y = c(1, 10, 2, 20), f = c("a", "a", "b", "b"))
  tinyplot(y ~ x, facet = ~f, data = d, facet.args = list(free = TRUE), ...)
  tinyplot:::get_environment_variable(".fusr")[[1]]
}
expect_equal(fusr_for(xpad = 0)[1:2], c(1, 10))
expect_equal(fusr_for()[1:2], c(0.64, 10.36))

# A negative pad would crop the data rather than pad it -- past -0.5 it
# collapses the range, and at -1 it silently reverses the axis -- so it is
# refused. There is no upper bound: a large pad is just a zoomed-out plot.
expect_error(usr_for(xpad = -0.1), pattern = "greater than or equal to 0")
expect_error(usr_for(ypad = -1), pattern = "greater than or equal to 0")
expect_error(usr_for(xpad = "a"), pattern = "must be numeric")
expect_error(usr_for(xpad = c(0.1, 0.2)), pattern = "length must be 1")
expect_equal(usr_for(xpad = 3)[1:2], c(-26, 37))

# The same checks apply when the value arrives via tpar() rather than the arg.
# NB: tpar() assigns before it validates, so a rejected value sticks and would
# poison every later call (see test-record.R). Reset each one explicitly.
expect_error(tpar(xpad = "a"), pattern = "xpad")
tpar(xpad = NULL)
expect_error(tpar(ypad = -1), pattern = "ypad")
tpar(ypad = NULL)

dev.off()
