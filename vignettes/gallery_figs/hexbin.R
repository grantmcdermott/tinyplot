library(tinyplot)

set.seed(1234)
dat = data.frame(x = rnorm(2e4), y = rnorm(2e4))

plt(
  y ~ x, data = dat, type = "hexbin",
  palette = hcl.colors(100, palette = "agSunset", rev = TRUE),
  theme = "clean2"
)
