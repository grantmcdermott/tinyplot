library(tinyplot)

## a continuous `by` variable colours the line itself, rather than splitting
## it into groups
time = seq(0, 10*pi, length.out = 800)
spiral = data.frame(time = time, x = time * cos(time), y = time * sin(time))

plt(
  y ~ x | time,
  data = spiral,
  type = "l",
  lwd = 3,
  asp = 1,
  xlab = NA, ylab = NA,
  main = "Archimedean spiral",
  theme = "clean"
)
