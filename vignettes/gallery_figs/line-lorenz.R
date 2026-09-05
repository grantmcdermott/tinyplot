library(tinyplot)

## the canonical time-path: the x-z projection of a Lorenz attractor, where the
## colour gradient is the only thing separating one pass from the next (#711)
step = function(p, i) {
  p + 0.005 * c(
    10 * (p[2] - p[1]),
    p[1] * (28 - p[3]) - p[2],
    p[1] * p[2] - 8/3 * p[3]
  )
}
lz = do.call(rbind, Reduce(step, 1:6000, c(1, 1, 1), accumulate = TRUE))
lorenz = data.frame(time = seq_len(nrow(lz)) * 0.005, x = lz[, 1], z = lz[, 3])

plt(
  z ~ x | time,
  data = lorenz,
  type = "l",
  lwd = 2,
  xlab = NA, ylab = NA, legend = FALSE,
  main = "Lorenz attractor",
  theme = list("dark", grid = FALSE, xaxt = "n", yaxt = "n")
)
