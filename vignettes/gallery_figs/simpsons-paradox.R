library(tinyplot)

op = par(pch = 15)
plt(
  bill_dep ~ bill_len | species, data = penguins,
  pch = "by",
  cex = 1.2, alpha = 0.5,
  xlab = "Bill length (mm)",
  ylab = "Bill depth (mm)",
  main = "Simpson's paradox",
  sub = "Penguin bill dimensions",
  cap = "Notes: 1) Front colour ribbons = group-wise linear fit.\n2) Background grey ribbon = pooled linear fit.",
  theme = "clean2",
  draw = plt_add(bill_dep ~ bill_len, type = "lm", col = "gray50")
)
plt_add(type = "lm")
par(op)
