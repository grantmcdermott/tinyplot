library("tinyplot")

plt(eruptions ~ waiting, data = faithful,
  xlab = "Eruption time (min)",
  ylab = "Waiting time to next eruption (min)",
  main = "Eruptions of Old Faithful",
  cap = "Rug(s) = Relative density\nShaded region = LOESS fit",
  type = "n",
  theme = "nber"
)
plt_add(type = "loess")
plt_add(type = "p", pch = 21, col = "white", fill = 0.3, cex = 1.5)
plt_add(type = type_rug(jitter = TRUE, amount = 0.3))
plt_add(type = type_rug(jitter = TRUE, amount = 0.1, side = 2))
