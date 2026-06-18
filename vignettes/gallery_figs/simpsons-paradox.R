library(tinyplot)

plt(
  bill_dep ~ bill_len | species, data = penguins,
  pch = c(21, 24, 22), fill = 0.6, cex = 1.2,
  main = "Bill dimensions for different penguin species",
  sub = "Illustration of Simpson's paradox",
  cap = "Notes: Lines show OLS fit for species-specific (solid) vs. pooled (dotted grey) samples.",
  xlab = "Bill length (mm)", ylab = "Bill depth (mm)",
  legend = list("bottomright", title = "Species", bty = "o", lty = 1),
  theme = list("minimal", palette.qualitative = c("darkorange", "purple", "cyan4"))
)
plt_add(type = type_lm(se = FALSE), lwd = 2)
plt_add(bill_dep ~ bill_len, type = type_lm(se = FALSE), lwd = 2, lty = 2, col = "darkgray")
