library("tinyplot")

mtcars2 = within(
  mtcars, {
    make = sub(" .*", "", row.names(mtcars))
    model = sub("^\\S+\\s+", "", row.names(mtcars))
    merc = factor(ifelse(make == "Merc", "Mercedes", "Other"), levels = c("Other", "Mercedes"))
  }
)

plt(
  qsec ~ mpg | merc,
  data = mtcars2,
  legend = list(title = NULL),
  ylab = "1/4 mile time (s)",
  xlab = "Miles per gallon",
  main = "Performance vs efficiency",
  sub = "Mercedes vs the rest",
  cap = "Notes: Model numbers highlighted in text",
  theme = list("web", palette.qualitative = c("#0FCFC0", "#F79CD4"))
)
plt_add(
  type = "text", labels = mtcars2$model, repel = TRUE, xpd = NA, pos = 4,
  alpha = 0.3
)
plt_add(
  data = subset(mtcars2, make=="Merc")
)
plt_add(
  data = subset(mtcars2, make=="Merc"),
  type = "text", labels = subset(mtcars2, make == "Merc")$model,
  repel = TRUE, xpd = NA, pos = 4, font = 2
)

# optional annotations
arrows(12, 19, 12, 23, length = 0.1, angle = 20, col = "darkgrey", xpd = NA)
text(12, mean(c(19, 23)), "slower", srt = 90, adj = c(0.5, 1.5), col = "darkgrey", font = 3)
arrows(24, 15, 34.5, 15, length = 0.1, angle = 20, col = "darkgrey", xpd = NA)
text(mean(c(24, 34.5)), 15, "thriftier", adj = c(0.5, -1), col = "darkgrey", font = 3)

