library(tinyplot)

lik = expand.grid(
  question = c("Pay", "Workload", "Manager", "Culture"),
  response = c("Strong disagree", "Disagree", "Agree", "Strong agree", "Unsure")
)
lik$response = factor(lik$response, levels = unique(lik$response))
lik$share = c( # proportions summing to 1 within each question
  .10, .25, .05, .15,
  .20, .30, .15, .20,
  .35, .20, .40, .30,
  .25, .15, .35, .20,
  .10, .10, .05, .15
)
# diverging palette: reds (disagree) -> blues (agree), grey for "Unsure"
pal = c("#b2182b", "#ef8a62", "#67a9cf", "#2166ac", "grey")
tinyplot(
  share ~ question | response, data = lik,
  type = "barplot", center = TRUE, offset = "Unsure",
  flip = TRUE, xlab = NA, ylab = NA, yaxl = "percent",
  legend = list("top!", title = NULL),
  theme = list("clean2", palette.qualitative = pal),
  main = "Hypothetical Likert example with category offset"
)
tinyplot_add(type = "vline")
