fifa2026 = data.frame(
  team = c("Spain", "England", "France", "Germany", "Portugal", "Argentina", "Netherlands", "Brazil",
    "Belgium", "Norway", "Switzerland", "Croatia", "Colombia", "Japan", "Morocco", "United States",
    "Uruguay", "Senegal", "Sweden", "Ecuador", "Austria", "Turkey", "Canada", "Mexico", "South Korea",
    "Ivory Coast", "Algeria", "Australia", "Czech Republic", "Scotland", "Paraguay", "Egypt",
    "Bosnia and Herzegovina", "DR Congo", "Ghana", "Tunisia", "Iran", "Cape Verde", "Uzbekistan",
    "Panama", "Haiti", "New Zealand", "Saudi Arabia", "Curaçao", "South Africa", "Iraq", "Qatar", "Jordan"),
  win = c(14462, 12426, 12365, 11246, 8909, 8240, 5576, 4720, 2992, 2602, 2083, 1373, 1303, 1274, 990, 954,
    945, 945, 886, 752, 657, 651, 616, 601, 481, 281, 250, 222, 212, 206, 143, 121, 113, 71, 70, 59, 36,
    31, 29, 26, 24, 18, 12, 11, 7, 6, 2, 1)/1e5
)

library("tinyplot")
tinyplot(
  x = 1:48,
  y = fifa2026$win,
  type = "barplot",
  yaxl = "%",
  grid = "xY",
  xlab = "",
  ylab = "",
  xaxt = "n",
  main = "Spain most likely to (not) win the 2026 FIFA World Cup",
  cap = "Source: https://www.zeileis.org/news/fifa2026/",
  theme = list("clean2", grid.lty = 1, las = 2, dynmar = FALSE, mar = c(7, 4, 5, 0), cex.main = 1.3)
)
text(
  1:48,
  0,
  labels = fifa2026$team,
  srt = 45,
  adj = c(1.15, 1.15),
  xpd = TRUE,
  cex = 0.5
)
