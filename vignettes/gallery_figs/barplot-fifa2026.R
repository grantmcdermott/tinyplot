fifa2026 = c(Spain = 14462, England = 12426, France = 12365, Germany = 11246, Portugal = 8909, Argentina = 8240,
  Netherlands = 5576, Brazil = 4720, Belgium = 2992, Norway = 2602, Switzerland = 2083, Croatia = 1373,
  Colombia = 1303, Japan = 1274, Morocco = 990, `United States` = 954, Uruguay = 945, Senegal = 945,
  Sweden = 886, Ecuador = 752, Austria = 657, Turkey = 651, Canada = 616, Mexico = 601, `South Korea` = 481, 
  `Ivory Coast` = 281, Algeria = 250, Australia = 222, `Czech Republic` = 212, Scotland = 206, Paraguay = 143,
  Egypt = 121, `Bosnia and Herzegovina` = 113, `DR Congo` = 71, Ghana = 70, Tunisia = 59, Iran = 36,
  `Cape Verde` = 31, Uzbekistan = 29, Panama = 26, Haiti = 24, `New Zealand` = 18, 
  `Saudi Arabia` = 12, Curaçao = 11, `South Africa` = 7, Iraq = 6, Qatar = 2, Jordan = 1) / 1e5

library("tinyplot")
tinyplot(
  fifa2026,
  type = "barplot",
  xaxr = 45, xord = "desc", # rotate xlabs at 45 deg and order (i.t.o. descending y values)
  ylab = NA, yaxl = "%",
  main = "Spain most likely to (not) win the 2026 FIFA World Cup",
  cap = "Source: https://www.zeileis.org/news/fifa2026/",
  theme = list("broadsheet", col.default = "#2A9D8F", cex.xaxs = 0.5)
)
