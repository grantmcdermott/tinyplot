library(tinyplot)

aq = airquality
aq$Month = factor(month.name[aq$Month], levels = month.name[5:9])

tinyplot(
    Temp ~ Day | Month,
    data = aq,
    type = "l",
    legend = "direct",
    theme = "clean2",
    main = "Temperatures by month",
    sub = "Sometimes, direct legend labels are better"
)
