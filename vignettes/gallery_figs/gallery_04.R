library(tinyplot)
ttnc = as.data.frame(Titanic)
tinyplot(
    Survived ~ Sex | Class,
    facet = "by", data = ttnc,
    type = type_spineplot(weights = ttnc$Freq),
    palette = "Dark 2", facet.args = list(nrow = 1), axes = "t"
)
