library(tinyplot)
ttnc = as.data.frame(Titanic)
tinyplot(
    Survived ~ Sex | Class,
    facet = "by", facet.args = list(nrow = 1),
    legend = FALSE,
    data = ttnc,
    type = type_spineplot(weights = ttnc$Freq),
    theme = "void", axes = "t",
    main = "Who survived the Titanic disaster?",
    sub = "Frequencies by boarding class and sex"
)
