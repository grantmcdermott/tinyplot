source("helpers.R")
using("tinysnapshot")

s77 = transform(data.frame(state.x77), Illiteracy = Illiteracy / 100)

f = function() tinyplot(
    Life.Exp ~ Income | Illiteracy, data = s77,
    xaxl = '$',
    legend = list(labeller = '%'),
    theme = "clean"
)
expect_snapshot_plot(f, label = "tinylabel")
