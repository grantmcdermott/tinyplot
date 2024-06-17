source("helpers.R")
using("tinysnapshot")

# Test keyword(s)
## from palette.colors
f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        pch = 16,
        palette = "Tableau 10"
    )
}
expect_snapshot_plot(f, label = "palette_keyword")

## from hcl.colors
f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        pch = 16,
        palette = "Set 1"
    )
}
expect_snapshot_plot(f, label = "palette_keyword2")

# Test partial match(es)
f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        pch = 16,
        palette = "tableau"
    )
}
expect_snapshot_plot(f, label = "palette_keyword")

f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        pch = 16,
        palette = "set1"
    )
}
expect_snapshot_plot(f, label = "palette_keyword2")

# Partial match should fail if ambiguous
f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        type = "p", pch = 16,
        palette = "set" # set1, set2, set3, etc.
    )
}
expect_error(f())

# Test alpha transparency arg
f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        pch = 19,
        palette = "tableau",
        alpha = 0.5
    )
}
expect_snapshot_plot(f, label = "palette_alpha")

# Test function
f = function() {
    tinyplot(
        Sepal.Length ~ Petal.Length | Species, iris,
        pch = 16,
        palette = palette.colors(palette = "tableau", alpha = 0.5)
    )
}
expect_snapshot_plot(f, label = "palette_function")