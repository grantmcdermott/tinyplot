source("helpers.R")
using("tinysnapshot")

# 2-variable data frame -> y ~ x
f = function() tinyplot(cars, theme = "clean2")
expect_snapshot_plot(f, label = "df_2var")

# 3+ variables -> pairs-style grid
f = function() tinyplot(iris, theme = "clean2")
expect_snapshot_plot(f, label = "df_pairs")

# pairs grid with a `by` grouping variable (column name)
f = function() tinyplot(iris, by = "Species", theme = "clean2")
expect_snapshot_plot(f, label = "df_pairs_by")

# `by` as a standalone vector (grouping var excluded from the grid)
f = function() tinyplot(iris[, 1:4], by = iris$Species, theme = "clean2")
expect_snapshot_plot(f, label = "df_pairs_by_vector")

# axis labels and per-panel frames toggled on
f = function() tinyplot(iris, by = "Species", labs = TRUE, frames = TRUE, theme = "clean2")
expect_snapshot_plot(f, label = "df_pairs_labs_frames")


# Non-snapshot logical / error checks (run on any platform) -----

# `by` column name must exist in the data frame
expect_error(tinyplot(iris, by = "nope"))

# non-syntactic column names work (matching pairs())
df = iris[, 1:3]
names(df) = c("a b", "c d", "e f")
expect_silent(tinyplot(df))

# a pre-existing "__by__" column does not collide with the spliced vector
df = iris[, 1:4]
df[["__by__"]] = rnorm(nrow(df))
expect_silent(tinyplot(df, by = iris$Species))
