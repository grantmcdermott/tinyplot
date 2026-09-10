source("helpers.R")
using("tinysnapshot")

# Unlike the other test files, this one manages devices explicitly: whether a
# device records is exactly what `record` depends on, so it is the fixture
# rather than incidental setup. pdf(NULL) is a sink that draws nowhere and
# writes no file, and like all file devices it starts with its display list
# inhibited -- which is what makes it useful for both cases below.
recording_device = function() {
  pdf(NULL)
  dev.control(displaylist = "enable") # as interactive devices do by default
}
nonrecording_device = function() {
  pdf(NULL) # as file-based devices are by default
}


# `record` is opt-in: the default return value is NULL, as before.
recording_device()
expect_null(tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris))

# record = TRUE returns a replayable object with a non-empty display list
p = tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, record = TRUE)
expect_inherits(p, "recordedplot")
expect_true(length(p[[1]]) > 0L)

# tpar() sets the default, and an explicit argument takes precedence over it
tpar(record = TRUE)
expect_inherits(tinyplot(Sepal.Length ~ Petal.Length, data = iris), "recordedplot")
expect_null(tinyplot(Sepal.Length ~ Petal.Length, data = iris, record = FALSE))
tpar(record = NULL)
expect_null(tinyplot(Sepal.Length ~ Petal.Length, data = iris))
dev.off()

# A non-recording device yields an empty plot; warn rather than hand back
# something that silently replays blank.
nonrecording_device()
expect_warning(
  tinyplot(Sepal.Length ~ Petal.Length, data = iris, record = TRUE),
  pattern = "not recording"
)
dev.off()

# tinyplot enables the display list on devices it opens itself, so record = TRUE
# and file = must compose (no enclosing device here: `file` supplies its own).
tmp = tempfile(fileext = ".png")
q = tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
             record = TRUE, file = tmp)
expect_inherits(q, "recordedplot")
expect_true(length(q[[1]]) > 0L)
expect_true(file.exists(tmp))
unlink(tmp)

# ... and without record, file = still returns NULL
tmp2 = tempfile(fileext = ".png")
expect_null(tinyplot(Sepal.Length ~ Petal.Length, data = iris, file = tmp2))
unlink(tmp2)

# invalid input
nonrecording_device()
expect_error(
  tinyplot(Sepal.Length ~ Petal.Length, data = iris, record = "yes"),
  pattern = "record"
)
dev.off()
expect_error(tpar(record = "yes"), pattern = "record")
# NB: tpar() assigns before it validates, so a rejected value sticks and would
# poison every later call in this file. Reset it explicitly.
tpar(record = NULL)

# A recorded plot replays to exactly the same output as the original draw.
# Deliberately not a snapshot test: replaying onto a *different* device than the
# one that recorded is not faithful (text metrics differ by backend), and the
# snapshot device is configured separately from whatever records here. Comparing
# two files produced in this session sidesteps that and tests the real property.
if (requireNamespace("svglite", quietly = TRUE)) {
  draw = function(f, record = FALSE) {
    svglite::svglite(f, width = 7, height = 7)
    on.exit(dev.off())
    if (record) dev.control(displaylist = "enable")
    tinyplot(Sepal.Length ~ Petal.Length | Species, data = iris, record = record)
  }
  f_native = tempfile(fileext = ".svg")
  f_replay = tempfile(fileext = ".svg")
  draw(f_native)
  rec = draw(tempfile(fileext = ".svg"), record = TRUE)
  svglite::svglite(f_replay, width = 7, height = 7)
  replayPlot(rec)
  dev.off()
  native = readLines(f_native, warn = FALSE)
  replay = readLines(f_replay, warn = FALSE)
  # guard against the comparison passing vacuously on two empty files
  expect_true(length(native) > 10L)
  expect_identical(replay, native)
  unlink(c(f_native, f_replay))
}

tpar(record = NULL)
