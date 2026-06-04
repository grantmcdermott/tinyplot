source("helpers.R")
using("tinysnapshot")

# register a custom theme
tinytheme_register("float2", theme = "float", grid = TRUE, bg = "#f5e6c8")

# snapshot: registered theme used ephemerally
f = function() tinyplot(
  1:5,
  main = "Registered theme test",
  sub = 'theme = "float2"',
  theme = "float2"
)
expect_snapshot_plot(f, label = "tinytheme_register_float2")

# error on unregistered name
expect_error(
  tinytheme("float3"),
  pattern = "must be one of",
  info = "unregistered name errors"
)

# clean up
tinytheme_unregister("float2")
