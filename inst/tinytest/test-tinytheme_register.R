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

# register shortcut via tinytheme(..., register = )
tinytheme("float", bg = "#f5e6c8", register = "float3")
expect_equal(
  tpar("tinytheme"), "float3",
  info = "register shortcut activates under registered name"
)
expect_true(
  "float3" %in% tinytheme_list()[["registered"]],
  info = "register shortcut adds to registry"
)
tinytheme()

# clean up
tinytheme_unregister("float2")
tinytheme_unregister("float3")
