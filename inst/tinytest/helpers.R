library(tinytest)
library(tinysnapshot)

# # Skip tests if not on Linux
# ON_LINUX = Sys.info()["sysname"] == "Linux"
# if (!ON_LINUX) exit_file("Linux snapshots")

options("tinysnapshot_os" = "Linux")
options("tinysnapshot_device" = "svglite")
options("tinysnapshot_device_args" = list(
  user_fonts = fontquiver::font_families("Liberation"),
  # Pin the plotmath symbol font (face 5) so it doesn't drift to a system
  # symbol font (e.g. Arch's "Standard Symbols PS") instead of CI's DejaVu Sans.
  system_fonts = list(symbol = "DejaVu Sans")
))

# reset theme in every file
tinytheme()
