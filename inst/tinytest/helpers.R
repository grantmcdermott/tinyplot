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

# Mirror the gate that tinysnapshot uses to decide whether plot snapshots run
snapshots_run = local({
  os = getOption("tinysnapshot_os", default = Sys.info()[["sysname"]])
  skip = getOption(
    "tinysnapshot_plot_skip",
    default = !interactive() && !identical(Sys.getenv("NOT_CRAN"), "true")
  )
  Sys.info()[["sysname"]] %in% os && !skip
})

# reset theme in every file
tinytheme()
