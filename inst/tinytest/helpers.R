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

# Mirror of the gate tinysnapshot uses to decide whether plot snapshots run.
# A skipped snapshot never calls the plotting function, so tests that assert on
# a *side effect* of drawing (e.g. a warning) must skip in lockstep, or they
# outlive the thing they depend on and fail for an unrelated-looking reason.
# Mirrors both of tinysnapshot's conditions: the OS check (snapshots are pinned
# to Linux above, so they never run on e.g. macOS) and the skip flag.
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
