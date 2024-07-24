library(tinytest)
library(tinysnapshot)

# Skip tests if not on Linux
ON_LINUX = Sys.info()["sysname"] == "Linux"
if (!ON_LINUX) exit_file("Linux snapshots")

options("tinysnapshot_os" = "Linux")
options("tinysnapshot_device" = "svglite")
options("tinysnapshot_device_args" = list(user_fonts = fontquiver::font_families("Liberation")))
