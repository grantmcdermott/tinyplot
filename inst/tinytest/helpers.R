library(tinytest)
library(tinysnapshot)
ON_CRAN <- !Sys.getenv("R_NOT_CRAN") %in% c("yes", "true", "TRUE")
ON_LINUX <- Sys.info()["sysname"] == "Linux"
options("tinysnapshot_os" = "Linux")
options("tinysnapshot_device" = "svglite")
options("tinysnapshot_device_args" = list(user_fonts = fontquiver::font_families("Liberation")))