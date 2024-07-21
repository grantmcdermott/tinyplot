setup_device = function(file, width, height) {
  # write to file
  if (!is.null(file)) {
    filepath = file
    filewidth = width
    fileheight = height
    if (is.null(filewidth)) filewidth = .tpar[["file.width"]]
    if (is.null(fileheight)) fileheight = .tpar[["file.height"]]
    fileres = .tpar[["file.res"]]
    # catch to close interactive device if one isn't already open
    fkdev = is.null(dev.list())
    # grab existing device pars to pass on to next one
    dop = par(no.readonly = TRUE)
    # close interactive device if not already open
    if (isTRUE(fkdev)) dev.off()
    exttype = file_ext(filepath)
    if (exttype == "jpg") exttype = "jpeg"
    switch(exttype,
      png = png(filepath, width = filewidth, height = fileheight, units = "in", res = fileres),
      jpeg = jpeg(filepath, width = filewidth, height = fileheight, units = "in", res = fileres),
      pdf = pdf(filepath, width = filewidth, height = fileheight),
      svg = svg(filepath, width = filewidth, height = fileheight),
      stop("\nUnsupported file extension. Only '.png', '.jpg', '.pdf', or '.svg' are allowed.\n")
    )
    dop$new = FALSE # catch for some interfaces
    par(dop)

  # interactive plot with user-specified width/height
  } else if (!is.null(width) || !is.null(height)) {
    devwidth = width
    devheight = height
    # if one of width or height is missing, set equal to the other
    if (is.null(devwidth)) devwidth = devheight
    if (is.null(devheight)) devheight = devwidth
    # catch to close interactive device if one isn't already open
    fkdev = is.null(dev.list())
    # grab existing device pars to pass on to next one
    dop = par(no.readonly = TRUE)
    # close interactive device if not already open
    if (isTRUE(fkdev)) dev.off()
    dev.new(width = devwidth, height = devheight)
    dop$new = FALSE # catch for some interfaces
    par(dop)
  }
}
