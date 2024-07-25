# Draw plot draw plot elements
#
# This function is called by `tinyplot()` to draw plot elements such as lines, polygons and points.
#
draw_elements = function(type,
                          xx,
                          yy,
                          xxmin,
                          xxmax,
                          yymin,
                          yymax,
                          bg,
                          icol,
                          ilwd,
                          ipch,
                          ibg,
                          ilty,
                          cex,
                          dots,
                          empty_plot,
                          facet_by,
                          split_data,
                          i = 1,
                          xlvls,
                          lgnd_labs) {

  ## polygons before lines, segments/arrows before points, etc.
  if (isTRUE(empty_plot)) {

  } else if (type == "ribbon") {
    polygon(
      x = c(xx, rev(xx)),
      y = c(yymin, rev(yymax)),
      col = bg[i],
      border = FALSE
    )
  } else if (type == "pointrange") {
    segments(
      x0 = xx,
      y0 = yymin,
      x1 = xx,
      y1 = yymax,
      col = icol,
      # lty = ilty,
      lwd = ilwd
    )
  } else if (type == "errorbar") {
    arrows(
      x0 = xx,
      y0 = yymin,
      x1 = xx,
      y1 = yymax,
      col = icol,
      # lty = ilty,
      lwd = ilwd,
      length = 0.05,
      angle = 90,
      code = 3
    )
  }

  ## now draw the points/lines/polygons/etc
  if (isTRUE(empty_plot)) {
    # empty plot
  } else if (type %in% c("p", "pointrange", "errorbar")) {
    points(
      x = xx,
      y = yy,
      col = icol,
      bg = ibg,
      ## rather hardcode "p" to avoid warning message about "pointrange"
      type = "p",
      pch = ipch,
      # lty = ilty,
      lwd = ilwd,
      cex = cex
    )
  } else if (type %in% c("l", "o", "b", "c", "h", "s", "S", "ribbon")) {
    rtype = type == "ribbon"
    if (rtype) type = "l"
    lines(
      x = xx,
      y = yy,
      col = icol,
      type = type,
      pch = ipch,
      lty = ilty,
      lwd = ilwd
    )
    if (rtype) type = "ribbon"
  } else if (type == "polygon") {
    polygon(
      x = xx,
      y = yy,
      border = icol,
      col = ibg,
      lty = ilty,
      lwd = ilwd
    )
  } else if (type == "polypath") {
    irule = ifelse(!is.null(dots[["rule"]]), dots[["rule"]], "winding")
    polypath(
      x = xx,
      y = yy,
      border = icol,
      col = ibg,
      lty = ilty,
      lwd = ilwd,
      rule = irule
    )
  } else if (type == "boxplot") {
    horizontal = ifelse(!is.null(dots[["horizontal"]]), dots[["horizontal"]], FALSE)
    boxwex_xx = ifelse(!is.null(dots[["boxwex"]]), dots[["boxwex"]], 0.8)
    staplewex_xx = ifelse(!is.null(dots[["staplewex"]]), dots[["staplewex"]], 0.5)
    outwex_xx = ifelse(!is.null(dots[["outwex"]]), dots[["outwex"]], 0.5)
    at_xx = unique(xx)
    x_by = !is.null(by) && identical(xlvls, lgnd_labs)
    if (!is.null(by) && isFALSE(x_by) && isFALSE(facet_by) && length(split_data)>1) {
      boxwex_xx_orig = boxwex_xx
      boxwex_xx = boxwex_xx / length(split_data) - 0.01
      at_xx = at_xx + seq(-((boxwex_xx_orig - boxwex_xx) / 2), ((boxwex_xx_orig - boxwex_xx) / 2), length.out = length(split_data))[i]
    }
    boxplot(
      formula = yy ~ xx,
      pch = ipch,
      lty = ilty,
      border = icol,
      col = ibg,
      add = TRUE, axes = FALSE,
      horizontal = horizontal,
      at = at_xx,
      boxwex = boxwex_xx,
      staplewex = staplewex_xx,
      outwex = outwex_xx
    )
  } else if (type == "rect") {
    rect(
      xleft = xxmin, ybottom = yymin, xright = xxmax, ytop = yymax,
      lty = ilty,
      lwd = ilwd,
      border = icol,
      col = ibg
    )
  } else if (type == "segments") {
    segments(
      x0 = xxmin, y0 = yymin, x1 = xxmax, y1 = yymax,
      lty = ilty,
      lwd = ilwd,
      col = icol
    )
  } else {
    stop("`type` argument not supported.", call. = FALSE)
  }
}
