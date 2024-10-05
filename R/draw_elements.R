# Draw plot draw plot elements
#
# This function is called by `tinyplot()` to draw plot elements such as lines, polygons and points.
#
draw_elements = function(
    type,
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
    x_by = FALSE,
    flip = FALSE
    ) {

      types = c("boxplot", "ribbon", "pointrange", "errorbar", "polygon", "polypath", "rect", "segments", "p", "points", "l", "o", "b", "c", "h", "s", "S", "n")
      assert_choice(type, types, null.ok = TRUE)

      ## polygons before lines, segments/arrows before points, etc.
      if (isTRUE(empty_plot)) return(invisible())

      if (type == "boxplot") {
        at_xx = unique(xx)
        horizontal = ifelse(!is.null(dots[["horizontal"]]), dots[["horizontal"]], FALSE)
        range_xx = ifelse(!is.null(dots[["range"]]), dots[["range"]], 1.5)
        boxwidth_xx = NULL
        if (!is.null(dots[["boxwidth"]])) boxwidth_xx = dots[["boxwidth"]]
        varwidth_xx = ifelse(!is.null(dots[["varwidth"]]), dots[["varwidth"]], FALSE)
        notch_xx = ifelse(!is.null(dots[["notch"]]), dots[["notch"]], FALSE)
        outline_xx = ifelse(!is.null(dots[["outline"]]), dots[["outline"]], TRUE)
        boxwex_xx = ifelse(!is.null(dots[["boxwex"]]), dots[["boxwex"]], 0.8)
        if (isTRUE(x_by)) boxwex_xx = boxwex_xx * 2
        staplewex_xx = ifelse(!is.null(dots[["staplewex"]]), dots[["staplewex"]], 0.5)
        outwex_xx = ifelse(!is.null(dots[["outwex"]]), dots[["outwex"]], 0.5)
        if (!is.null(by) && isFALSE(x_by) && isFALSE(facet_by) && length(split_data) > 1) {
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
          range = range_xx,
          width = boxwidth_xx,
          varwidth = varwidth_xx,
          notch = notch_xx,
          outline = outline_xx,
          boxwex = boxwex_xx,
          staplewex = staplewex_xx,
          outwex = outwex_xx
        )

    } else {
        draw_fun = switch(
        type,
        "ribbon" = draw_ribbon,
        "pointrange" = draw_pointrange,
        "errorbar" = draw_errorbar,
        "polygon" = draw_polygon,
        "polypath" = draw_polypath,
        "rect" = draw_rect,
        "segments" = draw_segments,
        "p" = ,
        "points" = draw_points,
        "l" = ,
        "o" = ,
        "b" = ,
        "c" = ,
        "h" = ,
        "s" = ,
        "S" = draw_lines)
        draw_fun(xx = xx,
          yy = yy,
          xxmin = xxmin,
          yymin = yymin,
          xxmax = xxmax,
          yymax = yymax,
          icol = icol,
          ilwd = ilwd,
          ilty = ilty,
          ipch = ipch,
          cex = cex,
          ibg = ibg,
          dots = dots,
          type = type,
          flip = flip)

      }
}


# Draw Ribbon
draw_ribbon <- function(xx, yy, xxmin, xxmax, yymin, yymax, ibg, ilty, ilwd, icol, ipch, i = 1, flip = FALSE, ...) {
  if (isFALSE(flip)) {
    draw_polygon(x = c(xx, rev(xx)), y = c(yymin, rev(yymax)), icol = NA, ibg = ibg)
  } else {
    draw_polygon(x = c(xxmin, rev(xxmax)), y = c(yy, rev(yy)), icol = NA, ibg = ibg)
  }
  draw_lines(xx = xx, yy = yy, icol = icol, ipch = ipch, ilty = ilty, ilwd = ilwd, type = "l")
}


# Draw Pointrange
draw_pointrange <- function(xx, yy, xxmin, yymin, xxmax, yymax, icol, ibg, ipch, ilwd, cex, ...) {
  segments(
    x0 = xxmin,
    y0 = yymin,
    x1 = xxmax,
    y1 = yymax,
    col = icol,
    lwd = ilwd
  )
  draw_points(xx = xx, yy = yy, icol = icol, ibg = ibg, ipch = ipch, ilwd = ilwd, cex = cex)
}


# Draw Errorbar
draw_errorbar <- function(xx, yy, xxmin, yymin, xxmax, yymax, icol, ibg, ipch, ilwd, cex, ...) {
  arrows(
    x0 = xxmin,
    y0 = yymin,
    x1 = xxmax,
    y1 = yymax,
    col = icol,
    lwd = ilwd,
    length = 0.05,
    angle = 90,
    code = 3
  )
  draw_points(xx = xx, yy = yy, icol = icol, ibg = ibg, ipch = ipch, ilwd = ilwd, cex = cex)
}


# Draw Points
draw_points <- function(xx, yy, icol, ibg, ipch, ilwd, cex, ...) {
  points(
    x = xx,
    y = yy,
    col = icol,
    bg = ibg,
    type = "p",
    pch = ipch,
    lwd = ilwd,
    cex = cex
  )
}


# Draw Lines
draw_lines <- function(xx, yy, icol, ipch, ilty, ilwd, type, ...) {
  lines(
    x = xx,
    y = yy,
    col = icol,
    type = type,
    pch = ipch,
    lty = ilty,
    lwd = ilwd
  )
}


# Draw Polygon
draw_polygon <- function(xx, yy, icol, ibg, ilty = par("lty"), ilwd = par("lwd"), ...) {
  polygon(
    x = xx,
    y = yy,
    border = icol,
    col = ibg,
    lty = ilty,
    lwd = ilwd
  )
}


# Draw Polypath
draw_polypath <- function(xx, yy, icol, ibg, ilty, ilwd, dots, ...) {
  irule <- ifelse(!is.null(dots[["rule"]]), dots[["rule"]], "winding")
  polypath(
    x = xx,
    y = yy,
    border = icol,
    col = ibg,
    lty = ilty,
    lwd = ilwd,
    rule = irule
  )
}


# Draw Rectangle
draw_rect <- function(xxmin, yymin, xxmax, yymax, ilty, ilwd, icol, ibg, ...) {
  rect(
    xleft = xxmin, ybottom = yymin, xright = xxmax, ytop = yymax,
    lty = ilty,
    lwd = ilwd,
    border = icol,
    col = ibg
  )
}


# Draw Segments
draw_segments <- function(xxmin, yymin, xxmax, yymax, ilty, ilwd, icol, ...) {
  segments(
    x0 = xxmin, y0 = yymin, x1 = xxmax, y1 = yymax,
    lty = ilty,
    lwd = ilwd,
    col = icol
  )
}


# Draw Boxplot
draw_boxplot <- function(xx, yy, ipch, ilty, icol, ibg, dots, i = 1, x_by = FALSE, facet_by = FALSE, split_data, ...) {
  at_xx <- unique(xx)
  horizontal <- ifelse(!is.null(dots[["horizontal"]]), dots[["horizontal"]], FALSE)
  range_xx <- ifelse(!is.null(dots[["range"]]), dots[["range"]], 1.5)
  boxwidth_xx <- ifelse(!is.null(dots[["boxwidth"]]), dots[["boxwidth"]], NULL)
  varwidth_xx <- ifelse(!is.null(dots[["varwidth"]]), dots[["varwidth"]], FALSE)
  notch_xx <- ifelse(!is.null(dots[["notch"]]), dots[["notch"]], FALSE)
  outline_xx <- ifelse(!is.null(dots[["outline"]]), dots[["outline"]], TRUE)
  boxwex_xx <- ifelse(!is.null(dots[["boxwex"]]), dots[["boxwex"]], 0.8)
  if (isTRUE(x_by)) boxwex_xx <- boxwex_xx * 2
  staplewex_xx <- ifelse(!is.null(dots[["staplewex"]]), dots[["staplewex"]], 0.5)
  outwex_xx <- ifelse(!is.null(dots[["outwex"]]), dots[["outwex"]], 0.5)
  
  # Handle multiple groups
  if (!is.null(split_data) && isFALSE(x_by) && isFALSE(facet_by) && length(split_data) > 1) {
    boxwex_xx_orig <- boxwex_xx
    boxwex_xx <- boxwex_xx / length(split_data) - 0.01
    at_xx <- at_xx + seq(
      -((boxwex_xx_orig - boxwex_xx) / 2),
      ((boxwex_xx_orig - boxwex_xx) / 2),
      length.out = length(split_data)
    )[i]
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
    range = range_xx,
    width = boxwidth_xx,
    varwidth = varwidth_xx,
    notch = notch_xx,
    outline = outline_xx,
    boxwex = boxwex_xx,
    staplewex = staplewex_xx,
    outwex = outwex_xx
  )
}


