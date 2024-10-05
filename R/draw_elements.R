# Draw plot draw plot elements
#
# This function is called by `tinyplot()` to draw plot elements such as lines, polygons and points.
#
draw_elements = function(
    type,
    ix,
    iy,
    ixmin,
    ixmax,
    iymin,
    iymax,
    icol,
    ilwd,
    ipch,
    ibg,
    ilty,
    i,
    cex,
    dots,
    empty_plot,
    facet_by,
    split_data,
    x_by = FALSE,
    flip = FALSE,
    draw_fun = NULL
    ) {

      types = c("boxplot", "ribbon", "pointrange", "errorbar", "polygon", "polypath", "rect", "segments", "p", "points", "l", "o", "b", "c", "h", "s", "S", "n")
      assert_choice(type, types, null.ok = TRUE)

      ## polygons before lines, segments/arrows before points, etc.
      if (isTRUE(empty_plot)) return(invisible())

      if (is.null(draw_fun)) {
        draw_fun = switch(
          type,
          "ribbon" = draw_ribbon,
          "polygon" = draw_polygon,
          "polypath" = draw_polypath,
          "rect" = draw_rect,
          "segments" = draw_segments,
          "boxplot" = draw_boxplot,
          "p" = ,
          "points" = draw_points(),
          "l" = ,
          "o" = ,
          "b" = ,
          "c" = ,
          "h" = ,
          "s" = ,
          "S" = draw_lines)
      }
      draw_fun(
        ibg = ibg,
        icol = icol,
        ilty = ilty,
        ilwd = ilwd,
        ipch = ipch,
        ix = ix,
        ixmax = ixmax,
        ixmin = ixmin,
        iy = iy,
        iymax = iymax,
        iymin = iymin,
        cex = cex,
        dots = dots,
        type = type,
        x_by = x_by,
        i = i,
        facet_by = facet_by,
        split_data = split_data,
        flip = flip)

}


draw_ribbon <- function(ix, iy, ixmin, ixmax, iymin, iymax, ibg, ilty, ilwd, icol, ipch, i, flip = FALSE, ...) {
  if (isFALSE(flip)) {
    draw_polygon(ix = c(ix, rev(ix)), iy = c(iymin, rev(iymax)), icol = NA, ibg = ibg)
  } else {
    draw_polygon(ix = c(ixmin, rev(ixmax)), iy = c(iy, rev(iy)), icol = NA, ibg = ibg)
  }
  draw_lines(ix = ix, iy = iy, icol = icol, ipch = ipch, ilty = ilty, ilwd = ilwd, type = "l")
}







draw_lines <- function(ix, iy, icol, ipch, ilty, ilwd, type, ...) {
  lines(
    x = ix,
    y = iy,
    col = icol,
    type = type,
    pch = ipch,
    lty = ilty,
    lwd = ilwd
  )
}


draw_polygon <- function(ix, iy, icol, ibg, ilty = par("lty"), ilwd = par("lwd"), ...) {
  polygon(
    x = ix,
    y = iy,
    border = icol,
    col = ibg,
    lty = ilty,
    lwd = ilwd
  )
}


draw_polypath <- function(ix, iy, icol, ibg, ilty, ilwd, dots, ...) {
  irule <- ifelse(!is.null(dots[["rule"]]), dots[["rule"]], "winding")
  polypath(
    x = ix,
    y = iy,
    border = icol,
    col = ibg,
    lty = ilty,
    lwd = ilwd,
    rule = irule
  )
}


draw_rect <- function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ibg, ...) {
  rect(
    xleft = ixmin, ybottom = iymin, xright = ixmax, ytop = iymax,
    lty = ilty,
    lwd = ilwd,
    border = icol,
    col = ibg
  )
}


draw_segments <- function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ...) {
  segments(
    x0 = ixmin, y0 = iymin, x1 = ixmax, y1 = iymax,
    lty = ilty,
    lwd = ilwd,
    col = icol
  )
}


draw_boxplot <- function(i, ix, iy, ipch, ilty, icol, ibg, dots, x_by = FALSE, facet_by = FALSE, split_data, ...) {

  at_ix <- unique(ix)
  horizontal <- if (!is.null(dots[["horizontal"]])) dots[["horizontal"]] else FALSE
  range_ix <- if (!is.null(dots[["range"]])) dots[["range"]] else 1.5
  boxwidth_ix <- if (!is.null(dots[["boxwidth"]])) dots[["boxwidth"]] else NULL
  varwidth_ix <- if (!is.null(dots[["varwidth"]])) dots[["varwidth"]] else FALSE
  notch_ix <- if (!is.null(dots[["notch"]])) dots[["notch"]] else FALSE
  outline_ix <- if (!is.null(dots[["outline"]])) dots[["outline"]] else TRUE
  boxwex_ix <- if (!is.null(dots[["boxwex"]])) dots[["boxwex"]] else 0.8
  if (isTRUE(x_by)) boxwex_ix <- boxwex_ix * 2
  staplewex_ix <- if (!is.null(dots[["staplewex"]])) dots[["staplewex"]] else 0.5
  outwex_ix <- if (!is.null(dots[["outwex"]])) dots[["outwex"]] else 0.5

  # Handle multiple groups
  if (!is.null(split_data) && isFALSE(x_by) && isFALSE(facet_by) && length(split_data) > 1) {
    boxwex_ix_orig <- boxwex_ix
    boxwex_ix <- boxwex_ix / length(split_data) - 0.01
    at_ix <- at_ix + seq(
      -((boxwex_ix_orig - boxwex_ix) / 2),
      ((boxwex_ix_orig - boxwex_ix) / 2),
      length.out = length(split_data)
    )[i]
  }

  boxplot(
    formula = iy ~ ix,
    pch = ipch,
    lty = ilty,
    border = icol,
    col = ibg,
    add = TRUE, axes = FALSE,
    horizontal = horizontal,
    at = at_ix,
    range = range_ix,
    width = boxwidth_ix,
    varwidth = varwidth_ix,
    notch = notch_ix,
    outline = outline_ix,
    boxwex = boxwex_ix,
    staplewex = staplewex_ix,
    outwex = outwex_ix
  )

}


