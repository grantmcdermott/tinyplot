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
          "ribbon" = type_ribbon()$draw,
          "polygon" = type_polygon()$draw,
          "rect" = type_rect()$draw,
          "p" = ,
          "points" = type_points()$draw,
          "l" = ,
          "o" = ,
          "b" = ,
          "c" = ,
          "h" = ,
          "s" = ,
          "S" = type_lines(type = type)$draw)
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


