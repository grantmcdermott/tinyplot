#' Draw gradient (continuous) legend swatch
#'
#' @description For gradient legends, we draw a custom color swatch using
#'   grDevices::as.raster and add labels, tick marks, and title manually.
#'
#' @param legend_args Legend arguments list
#' @param fklgnd Fake legend object (from drawing with plot=FALSE)
#' @param lmar Legend margins
#' @param outer_side Logical flag for outer side placement
#' @param outer_end Logical flag for outer end placement
#' @param outer_right Logical flag for outer right placement
#' @param outer_bottom Logical flag for outer bottom placement
#' @param user_inset Logical flag indicating user-supplied inset
#'
#' @returns NULL (draws gradient legend as side effect)
#'
#' @keywords internal
draw_gradient_swatch = function(
  legend_args,
  fklgnd,
  lmar,
  outer_side,
  outer_end,
  outer_right,
  outer_bottom,
  user_inset = FALSE
) {
  pal = legend_args[["col"]]
  lgnd_labs = legend_args[["legend"]]
  if (!is.null(legend_args[["horiz"]])) {
    horiz = legend_args[["horiz"]]
  } else {
    horiz = FALSE
  }

  # Create raster color swatch
  if (isTRUE(horiz)) {
    rasterlgd = as.raster(matrix(pal, nrow = 1))
  } else {
    rasterlgd = as.raster(matrix(rev(pal), ncol = 1))
  }

  corners = par("usr")
  rasterbox = rep(NA_real_, 4)

  # Determine positioning flags
  inner = !any(c(outer_side, outer_end))
  inner_right = inner_bottom = FALSE
  if (inner) {
    if (!is.null(legend_args[["x"]]) && grepl("left$|right$", legend_args[["x"]])) {
      inner_right = grepl("right$", legend_args[["x"]])
    }
    if (!is.null(legend_args[["x"]]) && grepl("^bottoml|^top", legend_args[["x"]])) {
      inner_bottom = grepl("^bottom", legend_args[["x"]])
    }
  }

  # Calculate raster box coordinates based on position
  if (inner) {
    fklgnd$rect$h = fklgnd$rect$h - lines_to_user_y(1.5 + 0.4)

    rasterbox[1] = fklgnd$rect$left
    if (isFALSE(inner_right)) {
      rasterbox[1] = rasterbox[1] + lines_to_user_x(0.2)
    }
    rasterbox[2] = fklgnd$rect$top - fklgnd$rect$h - lines_to_user_y(1.5 + 0.2)
    rasterbox[3] = rasterbox[1] + lines_to_user_x(1.25)
    rasterbox[4] = rasterbox[2] + fklgnd$rect$h

  } else if (outer_side) {
    rb1_adj = lines_to_user_x(lmar[1] + 0.2)
    rb3_adj = lines_to_user_x(1.25)
    rb2_adj = (corners[4] - corners[3] - lines_to_user_y(5 + 1 + 2.5)) / 2
    # Override if top or bottom
    if (!is.null(legend_args[["x"]])) {
      if (grepl("^bottom", legend_args[["x"]])) {
        rb2_adj = corners[3]
      }
      if (grepl("^top", legend_args[["x"]])) {
        rb2_adj = corners[4] - lines_to_user_y(5 + 1 + 2.5)
      }
    }
    if (user_inset) {
      rb2_adj = rb2_adj + legend_args[["inset"]][2] + 0.05
    }
    rb4_adj = lines_to_user_y(5 + 1)

    if (outer_right) {
      rasterbox[1] = corners[2] + rb1_adj
      if (user_inset) {
        rasterbox[1] = rasterbox[1] - (corners[2] - legend_args[["inset"]][1]) / 2
      }
      rasterbox[2] = rb2_adj
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] + rb4_adj
    } else {
      rb1_adj = rb1_adj + lines_to_user_x(par("mar")[2] + 1)
      rasterbox[1] = corners[1] - rb1_adj
      rasterbox[2] = rb2_adj
      rasterbox[3] = rasterbox[1] - rb3_adj
      rasterbox[4] = rasterbox[2] + rb4_adj
    }

  } else if (outer_end) {
    rb1_adj = (corners[2] - corners[1] - lines_to_user_x(5 + 1)) / 2
    rb3_adj = lines_to_user_x(5 + 1)
    rb2_adj = lines_to_user_y(lmar[1])
    rb4_adj = lines_to_user_y(1.25)

    if (outer_bottom) {
      rb2_adj = rb2_adj + lines_to_user_y(par("mar")[2])
      rasterbox[1] = rb1_adj
      rasterbox[2] = corners[3] - rb2_adj
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] - rb4_adj
    } else {
      rb2_adj = rb2_adj + lines_to_user_y(1.25 + 1)
      rasterbox[1] = rb1_adj
      rasterbox[2] = corners[4] + rb2_adj
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] - rb4_adj
    }
  }

  # Draw the gradient swatch
  rasterImage(
    rasterlgd,
    rasterbox[1], #x1
    rasterbox[2], #y1
    rasterbox[3], #x2
    rasterbox[4], #y2
    xpd = NA
  )

  # Add labels, tick marks, and title
  if (isFALSE(horiz)) {
    draw_gradient_labels_vertical(rasterbox, lgnd_labs, legend_args, inner, outer_right)
  } else {
    draw_gradient_labels_horizontal(rasterbox, lgnd_labs, legend_args)
  }
}


# Draw vertical gradient legend labels, ticks, and title
draw_gradient_labels_vertical = function(rasterbox, lgnd_labs, legend_args, inner, outer_right) {
  labs_idx = !is.na(lgnd_labs)
  lgnd_labs[labs_idx] = paste0(" ", format(lgnd_labs[labs_idx]))

  # Determine anchors based on position
  if (!inner && !outer_right) {
    lbl_x_anchor = rasterbox[1]
    ttl_x_anchor = rasterbox[1] + max(strwidth(lgnd_labs[labs_idx]))
    lbl_adj = c(0, 0.5)
    ttl_adj = c(1, 0)
  } else {
    lbl_x_anchor = rasterbox[3]
    ttl_x_anchor = rasterbox[1]
    lbl_adj = c(0, 0.5)
    ttl_adj = c(0, 0)
  }

  # Draw labels
  text(
    x = lbl_x_anchor,
    y = seq(rasterbox[2], rasterbox[4], length.out = length(lgnd_labs)),
    labels = lgnd_labs,
    xpd = NA,
    adj = lbl_adj
  )

  # Draw tick marks (white dashes)
  lgnd_ticks = lgnd_labs
  lgnd_ticks[labs_idx] = "-   -"
  text(
    x = lbl_x_anchor,
    y = seq(rasterbox[2], rasterbox[4], length.out = length(lgnd_labs)),
    labels = lgnd_ticks,
    col = "white",
    xpd = NA,
    adj = c(1, 0.5)
  )

  # Draw title
  text(
    x = ttl_x_anchor,
    y = rasterbox[4] + lines_to_user_y(1),
    labels = legend_args[["title"]],
    xpd = NA,
    adj = ttl_adj
  )
}

# Draw horizontal gradient legend labels, ticks, and title
draw_gradient_labels_horizontal = function(rasterbox, lgnd_labs, legend_args) {
  # Legend labels
  text(
    x = seq(rasterbox[1], rasterbox[3], length.out = length(lgnd_labs)),
    y = rasterbox[4],
    labels = lgnd_labs,
    xpd = NA,
    adj = c(0.5, 1.25)
  )

  # Legend tick marks (white dashes)
  lgnd_ticks = lgnd_labs
  lgnd_ticks[!is.na(lgnd_ticks)] = "-   -"
  text(
    x = seq(rasterbox[1], rasterbox[3], length.out = length(lgnd_labs)),
    y = rasterbox[4],
    labels = lgnd_ticks,
    col = "white",
    xpd = NA,
    adj = c(0, 0.5),
    srt = 90
  )

  # Legend title
  text(
    x = rasterbox[1],
    y = rasterbox[4],
    labels = paste0(legend_args[["title"]], " "),
    xpd = NA,
    adj = c(1, -0.5)
  )
}
