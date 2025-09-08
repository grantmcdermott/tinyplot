#' @title Calculate placement of legend and draw it
#'   
#' @description Function used to calculate the placement of (including
#'   outside the plotting area) and drawing of legend.
#'   
#' @md
#' @param legend Legend placement keyword or list, passed down from [tinyplot].
#' @param legend_args Additional legend arguments to be passed to
#'   \code{\link[graphics]{legend}}.
#' @param by_dep The (deparsed) "by" grouping variable name.
#' @param lgnd_labs The labels passed to `legend(legend = ...)`.
#' @param type Plotting type(s), passed down from [tinyplot].
#' @param pch Plotting character(s), passed down from [tinyplot].
#' @param lty Plotting linetype(s), passed down from [tinyplot].
#' @param lwd Plotting line width(s), passed down from [tinyplot].
#' @param col Plotting colour(s), passed down from [tinyplot].
#' @param bg Plotting character background fill colour(s), passed down from [tinyplot].
#' @param cex Plotting character expansion(s), passed down from [tinyplot].
#' @param gradient Logical indicating whether a continuous gradient swatch
#'   should be used to represent the colors.
#' @param lmar Legend margins (in lines). Should be a numeric vector of the form
#'   `c(inner, outer)`, where the first number represents the "inner" margin
#'   between the legend and the plot, and the second number represents the
#'   "outer" margin between the legend and edge of the graphics device. If no
#'   explicit value is provided by the user, then reverts back to `tpar("lmar")`
#'   for which the default values are `c(1.0, 0.1)`.
#' @param has_sub Logical. Does the plot have a sub-caption. Only used if
#'   keyword position is "bottom!", in which case we need to bump the legend
#'   margin a bit further.
#' @param new_plot Logical. Should we be calling plot.new internally?
#' @param draw Logical. If `FALSE`, no legend is drawn but the sizes are
#'   returned. Note that a new (blank) plot frame will still need to be started
#'   in order to perform the calculations.
#' 
#' @returns No return value, called for side effect of producing a(n empty) plot
#'   with a legend in the margin.
#' 
#' @importFrom graphics grconvertX grconvertY rasterImage strwidth
#' @importFrom grDevices as.raster recordGraphics
#' @importFrom utils modifyList
#' 
#' @examples
#' oldmar = par("mar")
#' 
#' draw_legend(
#'   legend = "right!", ## default (other options incl, "left(!)", ""bottom(!)", etc.)
#'   legend_args = list(title = "Key", bty = "o"),
#'   lgnd_labs = c("foo", "bar"),
#'   type = "p",
#'   pch = 21:22,
#'   col = 1:2
#' )
#' 
#' # The legend is placed in the outer margin...
#' box("figure", col = "cyan", lty = 4)
#' # ... and the plot is proportionally adjusted against the edge of this
#' # margin.
#' box("plot")
#' # You can add regular plot objects per normal now
#' plot.window(xlim = c(1,10), ylim = c(1,10))
#' points(1:10)
#' points(10:1, pch = 22, col = "red")
#' axis(1); axis(2)
#' # etc.
#' 
#' # Important: A side effect of draw_legend is that the inner margins have been
#' # adjusted. (Here: The right margin, since we called "right!" above.)
#' par("mar")
#' 
#' # To reset you should call `dev.off()` or just reset manually.
#' par(mar = oldmar)
#' 
#' # Note that the inner and outer margin of the legend itself can be set via
#' # the `lmar` argument. (This can also be set globally via
#' # `tpar(lmar = c(inner, outer))`.)
#' draw_legend(
#'   legend_args = list(title = "Key", bty = "o"),
#'   lgnd_labs = c("foo", "bar"),
#'   type = "p",
#'   pch = 21:22,
#'   col = 1:2,
#'   lmar = c(0, 0.1) ## set inner margin to zero
#' )
#' box("figure", col = "cyan", lty = 4)
#' 
#' par(mar = oldmar)
#' 
#' # Continuous (gradient) legends are also supported
#' draw_legend(
#'   legend = "right!",
#'   legend_args = list(title = "Key"),
#'   lgnd_labs = LETTERS[1:5],
#'   col = hcl.colors(5),
#'   gradient = TRUE ## enable gradient legend
#' )
#' 
#' par(mar = oldmar)
#' 
#' @export
draw_legend = function(
    legend = NULL,
    legend_args = NULL,
    by_dep = NULL,
    lgnd_labs = NULL,
    type = NULL,
    pch = NULL,
    lty = NULL,
    lwd = NULL,
    col = NULL,
    bg = NULL,
    cex = NULL,
    gradient = FALSE,
    lmar = NULL,
    has_sub = FALSE,
    new_plot = TRUE,
    draw = TRUE
) {
  
    if (is.null(lmar)) {
      lmar = tpar("lmar")
    } else {
      if (!is.numeric(lmar) || length(lmar)!=2) stop ("lmar must be a numeric of length 2.")
    }
  
    assert_logical(gradient)
    assert_logical(has_sub)
    assert_logical(new_plot)
    assert_logical(draw)
    
    #
    ## legend args ----

    list2env(
      compute_legend_args(
        legend = legend,
        legend_args = legend_args,
        by_dep = by_dep,
        lgnd_labs = lgnd_labs,
        type = type,
        pch = pch,
        lty = lty,
        lwd = lwd,
        col = col,
        bg = bg,
        cex = cex,
        gradient = gradient
      ),
      environment()
    )
  
    #
    ## legend placement ----
    topmar_epsilon = 0.1
    dynmar = isTRUE(.tpar[["dynmar"]])

    ## restore margin defaults
    ## (in case the plot region/margins were affected by the preceding tinyplot call)
    restore_margin_outer()
    if (!dynmar) restore_margin_inner(ooma)

    ooma = par("oma")
    omar = par("mar")
    
    ## Legend to outer side (either right or left) of plot
    if (outer_side) {
      # extra bump for spineplot if outer_right legend (to accommodate secondary y-axis)
      if (identical(type, "spineplot")) lmar[1] = lmar[1] + 1.1
      
      ## We have to set the inner margins of the plot before the (fake) legend is
      ## drawn, otherwise the inset calculation---which is based in the legend
      ## width---will be off the first time.
      if (outer_right) {
        omar[4] = 0
      } else {
        # For outer left we have to account for the y-axis label too, which
        # requires additional space
        omar[2] = par("mgp")[1] + 1*par("cex.lab")
      }
      par(mar = omar)
      
      # if (new_plot && draw) {
      if (new_plot) {
        plot.new()
        # For themed + dynamic plots, we need to make sure the adjusted plot
        # margins for the legend are reinstated (after being overwritten by
        # the before.plot.new hook.
        if (dynmar) {
          omar = par("mar")
          if (outer_right) {
            omar[4] = 0
          } else {
            omar[2] = par("mgp")[1] + 1*par("cex.lab")
          }
          par(mar = omar)
        }
      }
      
      ## Legend at the outer top or bottom of plot
    } else if (outer_end) {

      ## We have to set the inner margins of the plot before the (fake) legend is
      ## drawn, otherwise the inset calculation---which is based in the legend
      ## width---will be off the first time.
      if (outer_bottom) {
        omar[1] = par("mgp")[1] + 1*par("cex.lab")
        if (has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]]==1)) {
          omar[1] = omar[1] + 1*par("cex.sub")
        }
      } else {
        ## For "top!", the logic is slightly different: We don't expand the outer
        ## margin b/c we need the legend to come underneath the main title. So
        ## we rather expand the existing inner margin.
        ooma[3] = ooma[3] + topmar_epsilon
        par(oma = ooma)
      }
      par(mar = omar)

      # if (new_plot && draw) {
      if (new_plot) {
        plot.new()
        # For themed + dynamic plots, we need to make sure the adjusted plot
        # margins for the legend are reinstated (after being overwritten by
        # the before.plot.new hook.
        if (dynmar) {
          omar = par("mar")
          if (outer_bottom) {
            # omar[1] = par("mgp")[1] + 1*par("cex.lab")
            omar[1] = theme_clean$mgp[1] + 1*par("cex.lab") ## bit of a hack
            if (has_sub && (is.null(.tpar[["side.sub"]]) || .tpar[["side.sub"]]==1)) omar[1] = omar[1] + 1*par("cex.sub")
          } else {
            ooma[3] = ooma[3] + topmar_epsilon
            par(oma = ooma)
          }
          par(mar = omar)
        }
      }


    } else {
      if (new_plot) plot.new()
      
    }
    #
    ## draw the legend ----
    # Legend drawing is handled by the internal `tinylegend()` function, which:
    #   1. calculates appropriate insets for "outer" legend placement
    #   2. can draw gradient legends (via `gradient_legend()` below)
    #
    # Note: We wrap everything in `recordGraphics()` to preserve legend spacing
    # if the plot is resized (also necessary for Positron graphics logic regardless)
      recordGraphics(
        tinylegend(
          legend_args = legend_args,
          ooma = ooma,
          omar = omar,
          lmar = lmar,
          topmar_epsilon = topmar_epsilon,
          outer_side = outer_side,
          outer_right = outer_right,
          outer_end = outer_end,
          outer_bottom = outer_bottom,
          gradient = gradient,
          user_inset = user_inset,
          draw = draw
        ),
        list = list(
          legend_args = legend_args,
          ooma = ooma,
          omar = omar,
          lmar = lmar,
          topmar_epsilon = topmar_epsilon,
          outer_side = outer_side,
          outer_right = outer_right,
          outer_end = outer_end,
          outer_bottom = outer_bottom,
          gradient = gradient,
          user_inset = user_inset,
          draw = draw
        ),
        env = getNamespace("tinyplot")
      )
  
}


# tinylegend ----

## Internal workhorse function that draws the legend, given a set of legend
## arguments and other graphical parameters. It does this in three steps:
## 1) draw a fake legend, 2) calculate the associated inset and adjust the plot
## margins accordingly, 3) draw the real legend

tinylegend = function(
    legend_args,
    ooma, omar, lmar, topmar_epsilon,
    outer_side, outer_right, outer_end, outer_bottom,
    gradient,
    user_inset = FALSE,
    draw
) {
  
  #
  ## Step 1: "draw" fake legend
  
  fklgnd.args = modifyList(
    legend_args,
    list(plot = FALSE),
    keep.null = TRUE
  )
  
  if (gradient) {
    lgnd_labs_tmp = na.omit(fklgnd.args[["legend"]])
    if (length(lgnd_labs_tmp) < 5L) {
      nmore = 5L - length(lgnd_labs_tmp)
      lgnd_labs_tmp = c(lgnd_labs_tmp, rep("", nmore))
    }
    fklgnd.args = modifyList(
      fklgnd.args,
      list(legend = lgnd_labs_tmp),
      keep.null = TRUE
    )
    if (outer_end) fklgnd.args = modifyList(fklgnd.args, list(title = NULL), keep.null = TRUE)
  }
  
  fklgnd = do.call("legend", fklgnd.args)
  if (!draw) return(fklgnd)
  
  #
  ## Step 2: Calculate legend inset (for outer placement in plot region)
  
  # calculate outer margin width in lines
  soma = 0
  if (outer_side) {
    soma = grconvertX(fklgnd$rect$w, to="lines") - grconvertX(0, to="lines")
  } else if (outer_end) {
    soma = grconvertY(fklgnd$rect$h, to="lines") - grconvertY(0, to="lines")
  }
  # Add legend margins to the outer margin
  soma = soma + sum(lmar)
  
  ## differing outer margin adjustments depending on side
  if (outer_side) {
    if (outer_right) {
      ooma[4] = soma
    } else {
      ooma[2] = soma
    }
  } else if (outer_end) {
    if (outer_bottom) {
      ooma[1] = soma
    } else {
      omar[3] = omar[3] + soma - topmar_epsilon
      par(mar = omar)
    }
  }
  par(oma = ooma)
  
  # determine legend inset
  inset = 0
  if (outer_side) {
    inset = grconvertX(lmar[1], from="lines", to="npc") - grconvertX(0, from = "lines", to = "npc")
    # extra space needed for "left!" b/c of lhs inner margin
    if (!outer_right) {
      inset_bump = grconvertX(par("mar")[2], from = "lines", to = "npc") - grconvertX(0, from = "lines", to = "npc")
      inset = inset + inset_bump
    }
    inset = c(1+inset, 0)
  } else if (outer_end) {
    inset = grconvertY(lmar[1], from="lines", to="npc") - grconvertY(0, from="lines", to="npc")
    if (outer_bottom) {
      # extra space needed for "bottom!" b/c of lhs inner margin
      inset_bump = grconvertY(par("mar")[1], from="lines", to="npc") - grconvertY(0, from="lines", to="npc")
      inset = inset + inset_bump
    } else {
      epsilon_bump = grconvertY(topmar_epsilon, from="lines", to="npc") - grconvertY(0, from="lines", to="npc")
      inset = inset + epsilon_bump
    }
    inset = c(0, 1+inset)
  }
  
  # GM: The legend inset spacing only works _exactly_ if we refresh the plot
  # area. I'm not sure why (and it works properly if we use the same
  # parameters manually while debugging), but this hack seems to work.
  ## v0.3.0 update: Using (temporary) hook instead of direct par(new = TRUE)
  ## assignment to play nice with tinytheme logic.
  oldhook = getHook("before.plot.new")
  setHook("before.plot.new", function() par(new = TRUE), action = "append")
  setHook("before.plot.new", function() par(mar = omar), action = "append")
  plot.new()
  setHook("before.plot.new", oldhook, action = "replace")
  
  # Finally, set the inset as part of the legend args.
  legend_args[["inset"]] = if (user_inset) legend_args[["inset"]] + inset else inset
  
  #
  ## Step 3: Draw the legend

  if (gradient) {
    if (!more_than_n_unique(legend_args[["col"]], 1)) {
      if (!is.null(legend_args[["pt.bg"]]) && length(legend_args[["pt.bg"]])==100) {
        legend_args[["col"]] = legend_args[["pt.bg"]]
      }
    }
    gradient_legend(
      legend_args = legend_args,
      fklgnd = fklgnd,
      lmar = lmar,
      outer_side = outer_side,
      outer_end = outer_end,
      outer_right = outer_right,
      outer_bottom = outer_bottom,
      user_inset = user_inset
    )
  } else {
    do.call("legend", legend_args)
  }
  
}


# gradient legend ----

# For gradient (i.e., continuous color) legends, we'll role our own bespoke
# legend function based on grDevices::as.raster

gradient_legend = function(legend_args, fklgnd, lmar, outer_side, outer_end, outer_right, outer_bottom, user_inset = FALSE) {
  pal = legend_args[["col"]]
  lgnd_labs = legend_args[["legend"]]
  if (!is.null(legend_args[["horiz"]])) horiz = legend_args[["horiz"]] else horiz = FALSE
  if (isTRUE(horiz)) {
    rasterlgd = as.raster(matrix(pal, nrow = 1))
  } else {
    rasterlgd = as.raster(matrix(rev(pal), ncol = 1))
  }
  
  corners = par("usr")
  rasterbox = rep(NA_real_, 4)
  
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
 
  if (inner) {
    
    fklgnd$rect$h = fklgnd$rect$h - (grconvertY(1.5 + 0.4, from="lines", to="user") - grconvertY(0, from="lines", to="user"))
    
    rasterbox[1] = fklgnd$rect$left
    if (isFALSE(inner_right)) rasterbox[1] = rasterbox[1] + (grconvertX(0.2, from="lines", to="user") - grconvertX(0, from="lines", to="user"))
    rasterbox[2] = fklgnd$rect$top - fklgnd$rect$h - (grconvertY(1.5 + 0.2, from="lines", to="user") - grconvertY(0, from="lines", to="user"))
    rasterbox[3] = rasterbox[1] + (grconvertX(1.25, from="lines", to="user") - grconvertX(0, from="lines", to="user"))
    rasterbox[4] = rasterbox[2] + fklgnd$rect$h
    
  } else if (outer_side) {
   
   rb1_adj = grconvertX(lmar[1] + 0.2, from="lines", to="user") - grconvertX(0, from="lines", to="user")
   rb3_adj = grconvertX(1.25, from="lines", to="user") - grconvertX(0, from="lines", to="user")
   rb2_adj = (corners[4] - corners[3] - (grconvertY(5+1 + 2.5, from="lines", to="user") - grconvertY(0, from="lines", to="user"))) / 2
   # override if top or bottom
   if (!is.null(legend_args[["x"]])) {
     if (grepl("^bottom", legend_args[["x"]])) {
       rb2_adj = corners[3]
     }
     if (grepl("^top", legend_args[["x"]])) {
       rb2_adj = corners[4] - (grconvertY(5+1 + 2.5, from="lines", to="user") - grconvertY(0, from="lines", to="user"))
     }
   }
   if (user_inset) rb2_adj = rb2_adj + legend_args[["inset"]][2] + 0.05
   rb4_adj = grconvertY(5+1, from="lines", to="user") - grconvertY(0, from="lines", to="user")
   
   if (outer_right) {
     rasterbox[1] = corners[2] + rb1_adj
     if (user_inset) rasterbox[1] = rasterbox[1] - (corners[2] - legend_args[["inset"]][1])/2
     rasterbox[2] = rb2_adj 
     rasterbox[3] = rasterbox[1] + rb3_adj
     rasterbox[4] = rasterbox[2] + rb4_adj
   } else {
     rb1_adj = rb1_adj + grconvertX(par("mar")[2] + 1, from="lines", to="user") - grconvertX(0, from="lines", to="user")
     rasterbox[1] = corners[1] - rb1_adj
     rasterbox[2] = rb2_adj 
     rasterbox[3] = rasterbox[1] - rb3_adj
     rasterbox[4] = rasterbox[2] + rb4_adj
   }
    
  } else if (outer_end) {
    
    rb1_adj = (corners[2] - corners[1] - (grconvertX(5+1, from="lines", to="user") - grconvertX(0, from="lines", to="user"))) / 2
    rb3_adj = grconvertX(5+1, from="lines", to="user") - grconvertX(0, from="lines", to="user")
    rb2_adj = grconvertY(lmar[1], from="lines", to="user") - grconvertY(0, from="lines", to="user")
    rb4_adj = grconvertY(1.25, from="lines", to="user") - grconvertY(0, from="lines", to="user")
    
    if (outer_bottom) {
      rb2_adj = rb2_adj + grconvertY(par("mar")[2], from="lines", to="user") - grconvertY(0, from="lines", to="user")
      rasterbox[1] = rb1_adj
      rasterbox[2] = corners[3] - rb2_adj 
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] - rb4_adj
    } else {
      rb2_adj = rb2_adj + grconvertY(1.25 + 1, from="lines", to="user") - grconvertY(0, from="lines", to="user")
      rasterbox[1] = rb1_adj
      rasterbox[2] = corners[4] + rb2_adj 
      rasterbox[3] = rasterbox[1] + rb3_adj
      rasterbox[4] = rasterbox[2] - rb4_adj
    }
    
  }
  
  #
  ## Draw the gradient swatch
  
  rasterImage(
    rasterlgd,
    rasterbox[1], #x1
    rasterbox[2], #y1
    rasterbox[3], #x2
    rasterbox[4], #y2
    xpd = NA
  ) 
  
  #
  ## Add the labels, tick marks, and title 
  
  if (isFALSE(horiz)) {
    labs_idx = !is.na(lgnd_labs)
    lgnd_labs[labs_idx] = paste0(" ", format(lgnd_labs[labs_idx]))
    lbl_x_anchor = rasterbox[3]
    ttl_x_anchor = rasterbox[1]
    lbl_adj = c(0, 0.5)
    tck_adj = c(1, 0.5)
    ttl_adj = c(0, 0)
    if (!inner && !outer_right) {
      lbl_x_anchor = rasterbox[1]
      ttl_x_anchor = ttl_x_anchor + max(strwidth(lgnd_labs[labs_idx]))
      ttl_adj = c(1, 0)
    }
    text(
      x = lbl_x_anchor,
      y = seq(rasterbox[2], rasterbox[4], length.out = length(lgnd_labs)),
      labels = lgnd_labs,
      xpd = NA, 
      adj = lbl_adj
    )
    # legend tick marks
    lgnd_ticks = lgnd_labs
    lgnd_ticks[labs_idx] = "-   -"
    text(
      x = lbl_x_anchor,
      y = seq(rasterbox[2], rasterbox[4], length.out = length(lgnd_labs)),
      labels = lgnd_ticks, col = "white",
      xpd = NA, adj = tck_adj
    )
    # legend title
    text(
      x = ttl_x_anchor,
      y = rasterbox[4] + grconvertY(1, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user"),
      labels = legend_args[["title"]],
      xpd = NA, adj = ttl_adj
    )
  } else {
    lbl_y_anchor = rasterbox[4]
    ttl_y_anchor = rasterbox[4]
    lbl_adj = c(0.5, 1.25)
    tck_adj = c(0, 0.5)
    ttl_adj = c(1, -0.5)
    # legend labs
    text(
      x = seq(rasterbox[1], rasterbox[3], length.out = length(lgnd_labs)),
      y = lbl_y_anchor,
      labels = lgnd_labs,
      xpd = NA, adj = lbl_adj
    )
    # legend tick marks
    lgnd_ticks = lgnd_labs
    lgnd_ticks[!is.na(lgnd_ticks)] = "-   -"
    text(
      x = seq(rasterbox[1], rasterbox[3], length.out = length(lgnd_labs)),
      y = lbl_y_anchor,
      labels = lgnd_ticks, col = "white",
      xpd = NA, adj = tck_adj, srt = 90
    )
    # legend title
    text(
      x = rasterbox[1],
      y = ttl_y_anchor,
      labels = paste0(legend_args[["title"]], " "),
      xpd = NA, adj = ttl_adj
    )
  }
}


# sanitize legend (helper function) ----

sanitize_legend = function(legend, legend_args) {
  if (is.null(legend_args[["x"]])) {
    if (is.null(legend)) {
      legend_args[["x"]] = "right!"
    } else if (is.character(legend)) {
      legend_args = utils::modifyList(legend_args, list(x = legend))
    } else if (class(legend) %in% c("call", "name")) {
      largs = as.list(legend)
      if (is.null(largs[["x"]])) {
        lnms = names(largs)
        # check second position b/c first will be a symbol 
        if (is.null(lnms)) {
          largs = stats::setNames(largs, c("", "x"))
        } else if (length(largs)>=2 && lnms[2] == "") {
          lnms[2] = "x"
          largs = stats::setNames(largs, lnms)
        } else {
          largs[["x"]] = "right!"
        }
      }
      # Finally, combine with any pre-existing legend args (e.g., title from the by label)
      legend_args = utils::modifyList(legend_args, largs, keep.null = TRUE)
    }
  }
  return(legend_args)
}