#' @title Calculate placement of legend and draw it
#'   
#' @description Internal function used to calculate the placement of (including
#'   outside the plotting area) and drawing of legend.
#'   
#' @md
#' @param legend Legend placement keyword or list, passed down from [tinyplot].
#' @param legend_args Additional legend arguments to be passed to `legend()`.
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
#' 
#' @returns No return value, called for side effect of producing a(n empty) plot
#'   with a legend in the margin.
#' 
#' @importFrom graphics grconvertX grconvertY rasterImage strwidth
#' @importFrom grDevices as.raster recordGraphics
#' @importFrom utils modifyList
#' 
#' @examples
#' 
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
    new_plot = TRUE
) {
  
  if (is.null(lmar)) {
    lmar = tpar("lmar")
  } else {
    if (!is.numeric(lmar) || length(lmar)!=2) stop ("lmar must be a numeric of length 2.")
  }
  
  soma = outer_right = outer_bottom = NULL
  
  #
  ## legend args ----
  
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
  
  ## Use `!exists` rather than `is.null` for title in case user specified no title
  if (!exists("title", where = legend_args)) legend_args[["title"]] = by_dep
  if (is.null(legend_args[["pch"]])) legend_args[["pch"]] = pch
  if (is.null(legend_args[["lty"]])) legend_args[["lty"]] = lty
  if (!is.null(type) && !(type %in% c("p", "ribbon", "polygon"))) {
    if (is.null(legend_args[["lwd"]])) legend_args[["lwd"]] = lwd
  }
  if (is.null(legend_args[["col"]])) legend_args[["col"]] = col
  if (is.null(legend_args[["bty"]])) legend_args[["bty"]] = "n"
  if (is.null(legend_args[["horiz"]])) legend_args[["horiz"]] = FALSE
  if (is.null(legend_args[["xpd"]])) legend_args[["xpd"]] = NA
  if (is.null(legend_args[["pt.bg"]])) legend_args[["pt.bg"]] = bg
  if (
    type %in% c("p", "pointrange", "errorbar") &&
    (length(col) == 1 || length(cex) == 1) &&
    is.null(legend_args[["pt.cex"]])
  ) {
    legend_args[["pt.cex"]] = cex
  }
  if (type %in% c("ribbon", "polygon") || isTRUE(gradient)) {
    if (is.null(legend_args[["pch"]])) legend_args[["pch"]] = 22
    if (is.null(legend_args[["pt.cex"]])) legend_args[["pt.cex"]] = 3.5
    if (is.null(legend_args[["pt.lwd"]]) && (!is.null(type) && type != "polygon")) {
      legend_args[["pt.lwd"]] = 0
    }
    if (is.null(legend_args[["y.intersp"]])) legend_args[["y.intersp"]] = 1.25
    if (is.null(legend_args[["seg.len"]])) legend_args[["seg.len"]] = 1.25
  }
  
  
  if (is.null(legend_args[["legend"]])) {
    legend_args[["legend"]] = lgnd_labs
  } else if (length(lgnd_labs) != length(eval(legend_args[["legend"]]))) {
    warning(
      "\nUser-supplied legend labels do not match the number of groups.\n",
      "Defaulting to automatic labels determined by the group splits in `by`,\n"
    )
    legend_args[["legend"]] = lgnd_labs
  }
  
  #
  ## legend placement ----
  
  ooma = par("oma")
  omar = par("mar")
  topmar_epsilon = 0.1
  
  # Catch to avoid recursive offsets, e.g. repeated tinyplot calls with
  # "bottom!" legend position.
  
  ## restore inner margin defaults
  ## (in case the plot region/margins were affected by the preceding tinyplot call)
  if (any(ooma != 0)) {
    if ( ooma[1] != 0 & omar[1] == par("mgp")[1] + 1*par("cex.lab") ) omar[1] = 5.1
    if ( ooma[2] != 0 & omar[2] == par("mgp")[1] + 1*par("cex.lab") ) omar[2] = 4.1
    if ( ooma[3] == topmar_epsilon & omar[3] != 4.1 ) omar[3] = 4.1
    if ( ooma[4] != 0 & omar[4] == 0 ) omar[4] = 2.1
    par(mar = omar)
  }
  ## restore outer margin defaults
  par(omd = c(0,1,0,1))
  ooma = par("oma")
  
  
  ## Legend to outer side (either right or left) of plot
  if (grepl("right!$|left!$", legend_args[["x"]])) {
    
    outer_right = grepl("right!$", legend_args[["x"]])
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_right) legend_args[["x"]] = gsub("right!$", "left", legend_args[["x"]])
    if (!outer_right) legend_args[["x"]] = gsub("left!$", "right", legend_args[["x"]])
    
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
    
    if (isTRUE(new_plot)) plot.new()
    
    legend_args[["horiz"]] = FALSE
    
    # "draw" fake legend
    fklgnd.args = modifyList(
      legend_args,
      list(x = 0, y = 0, plot = FALSE),
      keep.null = TRUE
    )
    if (isTRUE(gradient)) {
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
    }
    fklgnd = do.call("legend", fklgnd.args)
    
    # calculate outer margin width in lines
    soma = grconvertX(fklgnd$rect$w, to="lines") - grconvertX(0, to="lines")
    # Add legend margins to the outer margin
    soma = soma + sum(lmar)
    ## differing outer margin adjustments depending on side
    if (outer_right) {
      ooma[4] = soma
    } else {
      ooma[2] = soma
    }
    par(oma = ooma)
    
    # determine legend inset
    inset = grconvertX(lmar[1], from="lines", to="npc") - grconvertX(0, from = "lines", to = "npc")
    if (isFALSE(outer_right)) {
      # extra space needed for "left!" b/c of lhs inner margin
      inset_bump = grconvertX(par("mar")[2], from = "lines", to = "npc") - grconvertX(0, from = "lines", to = "npc")
      inset = inset + inset_bump
    }
    # GM: The legend inset spacing only works _exactly_ if we refresh the plot
    # area. I'm not sure why (and it works properly if we use the same
    # parameters manually while debugging), but this hack seems to work.
    par(new = TRUE)
    plot.new()
    par(new = FALSE)
    # Finally, set the inset as part of the legend args.
    legend_args[["inset"]] = c(1+inset, 0)
    
    ## Legend at the outer top or bottom of plot
  } else if (grepl("bottom!$|top!$", legend_args[["x"]])) {
    
    outer_bottom = grepl("bottom!$", legend_args[["x"]])
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_bottom) legend_args[["x"]] = gsub("bottom!$", "top", legend_args[["x"]])
    if (!outer_bottom) legend_args[["x"]] = gsub("top!$", "bottom", legend_args[["x"]])
    
    ## We have to set the inner margins of the plot before the (fake) legend is
    ## drawn, otherwise the inset calculation---which is based in the legend
    ## width---will be off the first time.
    if (outer_bottom) {
      omar[1] = par("mgp")[1] + 1*par("cex.lab")
      if (isTRUE(has_sub)) omar[1] = omar[1] + 1*par("cex.sub")
    } else {
      ## For "top!", the logic is slightly different: We don't expand the outer
      ## margin b/c we need the legend to come underneath the main title. So
      ## we rather expand the existing inner margin.
      ooma[3] = ooma[3] + topmar_epsilon
      par(oma = ooma)
    }
    par(mar = omar)
    
    if (isTRUE(new_plot)) plot.new()
    
    legend_args[["horiz"]] = TRUE
    
    # Catch for horizontal ribbon legend spacing
    if (type=="ribbon" && isTRUE(legend_args[["horiz"]])) {
      if (legend_args[["pt.lwd"]] == 1) {
        legend_args[["x.intersp"]] = 1
      } else {
        legend_args[["x.intersp"]] = 0.5
      }
    } else if (isTRUE(gradient) && isTRUE(legend_args[["horiz"]])) {
      legend_args[["x.intersp"]] = 0.5
    }
    
    # "draw" fake legend
    fklgnd.args = modifyList(
      legend_args,
      list(plot = FALSE),
      keep.null = TRUE
    )
    if (isTRUE(gradient)) {
      lgnd_labs_tmp = na.omit(fklgnd.args[["legend"]])
      if (length(lgnd_labs_tmp) < 5L) {
        nmore = 5L - length(lgnd_labs_tmp)
        lgnd_labs_tmp = c(lgnd_labs_tmp, rep("", nmore))
      }
      fklgnd.args = modifyList(
        fklgnd.args,
        list(legend = lgnd_labs_tmp, title = NULL),
        keep.null = TRUE
      )
    }
    fklgnd = do.call("legend", fklgnd.args)
    
    # calculate outer margin width in lines
    soma = grconvertY(fklgnd$rect$h, to="lines") - grconvertY(0, to="lines")
    # Add legend margins to outer margin
    soma = soma + sum(lmar)
    ## differing outer margin adjustments depending on side
    if (outer_bottom) {
      ooma[1] = soma
    } else {
      omar[3] = omar[3] + soma - topmar_epsilon
      par(mar = omar)
    }
    par(oma = ooma)
    
    # determine legend inset
    inset = grconvertY(lmar[1], from="lines", to="npc") - grconvertY(0, from="lines", to="npc")
    if (isTRUE(outer_bottom)) {
      # extra space needed for "bottom!" b/c of lhs inner margin
      inset_bump = grconvertY(par("mar")[1], from="lines", to="npc") - grconvertY(0, from="lines", to="npc")
      inset = inset + inset_bump
    } else {
      epsilon_bump = grconvertY(topmar_epsilon, from="lines", to="npc") - grconvertY(0, from="lines", to="npc")
      inset = inset + epsilon_bump
    }
    # GM: The legend inset spacing only works _exactly_ if we refresh the plot
    # area. I'm not sure why (and it works properly if we use the same
    # parameters manually while debugging), but this hack seems to work.
    par(new = TRUE)
    plot.new()
    par(new = FALSE)
    # Finally, set the inset as part of the legend args.
    legend_args[["inset"]] = c(0, 1+inset)
    
  } else {
    legend_args[["inset"]] = 0
    if (isTRUE(new_plot)) plot.new()
  }
  
  # Finally, plot the legend. Note that we use recordGraphics to preserve the
  # legend spacing if the plot is resized.
  if (isTRUE(gradient)) {
    if (!more_than_n_unique(legend_args[["col"]], 1)) {
      if (!is.null(legend_args[["pt.bg"]]) && length(legend_args[["pt.bg"]])==100) {
        legend_args[["col"]] = legend_args[["pt.bg"]]
      }
    }
    recordGraphics(
      gradient_legend(legend_args = legend_args, lmar = lmar, outer_right = outer_right, outer_bottom = outer_bottom),
      list(legend_args = legend_args, lmar = lmar, outer_right = outer_right, outer_bottom = outer_bottom),
      getNamespace("tinyplot")
    )
  } else {
    recordGraphics(
      do.call("legend", legend_args),
      list(legend_args = legend_args),
      getNamespace("tinyplot")
    )
  }
  
}





# For gradient (i.e., continuous color) legends, we'll role our own bespoke
# legend function based on grDevices::as.raster
gradient_legend = function(legend_args, lmar = NULL, outer_right = NULL, outer_bottom = NULL) {
  if (is.null(lmar)) lmar = .tpar[["lmar"]]
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
  
  # catch for "inner" legends
  inner = FALSE
  inner_right = inner_bottom = NULL
  if (is.null(outer_right) && is.null(outer_bottom)) {
    inner = TRUE
    if (!is.null(legend_args[["x"]]) && grepl("left$|right$", legend_args[["x"]])) {
      inner_right = grepl("right$", legend_args[["x"]])
    }
    if (!is.null(legend_args[["x"]]) && grepl("^bottoml|^top", legend_args[["x"]])) {
      inner_bottom = grepl("^bottom", legend_args[["x"]])
    }
  }
 
 if (!is.null(outer_right)) {
   
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
   rb4_adj = grconvertY(5+1, from="lines", to="user") - grconvertY(0, from="lines", to="user")
   
   if (isTRUE(outer_right)) {
     rasterbox[1] = corners[2] + rb1_adj
     rasterbox[2] = rb2_adj 
     rasterbox[3] = rasterbox[1] + rb3_adj
     rasterbox[4] = rasterbox[2] + rb4_adj
   } else if (isFALSE(outer_right)) {
     rb1_adj = rb1_adj + grconvertX(par("mar")[2] + 1, from="lines", to="user") - grconvertX(0, from="lines", to="user")
     rasterbox[1] = corners[1] - rb1_adj
     rasterbox[2] = rb2_adj 
     rasterbox[3] = rasterbox[1] - rb3_adj
     rasterbox[4] = rasterbox[2] + rb4_adj
   }
    
  } else if (!is.null(outer_bottom)) {
    
    rb1_adj = (corners[2] - corners[1] - (grconvertX(5+1, from="lines", to="user") - grconvertX(0, from="lines", to="user"))) / 2
    rb3_adj = grconvertX(5+1, from="lines", to="user") - grconvertX(0, from="lines", to="user")
    rb2_adj = grconvertY(lmar[1], from="lines", to="user") - grconvertY(0, from="lines", to="user")
    rb4_adj = grconvertY(1.25, from="lines", to="user") - grconvertY(0, from="lines", to="user")
    
    if (isTRUE(outer_bottom)) {
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
    
  } else if (isTRUE(inner)) {
    
    # "draw" fake legend
    lgnd_labs_tmp = na.omit(legend_args[["legend"]])
    if (length(lgnd_labs_tmp) < 5L) {
      nmore = 5L - length(lgnd_labs_tmp)
      lgnd_labs_tmp = c(lgnd_labs_tmp, rep("", nmore))
    }
    fklgnd.args = modifyList(
      legend_args,
      list(plot = FALSE, legend = lgnd_labs_tmp),
      keep.null = TRUE
    )
    fklgnd = do.call("legend", fklgnd.args)
    fklgnd$rect$h = fklgnd$rect$h - (grconvertY(1.5 + 0.4, from="lines", to="user") - grconvertY(0, from="lines", to="user"))
    
    rasterbox[1] = fklgnd$rect$left
    if (isFALSE(inner_right)) rasterbox[1] = rasterbox[1] + (grconvertX(0.2, from="lines", to="user") - grconvertX(0, from="lines", to="user"))
    rasterbox[2] = fklgnd$rect$top - fklgnd$rect$h - (grconvertY(1.5 + 0.2, from="lines", to="user") - grconvertY(0, from="lines", to="user"))
    rasterbox[3] = rasterbox[1] + (grconvertX(1.25, from="lines", to="user") - grconvertX(0, from="lines", to="user"))
    rasterbox[4] = rasterbox[2] + fklgnd$rect$h
    
  }
  
  rasterImage(
    rasterlgd,
    rasterbox[1], #x1
    rasterbox[2], #y1
    rasterbox[3], #x2
    rasterbox[4], #y2
    xpd = NA
  ) 
  
  if (isFALSE(horiz)) {
    labs_idx = !is.na(lgnd_labs)
    lgnd_labs[labs_idx] = paste0(" ", lgnd_labs[labs_idx])
    lbl_x_anchor = rasterbox[3]
    ttl_x_anchor = rasterbox[1]
    lbl_adj = c(0, 0.5)
    tck_adj = c(1, 0.5)
    ttl_adj = c(0, 0)
    if (isFALSE(outer_right)) {
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


