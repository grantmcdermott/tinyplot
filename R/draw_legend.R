#' @title Calculate placement of legend and and draw it
#'   
#' @description Internal function used to calculate the placement of (including
#'   outside the plotting area) and drawing of legend.
#'   
#' @md
#' @param legend Legend placement keyword or list, passed down from `tinyplot`.
#' @param legend.args Additional legend arguments to be passed to `legend()`.
#' @param by_dep The (deparsed) "by" grouping variable name.
#' @param lgnd_labs The labels passed to `legend(legend = ...)`.
#' @param type Plotting type(s), passed down from `tinyplot`.
#' @param pch Plotting character(s), passed down from `tinyplot`.
#' @param lty Plotting linetype(s), passed down from `tinyplot`.
#' @param col Plotting colour(s), passed down from `tinyplot`.
#' @param bg Plotting character background fill colour(s), passed down from `tinyplot`.
#' @param cex Plotting character expansion(s), passed down from `tinyplot`.
#' @param lmar Legend margins (in lines). Should be a numeric vector of the form
#'   `c(inner, outer)`, where the first number represents the "inner" margin
#'   between the legend and the plot, and the second number represents the
#'   "outer" margin between the legend and edge of the graphics device. If no
#'   explicit value is provided by the user, then reverts back to `tpar("lmar")`
#'   for which the default values are `c(1.0, 0.1)`.
#' @param has_sub Logical. Does the plot have a sub-caption. Only used if
#'   keyword position is "bottom!", in which case we need to bump the legend
#'   margin a bit further.
#' @param new_plot Should we be calling plot.new internally?
#' @examples
#' 
#' oldmar = par("mar")
#' 
#' draw_legend(
#'   legend = "right!", ## default (other options incl, "left(!)", ""bottom(!)", etc.)
#'   legend.args = list(title = "Key", bty = "o"),
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
#'   legend.args = list(title = "Key", bty = "o"),
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
#' @export
draw_legend = function(
    legend = NULL,
    legend.args = NULL,
    by_dep = NULL,
    lgnd_labs = NULL,
    type = NULL,
    pch = NULL,
    lty = NULL,
    col = NULL,
    bg = NULL,
    cex = NULL,
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
  
  if (is.null(legend)) {
    legend.args[["x"]] = "right!"
  } else if (is.character(legend)) {
    legend.args = utils::modifyList(legend.args, list(x = legend))
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
    legend.args = utils::modifyList(legend.args, largs, keep.null = TRUE)
  }
  
  ## Use `!exists` rather than `is.null` for title in case user specified no title
  if (!exists("title", where = legend.args)) legend.args[["title"]] = by_dep
  if (is.null(legend.args[["pch"]])) legend.args[["pch"]] = pch
  if (is.null(legend.args[["lty"]])) legend.args[["lty"]] = lty
  if (is.null(legend.args[["col"]])) legend.args[["col"]] = col
  if (is.null(legend.args[["bty"]])) legend.args[["bty"]] = "n"
  if (is.null(legend.args[["horiz"]])) legend.args[["horiz"]] = FALSE
  if (is.null(legend.args[["xpd"]])) legend.args[["xpd"]] = NA
  if (is.null(legend.args[["pt.bg"]])) legend.args[["pt.bg"]] = bg
  if (
    type %in% c("p", "pointrange", "errorbar") &&
    (length(col) == 1 || length(cex) == 1) &&
    is.null(legend.args[["pt.cex"]])
  ) {
    legend.args[["pt.cex"]] = cex
  }
  if (type=="ribbon") {
    if (is.null(legend.args[["pch"]])) legend.args[["pch"]] = 22
    if (is.null(legend.args[["pt.cex"]])) legend.args[["pt.cex"]] = 3.5
    if (is.null(legend.args[["pt.lwd"]])) legend.args[["pt.lwd"]] = 0
    if (is.null(legend.args[["y.intersp"]])) legend.args[["y.intersp"]] = 1.25
    if (is.null(legend.args[["seg.len"]])) legend.args[["seg.len"]] = 1.25
  }
  
  
  if (is.null(legend.args[["legend"]])) {
    legend.args[["legend"]] = lgnd_labs
  } else if (length(lgnd_labs) != length(eval(legend.args[["legend"]]))) {
    warning(
      "\nUser-supplied legend labels do not match the number of groups.\n",
      "Defaulting to automatic labels determined by the group splits in `by`,\n"
    )
    legend.args[["legend"]] = lgnd_labs
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
  if (grepl("right!$|left!$", legend.args[["x"]])) {
    
    outer_right = grepl("right!$", legend.args[["x"]])
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_right) legend.args[["x"]] = gsub("right!$", "left", legend.args[["x"]])
    if (!outer_right) legend.args[["x"]] = gsub("left!$", "right", legend.args[["x"]])
    
    ## We have to set the inner margins of the plot before the (fake) legend is
    ## drawn, otherwise the inset calculation---which is based in the legend
    ## width---will be off the first time.
    if (outer_right) {
      # par(mar=c(par("mar")[1:3], 0)) ## Set rhs inner mar to zero
      omar[4] = 0 ## TEST
    } else {
      # For outer left we have to account for the y-axis label too, which
      # requires additional space
      # par(mar=c(
      #   par("mar")[1],
      #   par("mgp")[1] + 1*par("cex.lab"),
      #   par("mar")[3:4]
      #   ))
      omar[2] = par("mgp")[1] + 1*par("cex.lab") ## TEST
    }
    par(mar = omar) ## TEST
    
    if (isTRUE(new_plot)) plot.new()
    
    legend.args[["horiz"]] = FALSE
    
    # "draw" fake legend
    fklgnd.args = utils::modifyList(
      legend.args,
      list(x = 0, y = 0, plot = FALSE),
      keep.null = TRUE
    )
    fklgnd = do.call("legend", fklgnd.args)
    
    # calculate outer margin width in lines
    soma = grconvertX(fklgnd$rect$w, to="lines") - grconvertX(0, to="lines")
    # Add legend margins to the outer margin
    soma = soma + sum(lmar)
    # ooma = par("oma")  ## TEST (comment)
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
    legend.args[["inset"]] = c(1+inset, 0)
    
    ## Legend at the outer top or bottom of plot
  } else if (grepl("bottom!$|top!$", legend.args[["x"]])) {
    
    outer_bottom = grepl("bottom!$", legend.args[["x"]])
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_bottom) legend.args[["x"]] = gsub("bottom!$", "top", legend.args[["x"]])
    if (!outer_bottom) legend.args[["x"]] = gsub("top!$", "bottom", legend.args[["x"]])
    
    ## We have to set the inner margins of the plot before the (fake) legend is
    ## drawn, otherwise the inset calculation---which is based in the legend
    ## width---will be off the first time.
    if (outer_bottom) {
      omar[1] = par("mgp")[1] + 1*par("cex.lab") ## TEST
      if (isTRUE(has_sub)) omar[1] = omar[1] + 1*par("cex.sub") ## TEST 
    } else {
      ## For "top!", the logic is slightly different: We don't expand the outer
      ## margin b/c we need the legend to come underneath the main title. So
      ## we rather expand the existing inner margin.
      ooma[3] = ooma[3] + topmar_epsilon ## TESTING
      par(oma = ooma)
    }
    par(mar = omar)
    
    if (isTRUE(new_plot)) plot.new()
    
    legend.args[["horiz"]] = TRUE
    
    # Catch for horizontal ribbon legend spacing
    if (type=="ribbon" && isTRUE(legend.args[["horiz"]])) {
      if (legend.args[["pt.lwd"]] == 1) {
        legend.args[["x.intersp"]] = 1
      } else {
        legend.args[["x.intersp"]] = 0.5
      }
    }
    
    # "draw" fake legend
    fklgnd.args = utils::modifyList(
      legend.args,
      # list(x = 0, y = 0, plot = FALSE),
      list(plot = FALSE),
      keep.null = TRUE
    )
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
    legend.args[["inset"]] = c(0, 1+inset)
    
  } else {
    legend.args[["inset"]] = 0
    if (isTRUE(new_plot)) plot.new()
  }
  
  do.call("legend", legend.args)
  
}






