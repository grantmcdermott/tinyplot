#' @title Calculate placement of legend and and draw it
#'   
#' @description Internal function used to calculate the placement of (including
#'   outside the plotting area) and drawing of legend.
#'   
#' @md
#' @param legend Legend placement keyword or list, passed down from `plot2`.
#' @param legend.args Additional legend arguments to be passed to `legend()`.
#' @param by_dep The (deparsed) "by" grouping variable name.
#' @param lgnd_labs The labels passed to `legend(legend = ...)`.
#' @param type Plotting type(s), passed down from `plot2`.
#' @param pch Plotting character(s), passed down from `plot2`.
#' @param lty Plotting linetype(s), passed down from `plot2`.
#' @param col Plotting colour(s), passed down from `plot2`.
#' @param bg Plotting character background fill colour(s), passed down from `plot2`.
#' @param cex Plotting character expansion(s), passed down from `plot2`.
#' @param lmar Legend margins (in lines). Should be a numeric vector of length
#'   two, where the first number represents the "inner" margin between the
#'   legend and the plot, and the second number represents the "outer" margin
#'   between the legend and edge of the graphics device. If no explicit value is
#'   provided by the user, then reverts back to `par2("lmar")` for which the
#'   default values are c(0.6, 0.5).
#' @param has_sub Logical. Does the plot have a sub-caption. Only used if
#'   keyword position is "bottom!", in which case we need to bump the legend
#'   margin a bit further.
#' @param new_plot Should we be calling plot.new internally?
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
    lmar = par2("lmar")
  } else {
    if (!is.numeric(lmar) || length(lmar)!=2) stop ("lmar must be a numeric of length 2.")
  }
  
  w = h = outer_right = outer_bottom = NULL
  
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
  
  # Catch to avoid recursive offsets, e.g., repeated plot2 calls with
  # "bottom!" legend position.
  par(omd = c(0,1,0,1))
  
  ## Legend to outer side of plot
  if (grepl("right!$|left!$", legend.args[["x"]])) {
    
    outer_right = grepl("right!$", legend.args[["x"]])
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_right) legend.args[["x"]] = gsub("right!$", "left", legend.args[["x"]])
    if (!outer_right) legend.args[["x"]] = gsub("left!$", "right", legend.args[["x"]])
    
    ## We have to set the inner margins of the plot before the (fake) legend is
    ## drawn, otherwise the inset calculation---which is based in the legend
    ## width---will be off the first time.
    if (outer_right) {
      par(mar=c(par("mar")[1:3], lmar[1])) 
    } else {
      par(mar=c(par("mar")[1], lmar[1] + sum(par("mgp")), par("mar")[3:4]))
    }
    
    if (isTRUE(new_plot)) plot.new()
    
    legend.args[["horiz"]] = FALSE
    
    # "draw" fake legend
    fklgnd.args = utils::modifyList(
      legend.args,
      list(x = 0, y = 0, plot = FALSE),
      keep.null = TRUE
    )
    fklgnd = do.call("legend", fklgnd.args)
    
    # calculate outer (side) margin width in lines
    soma = grconvertX(fklgnd$rect$w, to="lines") - grconvertX(0, to="lines")
    # Add additional space to the side (i.e. as part of the outer margin)
    soma = soma + lmar[2] 
    
    winset = grconvertX(lmar[1], from="lines", to="nic") ## nic since omd has changed?
    ooma = par("oma")
    ## differing adjustments depending on side
    if (outer_right) {
      ooma[4] = soma
    } else {
      # extra space needed for outer_left b/c of y-axis title and axis labels
      ytisp = grconvertX(sum(par("mgp")) + lmar[1], from = "lines", to = "nic")
      winset = winset + ytisp
      ooma[2] = soma
    }
    par(oma = ooma)
    legend.args[["inset"]] = c(1+winset, 0)
    
    ## Legend at the outer top or bottom of plot
  } else if (grepl("bottom!$|top!$", legend.args[["x"]])) {
    
    outer_bottom = grepl("bottom!$", legend.args[["x"]])
    
    # Catch to reset right margin if previous legend position was "right!" or "left!"
    # GM: do we still want this here?
    if (par("mar")[4]== lmar[2]) par(mar=c(par("mar")[1:3], 2.1)) 
    if (par("mar")[2]== lmar[2]) par(mar=c(par("mar")[1], 4.1), par("mar")[3:4]) 
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_bottom) legend.args[["x"]] = gsub("bottom!$", "top", legend.args[["x"]])
    if (!outer_bottom) legend.args[["x"]] = gsub("top!$", "bottom", legend.args[["x"]])
    
    ## We have to set the inner margins of the plot before the (fake) legend is
    ## drawn, otherwise the inset calculation---which is based in the legend
    ## width---will be off the first time.
    if (outer_bottom) {
      par(mar=c(lmar[1], par("mar")[2:4]))
      if (isTRUE(has_sub)) {
        par(mar = c(lmar[1] + sum(par("mgp")) + 1, par("mar")[2:4]))
      } else {
        par(mar = c(lmar[1] + sum(par("mgp")), par("mar")[2:4]))
      }
    } else {
      par(mar = c(par("mar")[1:2], lmar[1] + sum(par("mgp")), par("mar")[4]))
    }
    
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
      list(x = 0, y = 0, plot = FALSE),
      keep.null = TRUE
    )
    fklgnd = do.call("legend", fklgnd.args)
    
    # calculate outer (side) margin width in lines
    soma = grconvertY(fklgnd$rect$h, to="lines") - grconvertY(0, to="lines")
    # Add additional space to the side (i.e. as part of the outer margin)
    soma = soma + lmar[2] 
    
    hinset = grconvertY(lmar[1], from="lines", to="nic") ## nic since omd has changed?
    ooma = par("oma")
    
    ## differing adjustments depending on side
    if (outer_bottom) {
      # extra space needed for outer_bottom b/c of x-axis title and axis labels
      xtisp = grconvertY(sum(par("mgp")) + 1 + lmar[1], from = "lines", to = "nic")
      hinset = hinset + xtisp
      if (has_sub) hinset = hinset + grconvertY(1 + 0.5, from = "lines", to = "nic")
      ooma[1] = soma
    } else {
      # extra space needed for outer_top b/c of x-axis title and axis labels
      # ## GM: FLAG TO CHANGE
      xtisp = grconvertY(sum(par("mgp")) + 1 + lmar[1], from = "lines", to = "nic")
      hinset = hinset + xtisp
      ooma[3] = soma
    }
    par(oma = ooma)
    legend.args[["inset"]] = c(0, 1+hinset)
    
  } else {
    # Catch to reset margins if previous legend position were "!" outside
    if (par("mar")[1] == lmar[2]) par(mar=c(5.1, par("mar")[2:4])) 
    if (par("mar")[2] == lmar[2]) par(mar=c(par("mar")[1], 4.1, par("mar")[3:4])) 
    if (par("mar")[3] == lmar[2]) par(mar=c(par("mar")[1:2], 4.1, par("mar")[4])) 
    if (par("mar")[4] == lmar[2]) par(mar=c(par("mar")[1:3], 2.1)) 
    legend.args[["inset"]] = 0
    if (isTRUE(new_plot)) plot.new()
  }
  
  do.call("legend", legend.args)
  
}






