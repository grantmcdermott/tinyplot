#' @title Calculate placement of legend and and draw it
#'   
#' @description Internal function used to calculate the placement of (including
#'   outside the plotting area) and drawing of legend.

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
    cex = NULL
) {
  
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
    
    # Margins of the plot (the first is the bottom margin)
    if (outer_right) {
      par(mar=c(par("mar")[1:3], 0.1)) # remove right inner margin space
    } else if (par("mar")[4]==0.1) {
      par(mar=c(par("mar")[1:3], 2.1)) # revert right margin if outer left
    }
    
    plot.new()
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_right) legend.args[["x"]] = gsub("right!$", "left", legend.args[["x"]])
    if (!outer_right) legend.args[["x"]] = gsub("left!$", "right", legend.args[["x"]])
    
    legend.args[["horiz"]] = FALSE
    
    lgnd = legend(
      0, 0, 
      bty    = legend.args[["bty"]],
      legend = legend.args[["legend"]],
      pch    = legend.args[["pch"]],
      lty    = legend.args[["lty"]],
      col    = legend.args[["col"]],
      title  = legend.args[["title"]],
      xpd    = legend.args[["xpd"]],
      plot   = FALSE
    )
    # calculate side margin width in ndc
    w = grconvertX(lgnd$rect$w, to="ndc") - grconvertX(0, to="ndc")
    ## differing adjustments depending on side
    if (outer_right) {
      w = w*1.5
      par(omd = c(0, 1-w, 0, 1))
      legend.args[["inset"]] = c(1.025, 0)
    } else {
      w = w + grconvertX(par("mgp")[1], from = "lines", to = "ndc") # extra space for y-axis title
      par(omd = c(w, 1, 0, 1))
      legend.args[["inset"]] = c(1+w, 0)
    }
    
    ## Legend at the outer top or bottom of plot
  } else if (grepl("bottom!$|top!$", legend.args[["x"]])) {
    
    outer_bottom = grepl("bottom!$", legend.args[["x"]])
    
    # Catch to reset right margin if previous legend position was "right!"
    if (par("mar")[4]== 0.1) par(mar=c(par("mar")[1:3], 2.1)) 
    
    plot.new()
    
    ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
    if (outer_bottom) legend.args[["x"]] = gsub("bottom!$", "top", legend.args[["x"]])
    if (!outer_bottom) legend.args[["x"]] = gsub("top!$", "bottom", legend.args[["x"]])
    
    legend.args[["horiz"]] = TRUE
    
    # Catch for horizontal ribbon legend spacing
    if (type=="ribbon" && isTRUE(legend.args[["horiz"]])) {
      if (legend.args[["pt.lwd"]] == 1) {
        legend.args[["x.intersp"]] = 1
      } else {
        legend.args[["x.intersp"]] = 0.5
      }
    }
    
    lgnd = legend(
      0, 0,
      bty    = legend.args[["n"]],
      legend = legend.args[["legend"]],
      horiz  = legend.args[["horiz"]],
      pch    = legend.args[["pch"]],
      lty    = legend.args[["lty"]],
      col    = legend.args[["col"]],
      title  = legend.args[["title"]],
      plot   = FALSE
    )
    # calculate bottom margin height in ndc
    h = grconvertX(lgnd$rect$h, to="ndc") - grconvertX(0, to="ndc")
    ## differing adjustments depending on side
    if (outer_bottom) {
      legend.args[["inset"]] = c(0, 1+2*h)
      par(omd = c(0,1,0+h,1))
    } else {
      legend.args[["inset"]] = c(0, 1)
      par(omd = c(0,1,0,1-h))
    }
    
  } else {
    # Catch to reset right margin if previous legend position was "right!"
    if (par("mar")[4] == 0.1) par(mar=c(par("mar")[1:3], par("mar")[2]-2)) 
    legend.args[["inset"]] = 0
    plot.new()
  }
  
  do.call("legend", legend.args)
  
}






