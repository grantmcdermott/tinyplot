plot2 =
  function(x, y, ...) {
    UseMethod("plot2")
  }


plot2.default = function(
    x,
    y = NULL,
    by = NULL,
    data = NULL,
    type = "p",
    xlim = NULL,
    ylim = NULL,
    # log = "",
    main = NULL, 
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = axes,
    asp = NA,
    grid = NULL,
    palette = NULL,
    # palette.args = NULL,
    legend.position = c("auto", "bottom", "right", "none"),
    pch = NULL,
    ...) {
  
  legend.position = match.arg(legend.position)
  
  if (is.null(y)) {
    y = x
    xlab = "Index"
    }
  
  if (is.null(xlab)) xlab = deparse(substitute(x))
  if (is.null(ylab)) ylab = deparse(substitute(y))
  
    
  if (is.null(xlim)) xlim = range(x)
  if (is.null(ylim)) ylim = range(y)
  
  if (!is.null(by)) {
    split_data = lapply(list(x=x, y=y), split, by)
    split_data = do.call(function(...) Map("list", ...), split_data)
  } else {
    split_data = list(list(x=x, y=y))
  }
    
  # colour palette
  if (is.null(palette)) palette = "Dark 2"
  cols = hcl.colors(length(split_data), palette = palette)
  
  # Save current graphical parameters
  opar = par(no.readonly = TRUE)
  
  # legend

  if (legend.position=="auto") {
    legend.position = ifelse(length(split_data)>1, "bottom", "none")
  }
  
  if (legend.position!="none") {
    
    if (length(split_data)>1) {
      legend = names(split_data)
    } else {
      legend = ylab
    }
    
    pch_type = lty_type = NULL
    if (type %in% c("p", "b", "o")) pch_type = ifelse(!is.null(pch), pch, 1)
    if (type %in% c("l", "b", "o")) lty_type = 1
    
    if (legend.position=="bottom") {
      
      # Margins of the plot (the first is the bottom margin)
      # par(mar=c(0.1, par('mar')[2:4])) # optional, removes bottom inner margin space
      plot.new()
      
      pos_anchor = "top"
      horiz = TRUE
      
      l = legend(
        0, 0, bty = "n", legend = legend,
        horiz = horiz,
        pch = pch_type, lty = lty_type,
        plot = FALSE
      )
      # calculate bottom margin height in ndc
      h = grconvertX(l$rect$h, to="ndc") - grconvertX(0, to="ndc")
      inset = c(0, 1+2.5*h)
      par(omd = c(0,1,0+h,1))
      

    } else if (legend.position=="right") {
      
      # Margins of the plot (the first is the bottom margin)
      par(mar=c(par("mar")[1:3], 0.1)) # remove right inner margin space
    
      plot.new()
      
      pos_anchor = "left"
      horiz = FALSE
      
      l = legend(
        0, 0, bty = "n", legend = legend,
        pch = pch_type, lty = lty_type,
        plot = FALSE
      )
      # calculate right margin width in ndc
      w = grconvertX(l$rect$w, to="ndc") - grconvertX(0, to="ndc")
      inset = c(1, 0)
      par(omd = c(0, 1-w, 0, 1))
      
    }
    
    legend(
      pos_anchor, inset = inset,
      bty="n",
      legend = legend,
      xpd = NA,
      horiz = horiz,
      pch = pch_type, lty = lty_type,
      col = cols
    )
    
  } else {
    
    plot.new()
    
  }
  
  # plot window
  plot.window(
    xlim = xlim, ylim = ylim, 
    asp = asp,
    ...
  )
  
  # axes, plot.frame and grid
  if (axes) axis(1); axis(2)
  if (frame.plot) box()
  if (!is.null(grid)) grid
  
  # draw the points/lines
  if (type=="p") invisible(
    lapply(
      seq_along(split_data), 
      function(i) points(
        x=split_data[[i]]$x, 
        y=split_data[[i]]$y, 
        col = cols[i], 
        type = type, 
        pch = pch
        )
      )
  )
  if (type %in% c("l", "o", "b")) invisible(
    lapply(
      seq_along(split_data), 
      function(i) lines(
        x=split_data[[i]]$x, 
        y=split_data[[i]]$y, 
        col = cols[i], 
        type = type,
        pch = pch
        )
      )
  )
  
  title(
    xlab = xlab,
    ylab = ylab,
    main = main,
    sub = sub
    )
  
  on.exit(par(opar))
  
}

plot2.formula = function(
    formula,
    data = parent.frame(),
    type = "p",
    xlim = NULL,
    ylim = NULL,
    # log = "",
    main = NULL, 
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    ann = par("ann"),
    axes = TRUE,
    frame.plot = axes,
    asp = NA,
    grid = NULL,
    palette = NULL,
    # palette.args = NULL,
    legend.position = "auto",
    pch = NULL,
    ...
    ) {

  # x, y, and by vars
  x = paste(formula[3])
  if (grepl(" | ", x)) {
    by = gsub(".* | ", "", x)
    x = gsub(" | .*", "", x)
  } else {
    by = NULL
  }
  y = paste(formula[2])
  
  if (is.null(xlab)) xlab = x
  if (is.null(ylab)) ylab = y
  
  x = data[[x]]
  y = data[[y]]
  if (!is.null(by)) by = data[[by]]
  
  if (is.null(xlim)) xlim = range(x)
  if (is.null(ylim)) ylim = range(y)
  
  
  # if (!is.null(by)) {
  #   split_data = split(data, data[[by]])
  # } else {
  #   split_data = list(data)
  # }
  
  plot2.default(
    x = x, y = y, by = by, 
    data = data,
    type = type,
    xlim = xlim,
    ylim = ylim,
    # log = "",
    main = main, 
    sub = sub,
    xlab = xlab,
    ylab = ylab,
    ann = ann,
    axes = axes,
    frame.plot = frame.plot,
    asp = asp,
    grid = grid,
    palette = palette,
    # palette.args = NULL,
    legend.position = legend.position,
    pch = pch,
    ...
    )

}
