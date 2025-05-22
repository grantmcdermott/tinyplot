#' Spineplot and spinogram types
#'
#' @description Type function(s) for producing spineplots and spinograms, which
#'   are modified versions of histograms or mosaic plots, and particularly
#'   useful for visualizing factor variables. Note that [`tinyplot`] defaults
#'   to `type_spineplot()` if `y` is a factor variable.
#' @inheritParams graphics::spineplot
#' @examples
#' # "spineplot" type convenience string
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "spineplot")
#' 
#' # Aside: specifying the type is redundant for this example, since tinyplot()
#' # defaults to "spineplot" if y is a factor (just like base plot).
#' tinyplot(Species ~ Sepal.Width, data = iris)
#' 
#' # Use `type_spineplot()` to pass extra arguments for customization
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_spineplot(breaks = 4))
#' 
#' p = palette.colors(3, "Pastel 1")
#' tinyplot(Species ~ Sepal.Width, data = iris, type = type_spineplot(breaks = 4, col = p))
#' rm(p)
#' 
#' # More idiomatic tinyplot way of drawing the previous plot: use y == by
#' tinyplot(
#'   Species ~ Sepal.Width | Species, data = iris, type = type_spineplot(breaks = 4),
#'   palette = "Pastel 1", legend = FALSE
#' )
#' 
#' # Grouped and faceted spineplots
#' 
#' ttnc = as.data.frame(Titanic)
#' 
#' tinyplot(
#'   Survived ~ Sex, facet = ~ Class, data = ttnc,
#'   type = type_spineplot(weights = ttnc$Freq)
#' )
#' 
#' # For grouped "by" spineplots, it's better visually to facet as well
#' tinyplot(
#'   Survived ~ Sex | Class, facet = "by", data = ttnc,
#'   type = type_spineplot(weights = ttnc$Freq)
#' )
#' 
#' # Fancier version. Note the smart inheritance of spacing etc.
#' tinyplot(
#'   Survived ~ Sex | Class, facet = "by", data = ttnc,
#'   type = type_spineplot(weights = ttnc$Freq),
#'   palette = "Dark 2", facet.args = list(nrow = 1), axes = "t"
#' )
#' 
#' # Note: It's possible to use "by" on its own (without faceting), but the
#' # overlaid result isn't great. We will likely overhaul this behaviour in a
#' # future version of tinyplot...
#' tinyplot(Survived ~ Sex | Class, data = ttnc,
#'   type = type_spineplot(weights = ttnc$Freq), alpha = 0.3
#' )
#' 
#' @export
type_spineplot = function(breaks = NULL, tol.ylab = 0.05, off = NULL, ylevels = NULL, col = NULL, xaxlabels = NULL, yaxlabels = NULL, weights = NULL) {
  col = col
  out = list(
    data = data_spineplot(off = off, breaks = breaks, ylevels = ylevels, xaxlabels = xaxlabels, yaxlabels = yaxlabels, weights = weights),
    draw = draw_spineplot(tol.ylab = tol.ylab, off = off, col = col, xaxlabels = xaxlabels, yaxlabels = yaxlabels),
    name = "spineplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom grDevices nclass.Sturges
data_spineplot = function(off = NULL, breaks = NULL, ylevels = ylevels, xaxlabels = NULL, yaxlabels = NULL, weights = NULL) {
    fun = function(
      datapoints,
      by = NULL, col = NULL, bg = NULL, palette = NULL,
      facet = NULL, facet.args = NULL,
      xlim = NULL, ylim = NULL,
      axes = TRUE, xaxt = NULL, yaxt = NULL, xaxb = NULL, yaxb = NULL,
      null_by, null_facet, 
      ...
    ) {
      
        ## process weights
        if (!is.null(weights)) {
            ny = length(datapoints$y)
            if (length(weights) != ny && length(weights) != 1L) {
                stop(sprintf("'weights' must have either length 1 or %s", ny))
            }
        }
        datapoints$weights = weights
        
        ## process x variable
        if (is.factor(datapoints$x)) {
            breaks = NULL
            off = if(is.null(off)) 0.02 else off/100
            if (is.null(xlim)) xlim = c(0, 1 + (nlevels(datapoints$x) - 1L) * off)
        } else {
            off = 0
            if (is.null(xlim)) xlim = c(0, 1)
    	      x = as.numeric(datapoints$x)
            if (is.null(breaks)) {
              breaks = if (!is.null(xaxb)) xaxb else if (is.null(weights)) nclass.Sturges(x) else ceiling(log2(sum(weights)) + 1)
	    }
            breaks = as.numeric(breaks)
            if (length(breaks) == 1L) {
                if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L) stop("invalid number of 'breaks'")
                if (breaks > 1e6) {
                    warning(gettextf("'breaks = %g' is too large and set to 1e6", breaks))
                    breaks = 1000000L
                }
                rg = if (is.null(weights)) range(x, na.rm = TRUE) else range(x[weights > 0], na.rm = TRUE)
                breaks = pretty(rg, n = breaks, min.n = 1L)        
            }
        }

        ## process y variable
        if (!is.factor(datapoints$y)) datapoints$y = factor(datapoints$y)
        # if (!is.null(ylevels)) datapoints$y = factor(datapoints$y, levels = if(is.numeric(ylevels)) levels(datapoints$y)[ylevels] else ylevels)
        if (is.null(ylim)) ylim = c(0, 1)

        ## adjust facet margins
        if (!is.null(facet) && is.null(facet.args[["fmar"]])) {
          facet.args[["fmar"]] = c(2, 2, 2, 2)
        }
        
        x_by = identical(datapoints$x, datapoints$by)
        y_by = identical(datapoints$y, datapoints$by)
        
        if (!is.null(ylevels)) {
          datapoints$y = factor(datapoints$y, levels = if(is.numeric(ylevels)) levels(datapoints$y)[ylevels] else ylevels)
          if (y_by) datapoints$by = datapoints$y
        }
        
        x.categorical = is.factor(datapoints$x)
        x = datapoints$x
        y = datapoints$y
        
        # if either x_by or y_by are TRUE, we'll only split by facets and then
        # use some simple logic to assign colouring on the backend
        if (isTRUE(x_by) || isTRUE(y_by)) {
          datapoints = split(datapoints, list(datapoints$facet))
          datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        } else {
          datapoints = split(datapoints, list(datapoints$by, datapoints$facet))
          datapoints = Filter(function(k) nrow(k) > 0, datapoints)
        }
        
        # construct spineplot rectangles and breaks points for each by-facet combo
        datapoints = Map(function(dat, x.categorical, off) {
          ## set up frequency table
          x = dat$x
          if(!x.categorical) {
            x = cut(as.numeric(x), breaks = breaks, include.lowest = TRUE)
          }
          ## TODO: process by grouping via: interaction + spacing + labeling
          ## (for now just do interaction)
          ## FIXME: data_facet only contains the first by group?
          ## if (any(dat$by != "")) x = interaction(dat$by, x)
          if(is.null(dat$weights)) {
            tab = table(x, dat$y)
          } else {
            tab = as.table(tapply(dat$weights, list(x, dat$y), FUN = sum, na.rm = TRUE))
            tab[is.na(tab)] = 0
          }
          nx = nrow(tab)
          ny = ncol(tab)
          
          ## compute coordinates
          ## cumulative proportions of x (plus off) vs. conditional cumulative proportions of y
          yat = rbind(0, apply(proportions(tab[, ny:1L, drop = FALSE], 1L), 1L, cumsum))
          yat[is.na(yat)] = 1
          xat = c(0, cumsum(proportions(marginSums(tab, 1L)) + off))
          
          ybottom = as.vector(yat[-(ny + 1L),])
          ytop = as.vector(yat[-1L,])
          xleft = rep(xat[1L:nx], rep(ny, nx))
          xright = rep(xat[2L:(nx+1L)] - off, rep(ny, nx))
          
          out = data.frame(
            by = dat$by[1], # already split
            facet = dat$facet[1], # already split
            ymin = ybottom,
            ymax = ytop,
            xmin = xleft,
            xmax = xright
          )
          
          attr(out, "nx") = nx
          attr(out, "ny") = ny
          attr(out, "xat") = xat
          attr(out, "yat") = yat
          return(out)
        }, 
        dat = datapoints,
        x.categorical = x.categorical,
        off = off
        )
        
        nx = attr(datapoints[[1]], "nx") ## should be the same for all by/facet groups
        ny = attr(datapoints[[1]], "ny") ## ditto
        xat = lapply(datapoints, attr, "xat")
        yat = lapply(datapoints, attr, "yat")
        datapoints = do.call(rbind, datapoints)
         
        if (is.null(yaxlabels)) yaxlabels = rev(levels(y))
      
        ## axis labels
        yaxlabels = if(is.null(yaxlabels)) levels(y) else rep_len(yaxlabels, ny)
        if (!is.null(yaxb)) {
          # yaxlabels = yaxlabels[yaxlabels %in% yaxb]
          ## rather use the "" assignment workaround below, since otherwise we 
          ## get a mismatch between the label names and ticks 
          yaxlabels[!(yaxlabels %in% yaxb)] = ""
        }
        if(x.categorical) {
          xaxlabels = if(is.null(xaxlabels)) {
            levels(x)
          } else {
            rep_len(xaxlabels, nx)
          }
        } else {
          xaxlabels = if(is.null(xaxlabels)) {
            if(is.numeric(x)) breaks else c(x[1L], x[c(diff(as.numeric(x)) > 0, TRUE)])
          } else {
            rep_len(xaxlabels, nx + 1L)
          }
        }
        
        # catch for x_by / y/by
        if (isTRUE(x_by)) datapoints$by = factor(rep(xaxlabels, each = ny)) # each x label extends over ny rows
        if (isTRUE(y_by)) datapoints$by = factor(rep(yaxlabels, length.out = nrow(datapoints)))
          
        ## grayscale flag
        grayscale = null_by && is.null(palette) && is.null(.tpar[["palette.qualitative"]])
        
        out = list(
          x = c(datapoints$xmin, datapoints$xmax), 
          y = c(datapoints$ymin, datapoints$ymax),
          ymin = datapoints$ymin, 
          ymax = datapoints$ymax, 
          xmin = datapoints$xmin, 
          xmax = datapoints$xmax, 
          col = col,
          bg = bg,
          datapoints = datapoints,
          by = if (null_by) by else datapoints$by, 
          facet = if (null_facet) facet else datapoints$facet,
          axes = FALSE,
          frame.plot = FALSE,
          xaxt = "n",
          yaxt = "n",
          xaxs = "i",
          yaxs = "i",
          ylabs = yaxlabels,
          type_info = list(
            off = off,
            x.categorical = x.categorical,
            nx = nx,
            ny = ny,
            xat = xat,
            yat = yat,
            xaxlabels = xaxlabels,
            yaxlabels = yaxlabels,
            breaks = breaks,
            axes = axes,
            xaxt = xaxt, 
            yaxt = yaxt,
            grayscale = grayscale,
            x_by = x_by,
            y_by = y_by
          ),
          facet.args = facet.args
        )
        
        return(out)
        
    }
    return(fun)
}

#' @importFrom grDevices gray.colors
draw_spineplot = function(tol.ylab = 0.05, off = NULL, col = NULL, xaxlabels = NULL, yaxlabels = NULL) {
    fun = function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ibg, 
                   flip,
                   facet_window_args,
                   type_info,
                   ifacet,
                   ...) {
      
      if (is.null(off)) off = type_info[["off"]]
      if (is.null(xaxlabels)) xaxlabels = type_info[["xaxlabels"]]
      if (is.null(yaxlabels)) yaxlabels = type_info[["yaxlabels"]]
      xat = type_info[["xat"]][[ifacet]]
      yat = type_info[["yat"]][[ifacet]]
      nx = type_info[["nx"]]
      ny = type_info[["ny"]]
      x.categorical = type_info[["x.categorical"]]
      grayscale = type_info[["grayscale"]]
      x_by = type_info[["x_by"]]
      y_by = type_info[["y_by"]]
      
      ## graphical parameters
      if (is.null(col)) {
        if (is.null(ibg)) ibg = icol
        if (isFALSE(y_by)) {
          ibg = if (isTRUE(grayscale)) gray.colors(ny) else seq_palette(ibg, ny)
        }
        ibg = rep_len(ibg, ny)
      } else {
        ibg = col
      }
      
      if (type_info[["xaxt"]] %in% c("l", "t", "n") &&
          type_info[["yaxt"]] %in% c("l", "t", "n") &&
          !all(c(type_info[["xaxt"]], type_info[["yaxt"]]) == "n")) ilwd = 0
      
      rect(
          xleft = ixmin, ybottom = iymin, xright = ixmax, ytop = iymax,
          lty = ilty,
          lwd = ilwd,
          border = par("fg"), #icol,
          col = ibg
      )
      
      ## axes
      ## - standard categorical axes (xaxt/yaxt == "s") _without_ ticks
      ## - never draw additional axis lines, box always for spinogram
      if(type_info[["axes"]]) {
          if (x.categorical) {
              spine_axis(if (flip) 2 else 1, at = (xat[1L:nx] + xat[2L:(nx+1L)] - off)/2, labels = xaxlabels,
                  type = type_info[["xaxt"]], categorical = TRUE)
          } else {
              spine_axis(if (flip) 2 else 1, at = xat, labels = xaxlabels,
                  type = type_info[["xaxt"]], categorical = FALSE)
          }
          yat = yat[, if(flip) ncol(yat) else 1L]
          equidist = any(diff(yat) < tol.ylab)
          yat = if(equidist) seq.int(1/(2*ny), 1-1/(2*ny), by = 1/ny) else (yat[-1L] + yat[-length(yat)])/2
          spine_axis(if (flip) 3 else 2, at = yat, labels = yaxlabels,
              type = type_info[["yaxt"]], categorical = TRUE)
          if (is_facet_position(if(flip) "bottom" else "right", ifacet, facet_window_args)) spine_axis(if (flip) 1 else 4,
              type = type_info[["yaxt"]], categorical = FALSE)
      }
      if(!x.categorical && (is.null(ilwd) || ilwd > 0)) box()
      
    }
    return(fun)
}




spine_axis = function(side, ..., type = "standard", categorical = TRUE) {
    type = match.arg(type, c("standard", "none", "labels", "ticks", "axis"))
    ## standard: with axis, ticks (unless categorical), and labels
    ## none: no axes
    ## labels: only labels without ticks and axis line
    ## ticks: only ticks and labels without axis line
    ## axis: only axis line and labels but no ticks

    if (type == "none") {
        invisible(numeric(0L))
    } else {
        args = list(side = side, ...)
        if (type == "labels") {
            args$tick = FALSE
        } else if (type == "ticks") {
            args$lwd = 0
            if (!("lwd.ticks" %in% names(args))) args$lwd.ticks = if (categorical) 0 else 1
        } else if (type == "axis") {
            if (categorical) {
                args$tick = FALSE
            } else {
                args$lwd.ticks = 0
            }
        } else {
            args$tick = !categorical
        }
        do.call("axis", args)
    }
}

#' @importFrom grDevices col2rgb convertColor hcl
to_hcl = function(x) {
    x = t(col2rgb(x, alpha = TRUE)/255)
    alpha = x[, 4]
    x = x[, 1:3]
    x = convertColor(x, from = "sRGB", to = "Luv")
    x = cbind(H = atan2(x[, 3L], x[, 2L]) * 180/pi, C = sqrt(x[, 2L]^2 + x[, 3L]^2), L = x[, 1L])
    x[is.na(x[, 1L]), 1L] = 0
    x[x[, 1L] < 0, 1L] = x[x[, 1L] < 0, 1L] + 360
    attr(x, "alpha") = alpha
    return(x)
}

seq_palette = function(x, n, power = 1.5) {
    x = drop(to_hcl(x[1L]))
    alpha = attr(x, "alpha")
    hcl(
      h = x[1L],
      c = seq.int(from = x[2L]^(1/power), to = 0, length.out = n + 1)[1L:n]^power,
      l = 100 - seq.int(from = (100 - x[3L])^(1/power), to = pmin(8, (100 - x[3L])/2)^(1/power), length.out = n)^power,
      alpha = alpha
    )[1L:n]
}
