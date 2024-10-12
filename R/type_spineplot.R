#' Spineplot and spinogram type
#'
#' @inheritParams graphics::spineplot
#' @export
type_spineplot = function(tol.ylab = 0.05, off = NULL, ylevels = NULL, col = NULL, xaxlabels = NULL, yaxlabels = NULL) {
  out = list(
    draw = draw_spineplot(tol.ylab = tol.ylab, off = off, col = col, xaxlabels = xaxlabels, yaxlabels = yaxlabels),
    data = data_spineplot(off = off, ylevels = ylevels),
    name = "spineplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom grDevices gray.colors
draw_spineplot = function(tol.ylab = 0.05, off = NULL, col = NULL, xaxlabels = NULL, yaxlabels = NULL) {
    fun = function(data_facet, ifacet, icol, ilty, ilwd, flip, facet_window_args, type_info, ...) {

        ## get data subset for current facet
        dat = data.frame(data_facet[[ifacet]])

        ## handle flip internally in order to actually flip splitting direction
        if (flip) names(dat)[c(which(names(dat) == "x"), which(names(dat) == "y"))] = c("y", "x")

        ## set up frequency table
        x = dat$x
        x.categorical = is.factor(x)
        breaks = type_info[["breaks"]]
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

        ## graphical parameters
        if (is.null(col)) col = if (icol == "#000000FF") gray.colors(ny) else seq_palette(icol, ny)
        col = rep_len(col, ny)
        off = if(!x.categorical) 0 else if(is.null(off)) 0.02 else off/100
        if (is.null(yaxlabels)) yaxlabels = rev(levels(dat$y))

        ## axis labels
        yaxlabels = if(is.null(yaxlabels)) levels(y) else rep_len(yaxlabels, ny)
        if(x.categorical) {
            xaxlabels = if(is.null(xaxlabels)) {
                levels(x)
            } else {
                rep_len(xaxlabels, nx)
            }
        } else {
            xaxlabels = if(is.null(xaxlabels)) {
                if(is.numeric(dat$x)) breaks else c(dat$x[1L], dat$x[c(diff(as.numeric(x)) > 0, TRUE)])
            } else {
                rep_len(xaxlabels, nx + 1L)
            }
        }

        ## compute coordinates
        ## cumulative proportions of x (plus off) vs. conditional cumulative proportions of y
        yat = rbind(0, apply(proportions(tab[, ny:1L, drop = FALSE], 1L), 1L, cumsum))
        yat[is.na(yat)] = 1
        xat = c(0, cumsum(proportions(marginSums(tab, 1L)) + off))

        ybottom = as.vector(yat[-(ny + 1L),])
        ytop = as.vector(yat[-1L,])
        xleft = rep(xat[1L:nx], rep(ny, nx))
        xright = rep(xat[2L:(nx+1L)] - off, rep(ny, nx))

        ## plot rectangles
        col = rep.int(col, nx)
        if (type_info[["xaxt"]] %in% c("l", "t", "n") &&
            type_info[["yaxt"]] %in% c("l", "t", "n") &&
            !all(c(type_info[["xaxt"]], type_info[["yaxt"]]) == "n")) ilwd = 0
        if (flip) {
            rect(ybottom, xleft, ytop, xright, col = col, lty = ilty, lwd = ilwd)
        } else {
            rect(xleft, ybottom, xright, ytop, col = col, lty = ilty, lwd = ilwd)
        }
        
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

#' @importFrom grDevices nclass.Sturges
data_spineplot = function(off = NULL, ylevels = ylevels) {
    fun = function(datapoints, breaks = NULL, weights = NULL,
      facet = NULL, facet.args = NULL, xlim = NULL, ylim = NULL, axes = TRUE, xaxt = NULL, yaxt = NULL, ...) {

        ## process weights
        if (!is.null(weights)) {
            ny = length(datapoints$y)
            if (length(weights) != ny && length(weights) != 1L) {
                stop(sprintf("'weights' must have either length 1 or %s", ny))
            }
        }
        datapoints$weights = weights

        ## process x variable
        if(is.factor(datapoints$x)) {
            breaks = NULL
            off = if(is.null(off)) 0.02 else off/100
            if (is.null(xlim)) xlim = c(0, 1 + (nlevels(datapoints$x) - 1L) * off)
        } else {
            off = 0
            if (is.null(xlim)) xlim = c(0, 1)
    	    x = as.numeric(datapoints$x)
            if (is.null(breaks)) {
                breaks = if(is.null(weights)) nclass.Sturges(x) else ceiling(log2(sum(weights)) + 1)
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
	if (!is.null(ylevels)) datapoints$y = factor(y, levels = if(is.numeric(ylevels)) levels(y)[ylevels] else ylevels)
        if (is.null(ylim)) ylim = c(0, 1)

        ## adjust facet margins
        if (!is.null(facet) && is.null(facet.args[["fmar"]])) {
          facet.args[["fmar"]] = c(2, 2, 2, 2)
        }

        out = list(
            datapoints = datapoints,
            facet.args = facet.args,
            xlim = xlim,
            ylim = ylim,
            axes = FALSE,
            frame.plot = FALSE,
            xaxt = "n",
            yaxt = "n",
            xaxs = "i",
            yaxs = "i",
            type_info = list(breaks = breaks, axes = axes, xaxt = xaxt, yaxt = yaxt)
        )
        return(out)
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

to_hcl = function(x) {
    x = t(col2rgb(x)/255)
    x = convertColor(x, from = "sRGB", to = "Luv")
    x = cbind(H = atan2(x[, 3L], x[, 2L]) * 180/pi, C = sqrt(x[, 2L]^2 + x[, 3L]^2), L = x[, 1L])
    x[is.na(x[, 1L]), 1L] = 0
    x[x[, 1L] < 0, 1L] = x[x[, 1L] < 0, 1L] + 360
    return(x)
}

seq_palette = function(x, n, power = 1.5) {
    x = drop(to_hcl(x[1L]))
    hcl(h = x[1L],
        c = seq.int(from = x[2L]^(1/power), to = 0, length.out = n + 1)[1L:n]^power,
        l = 100 - seq.int(from = (100 - x[3L])^(1/power), to = pmin(8, (100 - x[3L])/2)^(1/power), length.out = n)^power)[1L:n]
}

## aq = transform(
##   airquality,
##   Month = factor(Month, labels = month.abb[unique(Month)]),
##   Hot = Temp > median(Temp),
##   Wind2 = cut(Wind, c(0, 5, 10, 15, 21))
## )
## tinyplot(Hot ~ Wind2, facet = ~ Month, data = aq, type = type_spineplot())
## 
## ttnc = as.data.frame(Titanic)
## tinyplot(Survived ~ Sex, facet = ~ Class, data = ttnc, weights = ttnc$Freq, type = type_spineplot())
