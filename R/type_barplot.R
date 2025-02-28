#' Barplot type
#' 
#' @export
type_barplot = function(width = 5/6, beside = FALSE, FUN = NULL, xlevels = NULL, col = NULL) {
  col = col
  out = list(
    data = data_barplot(width = width, beside = beside, FUN = FUN, xlevels = xlevels),
    draw = draw_barplot(width = width),
    name = "ridge"                      #FIXME# use "ridge" instead of "barplot" to draw area legend (reverse order?)
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom stats aggregate
data_barplot = function(width = 5/6, beside = FALSE, FUN = NULL, xlevels = NULL) {
    fun = function(datapoints, col, bg, lty, palette, ribbon.alpha, xlab = NULL, ylab = NULL, xlim = NULL, ylim = ylim, yaxt, axes = TRUE, ...) {

        ## tabulate/aggregate datapoints
        if (!is.factor(datapoints$x)) { #FIXME# should be "if (is.null(datapoints$y))" without need to re-switch x and y
          datapoints$x = datapoints$y   #FIXME#
          xlab = ylab                   #FIXME#
          ylab = ""                     #FIXME#
          
          datapoints$y = numeric(nrow(datapoints))          
          if (!is.null(FUN)) warning("without 'y' variable 'FUN' specification is ignored")
          FUN = length
        } else {
          if (is.null(FUN)) FUN = function(x, ...) mean(x, ..., na.rm = TRUE)
        }
        if (!is.factor(datapoints$x)) datapoints$x = factor(datapoints$x)
        if (!is.null(xlevels)) datapoints$x = factor(datapoints$x, levels = if(is.numeric(xlevels)) levels(x)[xlevels] else xlevels)
        datapoints = aggregate(datapoints[, "y", drop = FALSE], datapoints[, c("x", "by", "facet")], FUN = FUN, drop = FALSE)
        datapoints$y[is.na(datapoints$y)] = 0 #FIXME: always?#
        if (!is.factor(datapoints$by)) datapoints$by = factor(datapoints$by)
        if (!is.factor(datapoints$facet)) datapoints$facet = factor(datapoints$facet)
        
        if (!beside && any(datapoints$y < 0)) {
          warning("'beside' must be TRUE if there are negative 'y' values")
          beside = TRUE
        }
        if (is.null(xlim)) xlim = c(1, length(levels(datapoints$x))) + c(-0.5, 0.5) * width
        if (is.null(ylim)) ylim = if (beside || length(unique(datapoints$by)) == 1L) {
          c(pmin(0, min(datapoints$y, na.rm = TRUE) * 1.02), pmax(0, max(datapoints$y, na.rm = TRUE) * 1.02))
        } else {
          c(0, max(tapply(datapoints$y, interaction(datapoints$x, datapoints$facet), sum, na.rm = TRUE)) * 1.02)
        }

        ## default color palette
        ngrps = length(unique(datapoints$by))
        if (ngrps == 1L && is.null(palette)) {
          if (is.null(col)) col = par("fg")
          if (is.null(bg)) bg = "black"
        } else {
          bg = "by"
        }

        lty = tinyplot:::by_lty(ngrps = ngrps, type = "barplot", lty = lty) #FIXME# emulate by aesthetics because otherwise not available
        col = tinyplot:::by_col(                                            #FIXME#
          ngrps = ngrps, col = col, palette = palette,                      #FIXME#
          gradient = FALSE, ordered = is.ordered(datapoints$by),            #FIXME#
          alpha = NULL)                                                     #FIXME#
        bg = tinyplot:::by_bg(                                              #FIXME#
          adjustcolor = adjustcolor, alpha = NULL, bg = bg, by = by,        #FIXME#
          by_continuous = FALSE, by_ordered = is.ordered(datapoints$by),    #FIXME#
          col = col, fill = NULL, palette = substitute(palette),            #FIXME#
          ribbon.alpha = ribbon.alpha, ngrps = ngrps, type = "boxplot")     #FIXME#
                                                                            #FIXME# lwd not available in type_data!
        
        out = list(
          datapoints = datapoints,
          xlab = xlab,
          ylab = ylab,
          xlim = xlim,
          ylim = ylim,
          axes = FALSE,
          frame.plot = FALSE,
          xaxs = "r",         #FIXME# include in if (isTRUE(flip)) logic in tinyplot
          yaxs = "i",         #FIXME#
          type_info = list(
            beside = beside,
            col = col,
            bg = bg,
            lty = lty,
            axes = axes,
            yaxt = yaxt
          )
        )
        return(out)
    }
    return(fun)
}

#' @importFrom grDevices rect
draw_barplot = function(width = 5/6) {
    fun = function(data_by, iby, ifacet, ilwd, flip, facet_by, type_info, ...) {
      if (iby == 1L) {
        df = lapply(data_by, as.data.frame)                     #FIXME# would be nice if tinyplot did not loop over by
        df = do.call("rbind", df)                               #FIXME#
        if (is.null(df$facet)) {                                #FIXME#
          df$facet = ""                                         #FIXME#
          df$facet = factor(df$facet)                           #FIXME#
        }                                                       #FIXME#
        df = df[as.numeric(df$facet) == ifacet, , drop = FALSE] #FIXME#

        if (flip) {
          xy <- which(names(df) %in% c("x", "y"))
          names(df)[xy] <- names(df)[xy[2L:1L]]
        }
        df = df[order(df$x), , drop = FALSE]
        nx = length(levels(df$x))
        nb = length(levels(df$by))
        
        if (type_info$beside) {        
          xl = as.numeric(df$x) - width/2 + (as.numeric(df$by) - 1) * width/nb * as.numeric(!facet_by)
          xr = if (facet_by) xl + width else xl + width/nb
          yb = 0
          yt = df$y
        } else {
          cs = tapply(df$y, df$x, function(z) cumsum(c(0, z)))
          xl = as.numeric(df$x) - width/2
          xr = xl + width
          yb = if (facet_by) 0 else unlist(lapply(cs, `[`, -(nb + 1L)))
          yt = if (facet_by) df$y else unlist(lapply(cs, `[`, -1L))
        }
        rect(
          xleft   = if (flip) yb else xl,
          ybottom = if (flip) xl else yb,
          xright  = if (flip) yt else xr,
          ytop    = if (flip) xr else yt,
          border  = type_info$col[df$by],
          col     = type_info$bg[df$by],
          lty     = type_info$lty[df$by],
          lwd     = ilwd)                  #FIXME# include lwd in type_info
        if (type_info[["axes"]]) {
          tinyplot:::tinyAxis(1:nx, side = if (flip) 2 else 1, at = 1L:nx, labels = levels(df$x), type = "labels")
          tinyplot:::tinyAxis(df$y, side = if (flip) 1 else 2, type = type_info[["yaxt"]])
        }
      }
    }
    return(fun)
}
