#' Barplot type
#'
#' @description Type function for producing barplots. For formulas of type
#'   `~ x` (without left-hand side) the barplot visualizes the counts (absolute
#'   frequencies) of the levels of `x`. For formulas of type `y ~ x` the value
#'   of `y` within each level of `x` is visualized, if necessary aggregated
#'   using some function (default: mean).
#'
#' @param width numeric, optional vector of bar widths. (The distance between
#'   the midpoints of the bars is always 1.)
#' @param beside logical. In case of a `by` grouping variable, should bars be
#'   juxtaposed? Default is to use stacked bars instead.
#' @param FUN a function to compute the summary statistic for `y` within each
#'   group of `x` in case of using a two-sided formula `y ~ x` (default: mean).
#' @param xlevels a character or numeric vector specifying in which order the
#'   levels of the `x` variable should be plotted.
#' @param drop.zeros logical. Should bars with zero height be dropped? If set
#'   to `FALSE` (default) a zero height bar is still drawn for which the border
#'   lines will still be visible.
#'
#' @examples
#' # Basic examples of frequency tables (without y variable)
#' tinyplot(~ cyl, data = mtcars, type = "barplot")
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot")
#' tinyplot(~ cyl | vs, data = mtcars, type = "barplot", beside = TRUE)
#' 
#' tinytheme("clean2")
#' 
#' # Example for numeric y aggregated by x (default: FUN = mean) + facets
#' tinyplot(extra ~ ID | group, facet = "by", data = sleep,
#'   type = "barplot", beside = TRUE)
#'   
#' # Note: We used automatic argument passing for `beside = TRUE` above. But
#' # this wouldn't work for `width`, since it would conflict with the top-level
#' # `tinyplot(..., width = <width>)` argument. Safer to pass through the
#' # `type_barplot()` functional equivalent.
#' tinyplot(extra ~ ID | group, facet = "by", data = sleep,
#'   type = type_barplot(beside = TRUE, width = .5))
#' 
#' # Fancy frequency table:
#' tinyplot(Freq ~ Sex | Survived, facet = ~ Class, data = as.data.frame(Titanic),
#'   type = "barplot", facet.args = list(nrow = 1), flip = TRUE)
#'
#' tinytheme()
#' 
#' @export
type_barplot = function(width = 5/6, beside = FALSE, FUN = NULL, xlevels = NULL, drop.zeros = FALSE) {
  out = list(
    data = data_barplot(width = width, beside = beside, FUN = FUN, xlevels = xlevels),
    draw = draw_barplot(width = width, drop.zeros = drop.zeros),
    name = "barplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom stats aggregate
data_barplot = function(width = 5/6, beside = FALSE, FUN = NULL, xlevels = NULL) {
    fun = function(datapoints, col, bg, lty, lwd, palette, ribbon.alpha, xlab = NULL, ylab = NULL, xlim = NULL, ylim = ylim, yaxt, axes = TRUE, ...) {

        ## tabulate/aggregate datapoints
        if (is.null(datapoints$y)) {
          xlab = ylab
          ylab = "Count"
          
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

        lty = by_lty(ngrps = ngrps, type = "barplot", lty = lty) #FIXME# emulate by aesthetics because otherwise not available
        lwd = by_lwd(ngrps = ngrps, type = "barplot", lwd = lwd)
        col = by_col(                                            #FIXME#
          ngrps = ngrps, col = col, palette = palette,                      #FIXME#
          gradient = FALSE, ordered = is.ordered(datapoints$by),            #FIXME#
          alpha = NULL)                                                     #FIXME#
        bg = by_bg(                                              #FIXME#
          adjustcolor = adjustcolor, alpha = NULL, bg = bg, by = by,        #FIXME#
          by_continuous = FALSE, by_ordered = is.ordered(datapoints$by),    #FIXME#
          col = col, fill = NULL, palette = substitute(palette),            #FIXME#
          ribbon.alpha = ribbon.alpha, ngrps = ngrps, type = "boxplot")     #FIXME#
        
        out = list(
          datapoints = datapoints,
          xlab = xlab,
          ylab = ylab,
          xlim = xlim,
          ylim = ylim,
          axes = FALSE,
          frame.plot = FALSE,
          xaxs = "r",
          yaxs = "i",
          type_info = list(
            beside = beside,
            col = col,
            bg = bg,
            lty = lty,
            lwd = lwd,
            axes = axes,
            yaxt = yaxt
          )
        )
        return(out)
    }
    return(fun)
}

#' @importFrom graphics rect
draw_barplot = function(width = 5/6, drop.zeros = FALSE) {
    fun = function(data_facet, iby, ifacet, ilwd, flip, facet_by, type_info, ...) {
      if (iby == 1L) {
        df = lapply(data_facet, as.data.frame) ## recombine all data in the current facet
        df = do.call("rbind", df)

        if (facet_by) df = df[as.numeric(df$by) == ifacet, , drop = FALSE]

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
        
        by = df$by
        if (drop.zeros) {
          yb = rep_len(yb, length(yt))
          yok = abs(yt - yb) > 0
          xl = xl[yok]
          xr = xr[yok]
          yb = yb[yok]
          yt = yt[yok]
          by = by[yok]
        }
        
        rect(
          xleft   = if (flip) yb else xl,
          ybottom = if (flip) xl else yb,
          xright  = if (flip) yt else xr,
          ytop    = if (flip) xr else yt,
          border  = type_info$col[by],
          col     = type_info$bg[by],
          lty     = type_info$lty[by],
          lwd     = type_info$lwd[by])
        if (type_info[["axes"]]) {
          tinyAxis(1:nx, side = if (flip) 2 else 1, at = 1L:nx, labels = levels(df$x), type = "labels")
          tinyAxis(df$y, side = if (flip) 1 else 2, type = type_info[["yaxt"]])
        }
      }
    }
    return(fun)
}
