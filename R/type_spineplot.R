#' Spineplot and spinogram types
#'
#' @description Type function(s) for producing spineplots and spinograms, which
#'   are modified versions of histograms or mosaic plots, and particularly
#'   useful for visualizing factor variables. Note that [`tinyplot`] defaults
#'   to `type_spineplot()` if `y` is a factor variable.
#' @param xlevels,xord two ways to control the order of the `x` variable, and
#'   hence of the axis. Supply one or the other; if both are given, `xlevels`
#'   takes precedence and `xord` is ignored. Both only affect categorical (i.e.,
#'   factor or character) variables.
#'
#'   `xlevels` names the levels literally: a character vector of level names in
#'   the desired order, or a numeric vector of the corresponding level indexes
#'   (e.g. `3:1`).
#'
#'   `xord` instead derives the order from the data, via a keyword or a
#'   function. Options are:
#'
#'   - `"total"` ranks the categories by frequency, most common first. (Both
#'   axes of a spineplot are categorical, so there is no response to rank on and
#'   observations are counted instead, weighted if `weights` is given.)
#'   - `"asis"` and `"rev"` permute the existing levels without consulting the
#'   data at all. The former takes the categories in the order that they appear
#'   in the data, while `"rev"` reverses the current level order.
#'   - a custom function that determines both the ranking statistic and its
#'   direction. The statistic is always sorted ascending, so `function(y) sum(y)`
#'   reverses `"total"`.
#'
#'   Both default to `NULL`, i.e. keep the existing factor levels.
#' @param ylevels,yord as for `xlevels` / `xord` above, but for the `y` variable.
#' @inheritParams graphics::spineplot
#' @param lighten logical. For grouped spineplots where the `y` variable is
#'   itself the grouping variable (i.e. `y == by`), should the fills use a
#'   lighter, opaque tint of the series colour(s)? Default is `FALSE`, i.e. the
#'   fills use the fully-saturated palette colour(s). (Unlike the other area
#'   types such as [`type_barplot`], where lightening is the default, spineplot
#'   tiles abut one another with no gap, so the darker saturated fills read
#'   better against their matching border colours.) Set to `TRUE` to opt in to
#'   the lighter tint. Note that `lighten` has no effect on other spineplot
#'   displays (single-group or `x == by`), which always use a sequential shading
#'   ramp of the base colour.
#' @examples
#' # "spineplot" type convenience string
#' tinyplot(Species ~ Sepal.Width, data = iris, type = "spineplot")
#' 
#' # Aside: specifying the type is redundant for this example, since tinyplot()
#' # defaults to "spineplot" if y is a factor (just like base plot).
#' tinyplot(Species ~ Sepal.Width, data = iris)
#' 
#' # Use `type_spineplot()` to pass extra arguments for customization
#' tinyplot(
#'   Species ~ Sepal.Width, data = iris,
#'   type = type_spineplot(breaks = 4)
#' )
#' 
#' # Passing custom colors to the y-axis categories
#' tinyplot(
#'   Species ~ Sepal.Width, data = iris,
#'   type = type_spineplot(breaks = 4, col = palette.colors(3, "Pastel 1"))
#' )
#' 
#' # More idiomatic tinyplot way of drawing the previous plot: use y == by
#' tinyplot(
#'   Species ~ Sepal.Width | Species, data = iris,
#'   type = type_spineplot(breaks = 4),
#'   palette = "Pastel 1", legend = FALSE
#' )
#' 
#' ## Grouped and faceted spineplots
#'
#' ttnc = as.data.frame(Titanic)
#' 
#' # Note: The Titanic (ttnc) dataset is pre-tabulated, so we pass its frequency
#' # counts via the top-level `weights` argument (accepted via non-standard
#' # evaluation in the formula method).
#' tinyplot(
#'   Survived ~ Sex, facet = ~ Class, data = ttnc,
#'   # type_spineplot(weights = ttnc$Freq), ## same thing but not NSE
#'   type = "spineplot", weights = Freq
#' )
#' 
#' # Reorder x and y variable categories either by their character levels or
#' # numeric indexes. (Here we combine a top-level `weights` with constructor-
#' # level arguments passed through `type_spineplot()`.)
#' tinyplot(
#'   Survived ~ Sex, facet = ~ Class, data = ttnc,
#'   type = type_spineplot(xlevels = c("Female", "Male"), ylevels = 2:1),
#'   weights = Freq
#' )
#'
#' # For (colour) grouped "by" spineplots, it's visually better to facet too
#' tinyplot(
#'   Survived ~ Sex | Class, data = ttnc,
#'   facet = "by",
#'   type = "spineplot", weights = Freq
#' )
#'
#' # Fancier version. Note the smart inheritance of spacing etc.
#' tinyplot(
#'   Survived ~ Sex | Class, data = ttnc,
#'   facet = "by", facet.args = list(nrow = 1),
#'   type = "spineplot", weights = Freq,
#'   theme = "void", axes = "t", lty = 0, legend = FALSE,
#'   main = "Who survived the Titanic disaster?",
#'   sub = "Frequencies by boarding class and sex"
#' )
#'
#' # Aside: It's possible to use "by" on its own (without faceting), but the
#' # overlaid result isn't great. We will likely overhaul this behaviour in a
#' # future version of tinyplot...
#' tinyplot(Survived ~ Sex | Class, data = ttnc,
#'   type = "spineplot", weights = Freq, alpha = 0.3
#' )
#' 
#' @export
type_spineplot = function(breaks = NULL, tol.ylab = 0.05, off = NULL, xlevels = NULL, xord = NULL, ylevels = NULL, yord = NULL, col = NULL, xaxlabels = NULL, yaxlabels = NULL, weights = NULL, lighten = FALSE) {
  col = col
  out = list(
    data = data_spineplot(off = off, breaks = breaks, xlevels = xlevels, xord = xord, ylevels = ylevels, yord = yord, xaxlabels = xaxlabels, yaxlabels = yaxlabels, weights = weights, lighten = lighten),
    draw = draw_spineplot(tol.ylab = tol.ylab, off = off, col = col, xaxlabels = xaxlabels, yaxlabels = yaxlabels, lighten = lighten),
    name = "spineplot"
  )
  class(out) = "tinyplot_type"
  return(out)
}

#' @importFrom grDevices nclass.Sturges
data_spineplot = function(off = NULL, breaks = NULL, xlevels = xlevels, xord = NULL, ylevels = ylevels, yord = NULL, xaxlabels = NULL, yaxlabels = NULL, weights = NULL, lighten = FALSE) {
    fun = function(settings, ...) {
        env2env(settings, environment(), c("datapoints", "xlim", "ylim", "facet", "facet.args", "by", "xaxb", "yaxb", "null_by", "null_facet", "col", "bg", "axes", "frame.plot", "xaxt", "yaxt", "lwd", "lty"))
        settings[["lighten"]] = lighten
      
        ## process weights: a top-level `weights` column (carried on datapoints
        ## via NSE) takes precedence over the constructor-level `weights` arg.
        ## Either way, unify into the `datapoints$weights` column and the local
        ## `weights` vector that the break/range logic below relies on.
        if (is.null(datapoints[["weights"]]) && !is.null(weights)) {
            datapoints$weights = weights
        }
        weights = datapoints[["weights"]]
        if (!is.null(weights)) settings$weights_used = TRUE
        
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
        if (is.null(ylim)) ylim = c(0, 1)

        ## adjust facet margins
        if (!is.null(facet) && is.null(facet.args[["fmar"]])) {
          facet.args[["fmar"]] = c(2, 2, 2, 2)
        }
        
        x_by = identical(datapoints$x, datapoints$by)
        y_by = identical(datapoints$y, datapoints$by)
        
        x.categorical = is.factor(datapoints$x)
        if (!is.null(xlevels) && x.categorical) {
          datapoints$x = sanitize_xlevels(datapoints$x, xlevels)
          if (x_by) datapoints$by = datapoints$x
        }
        if (!is.null(ylevels)) {
          datapoints$y = sanitize_xlevels(datapoints$y, ylevels, arg = "ylevels")
          if (y_by) datapoints$by = datapoints$y
        }
        ## Both axes here are categorical, so there is no response to rank on:
        ## the size keywords count observations instead (weighted, if given),
        ## i.e. "total" orders the categories by frequency.
        spine_w = if (!is.null(weights)) weights else rep.int(1, nrow(datapoints))
        if (!is.null(xord) && is.null(xlevels) && x.categorical) {
          datapoints$x = sanitize_ord(
            datapoints$x, spine_w, NULL,
            xord, arg = "xord", keywords = ord_keywords_scalar
          )
          if (x_by) datapoints$by = datapoints$x
        }
        if (!is.null(yord) && is.null(ylevels)) {
          datapoints$y = sanitize_ord(
            datapoints$y, spine_w, NULL,
            yord, arg = "yord", keywords = ord_keywords_scalar
          )
          if (y_by) datapoints$by = datapoints$y
        }
        
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
        if (isTRUE(y_by)) datapoints$by = factor(rep_len(yaxlabels, nrow(datapoints)))

        x = c(datapoints$xmin, datapoints$xmax)
        y = c(datapoints$ymin, datapoints$ymax)
        ymin = datapoints$ymin
        ymax = datapoints$ymax
        xmin = datapoints$xmin
        xmax = datapoints$xmax
        by = if (null_by) by else datapoints$by
        facet = if (null_facet) facet else datapoints$facet

        # Save original values for type_info before overwriting
        axes_orig = axes
        xaxt_orig = xaxt
        yaxt_orig = yaxt
        # `frame.plot` defaults to TRUE for numeric-x spinograms (the outer box
        # is drawn by draw_spineplot below, after the tiles); NULL/unset counts
        # as TRUE. Preserve the user's choice before overwriting it, so the box
        # honours the top-level `frame.plot` rather than the tile-border `lwd`.
        frameplot_orig = !isFALSE(frame.plot)

        axes = FALSE
        frame.plot = FALSE
        xaxt = "n"
        yaxt = "n"
        xaxs = "i"
        yaxs = "i"
        ylabs = yaxlabels
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
          axes = axes_orig,
          frame.plot = frameplot_orig,
          xaxt = xaxt_orig,
          yaxt = yaxt_orig,
          null_by = null_by,
          x_by = x_by,
          y_by = y_by
        )
        
        # legend customizations
        # Mirror type_barplot()
        settings$legend_args[["lty"]] = settings$legend_args[["lty"]] %||% 0
        settings$legend_args[["pch"]] = settings$legend_args[["pch"]] %||% 22
        settings$legend_args[["pt.cex"]] = settings$legend_args[["pt.cex"]] %||% 3.5
        settings$legend_args[["y.intersp"]] = settings$legend_args[["y.intersp"]] %||% 1.25
        settings$legend_args[["seg.len"]] = settings$legend_args[["seg.len"]] %||% 1.25

        # Declare this type's axes/legend behaviour so the main pipeline can read
        # semantic flags instead of hardcoding `type == "spineplot"` checks.
        # A spineplot suppresses the standard axes (xaxt/yaxt = "n") because it
        # draws its own (category + numeric labels, plus a secondary RHS axis)
        # via spine_axis(), and uses proportional [0, 1] limits.
        type_hints = list(
          draws_own_axes        = TRUE, # draws own tick-row axes despite xaxt/yaxt = "n"
          has_rhs_axis          = TRUE, # secondary right-hand axis (reserve margin)
          has_proportional_lim  = TRUE, # [0, 1] limits; don't expand to axis breaks
          legend_fills_from_col = TRUE, # legend swatch pt.bg defaults from col
          legend_border_fg      = TRUE, # ... and its border is always par("fg")
          # We force `frame.plot = FALSE` above so the pipeline doesn't draw a box
          # (draw_spineplot() draws its own). Surface the user's actual choice so
          # margin logic can still tell a framed plot from a frameless one.
          framed                = frameplot_orig
        )

        env2env(environment(), settings, c(
          "x", "y", "ymin", "ymax", "xmin", "xmax", "col", "bg", "datapoints",
          "by", "facet", "axes", "frame.plot", "xaxt", "yaxt", "xaxs", "yaxs",
          "ylabs", "type_info", "facet.args", "type_hints"
        ))
        
    }
    return(fun)
}

draw_spineplot = function(tol.ylab = 0.05, off = NULL, col = NULL, xaxlabels = NULL, yaxlabels = NULL, lighten = FALSE) {
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
      null_by = type_info[["null_by"]]
      x_by = type_info[["x_by"]]
      y_by = type_info[["y_by"]]
      
      ## graphical parameters
      if (is.null(col)) {
        if (isFALSE(y_by)) {
          # For single-group displays, use a neutral grey ramp (gray.colors)
          # whenever the resolved seed colour is achromatic (e.g. the black
          # default of the plain default or the "bw"/"ipsum" themes), so these
          # are consistent regardless of whether a palette is declared -- the
          # same principle as the single-group fill logic in by_bg(). For grouped
          # displays we never switch to grayscale: each group (including one
          # whose palette colour is black) follows the same seq_palette ramp so
          # the fills stay in sync with the legend swatches.
          if (is.null(ibg)) ibg = icol
          gs = isTRUE(null_by) && is_achromatic(ibg)
          ibg = seq_palette(ibg, ny, grayscale = gs)
        } else {
          # When the y variable is itself the grouping (`y_by`), each band is a
          # group's palette colour. The fill is resolved once in prepare_legend()
          # and arrives via `ibg` -- lightened to match the other area types
          # (barplot/boxplot/violin) unless `lighten` is off (issue #646). Only
          # fall back to lightening the group colour `icol` here if no fill was
          # supplied (e.g. a standalone draw outside the legend pipeline).
          if (is.null(ibg)) ibg = if (isTRUE(lighten)) lighten_fill(icol) else icol
        }
        ibg = rep_len(ibg, ny)
      } else {
        ibg = col
      }

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
          # Spineplot draws its own axes, so it must apply the same per-facet rule
          # the generic pipeline uses (see draw_facet_axis()): framed panels each
          # get an axis, frameless ones only on the outer edge. `frame.plot` comes
          # via type_info because data_spineplot() forces the settings copy FALSE.
          keep_axis = function(side) {
            draw_facet_axis(
              side, ifacet, facet_window_args,
              framed = facet_axes_framed(
                type_info[["frame.plot"]], type_info[["xaxt"]], type_info[["yaxt"]]
              ),
              free = isTRUE(facet_window_args[["facet.args"]][["free"]]),
              axes = facet_window_args[["facet.args"]][["axes"]]
            )
          }
          xside = if (flip) 2 else 1
          yside = if (flip) 3 else 2
          rside = if (flip) 1 else 4
          if (keep_axis(xside)) {
            if (x.categorical) {
                spine_axis(xside, at = (xat[1L:nx] + xat[2L:(nx+1L)] - off)/2, labels = xaxlabels,
                    type = type_info[["xaxt"]], categorical = TRUE)
            } else {
                spine_axis(xside, at = xat, labels = xaxlabels,
                    type = type_info[["xaxt"]], categorical = FALSE)
            }
          }
          yat = yat[, if(flip) ncol(yat) else 1L]
          equidist = any(diff(yat) < tol.ylab)
          yat = if(equidist) seq.int(1/(2*ny), 1-1/(2*ny), by = 1/ny) else (yat[-1L] + yat[-length(yat)])/2
          if (keep_axis(yside)) {
            spine_axis(yside, at = yat, labels = yaxlabels,
                type = type_info[["yaxt"]], categorical = TRUE)
          }
          # The secondary numeric axis only ever belongs on the far edge, so it
          # keeps its position test regardless of framing -- hence the forced
          # "outer" below, rather than the user's `axes` value. An explicit
          # "none" must still suppress it though, like any other axis, so route
          # that through the shared predicate too. (`draw_facet_axis()` maps
          # side 4 -> "right" and side 1 -> "bottom", matching `rside`.)
          .raxes = facet_window_args[["facet.args"]][["axes"]]
          if (draw_facet_axis(
                rside, ifacet, facet_window_args,
                framed = FALSE, free = FALSE,
                axes = if (identical(.raxes, "none")) "none" else "outer"
              )) {
            spine_axis(rside, type = type_info[["yaxt"]], categorical = FALSE)
          }
      }
      # Outer box for numeric-x spinograms. This is a structural frame, so it
      # follows the top-level `frame.plot` (via type_info) rather than the
      # tile-border `lwd` -- otherwise `lwd = 0` would wrongly drop the box too.
      if (!x.categorical && isTRUE(type_info[["frame.plot"]])) box()
      
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
