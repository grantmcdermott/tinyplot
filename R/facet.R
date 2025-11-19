#' Draw facet windows
#' 
#' @description Internal functions called from `tinyplot` in order to draw the
#' plot window with different facets, grids, axes, etc. 
#' 
#' `facet_layout` determines the layout of the facets, based on a set of inputs.
#' 
#' `draw_facet_window` is the main workhorse function for setting the exterior
#' plot elements as part of a `tinyplot` call, including adjustment of margins
#' for dynamic themes, etc.
#' 
#' @keywords internal
#' @rdname facet
draw_facet_window = function(
    # add arg first, since that determines what happens (if at all)
    add,
    # facet-specific args
    cex_fct_adj,
    facet.args,
    facet_newlines, facet_font, facet_rect, facet_text,
    facet_col, facet_bg, facet_border,
    facet, facets, ifacet,
    nfacets, nfacet_cols, nfacet_rows,
    # axes args
    axes, flip, frame.plot, oxaxis, oyaxis,
    xlabs, xlim, null_xlim, xaxt, xaxs, xaxb, xaxl,
    ylabs, ylim, null_ylim, yaxt, yaxs, yaxb, yaxl,
    asp, log,
    # other args (in approx. alphabetical + group ordering)
    dots,
    draw,
    grid,
    has_legend,
    type,
    x, xmax, xmin,
    y, ymax, ymin,
    tpars = NULL
    ) {
  
  if (is.null(tpars)) tpars = tpar()
  
  # if add is TRUE, just return inputs without any calculations
  if (isTRUE(add)) {
    return(as.list(environment()))
  }
  
  # if breaks are provided use these (but only if x/ylabs are null)
  if (!is.null(xaxb) && !is.null(xlabs)) xlabs = xaxb
  if (!is.null(yaxb) && !is.null(ylabs)) ylabs = yaxb
  
  # draw background color only in the grid rectangle
  grid.bg = get_tpar("grid.bg", tpar_list = tpars)
  if (!is.null(grid.bg)) {
    corners = par("usr")
    rect(corners[1], corners[3], corners[2], corners[4], col = grid.bg, border = NA)
  }

  ## dynamic margins flag
  dynmar = isTRUE(get_tpar("dynmar", tpar_list = tpars))
  
  ## optionally allow to modify the style of axis interval calculation
  if (!is.null(xaxs)) par(xaxs = xaxs)
  if (!is.null(yaxs)) par(yaxs = yaxs)

  if (nfacets > 1) {
    # Set facet margins (i.e., gaps between facets)
    if (is.null(facet.args[["fmar"]])) {
      fmar = tpar("fmar")
    } else {
      if (length(facet.args[["fmar"]]) != 4) {
        warning(
          "`fmar` has to be a vector of length four, e.g.",
          "`facet.args = list(fmar = c(b,l,t,r))`.",
          "\n",
          "Resetting to fmar = c(1,1,1,1) default.",
          "\n"
        )
        fmar = tpar("fmar")
      } else {
        fmar = facet.args[["fmar"]]
      }
    }
    # We need to adjust for n>=3 facet cases for correct spacing...
    if (nfacets >= 3) {
      ## ... exception for 2x2 cases
      if (!(nfacet_rows == 2 && nfacet_cols == 2)) fmar = fmar * .75
    }
    # Extra reduction if no plot frame to reduce whitespace
    if (isFALSE(frame.plot) && !isTRUE(facet.args[["free"]])) {
      fmar = fmar - 0.5
    }

    ooma = par("oma")

    # Bump top margin down for facet titles
    fmar[3] = fmar[3] + 1
    if (isTRUE(attr(facet, "facet_grid"))) {
      fmar[3] = max(0, fmar[3] - 1)
      # Indent for RHS facet_grid title strip if "right!" legend
      if (has_legend && ooma[4] > 0) ooma[4] = ooma[4] + 1
    }
    fmar[3] = fmar[3] + facet_newlines * facet_text / cex_fct_adj

    omar = par("mar")
    
    ## Dynamic plot margin adjustments
    if (dynmar) {
      if (par("las") %in% 1:2) {
        # extra whitespace bump on the y axis
        ## overrides for ridge and some types that use integer spacing with (named) axis labels ## FXIME
        if (type == "ridge") {
          yaxlabs = levels(y)
        } else if (!is.null(ylabs)) {
          yaxlabs = if (!is.null(names(ylabs))) names(ylabs) else ylabs 
        } else if (type == "boxplot" && isTRUE(flip) && !is.null(xlabs)) {
          yaxlabs = if (!is.null(names(xlabs))) names(xlabs) else xlabs 
        } else {
          # yaxl = axTicks(2)
          yaxlabs = axisTicks(usr = extendrange(ylim, f = 0.04), log = par("ylog"))
        }
        if (!is.null(yaxl)) yaxlabs = tinylabel(yaxlabs, yaxl)
        # whtsbp = grconvertX(max(strwidth(yaxl, "figure")), from = "nfc", to = "lines") - 1
        whtsbp = grconvertX(max(strwidth(yaxlabs, "figure")), from = "nfc", to = "lines") - grconvertX(0, from = "nfc", to = "lines") - 1
        if (whtsbp > 0) {
          omar = omar + c(0, whtsbp, 0, 0) * cex_fct_adj
          fmar[2] = fmar[2] + whtsbp * cex_fct_adj
        }
        # Extra reduction if no plot frame to reduce whitespace
        if (isFALSE(frame.plot) && !isTRUE(facet.args[["free"]])) {
          fmar[2] = fmar[2] - (whtsbp * cex_fct_adj)
        }
      }
      if (par("las") %in% 2:3) {
        # extra whitespace bump on the x axis
        # xaxlabs = axTicks(1)
        xaxlabs = if (is.null(xlabs)) axisTicks(usr = extendrange(xlim, f = 0.04), log = par("xlog")) else 
          if (!is.null(names(xlabs))) names(xlabs) else xlabs
        if (!is.null(xaxl)) xaxlabs = tinylabel(xaxlabs, xaxl)
        whtsbp = grconvertX(max(strwidth(xaxlabs, "figure")), from = "nfc", to = "lines") - 1
        if (whtsbp > 0) {
          omar = omar + c(whtsbp, 0, 0, 0) * cex_fct_adj
          fmar[1] = fmar[1] + whtsbp * cex_fct_adj
        }
        # Extra reduction if no plot frame to reduce whitespace
        if (isFALSE(frame.plot) && !isTRUE(facet.args[["free"]])) {
          fmar[1] = fmar[1] - (whtsbp * cex_fct_adj)
        }
      }
      # FIXME: Is this causing issues for lhs legends with facet_grid?
      # catch for missing rhs legend
      if (isTRUE(attr(facet, "facet_grid")) && !has_legend) {
        omar[4] = omar[4] + 1
      }
    }

    # Now we set the margins. The trick here is that we simultaneously adjust
    # inner (mar) and outer (oma) margins by the same amount, but in opposite
    # directions, to preserve the overall facet and plot centroids.
    nmar = (fmar + .1) / cex_fct_adj
    noma = (ooma + omar - fmar - .1) / cex_fct_adj
    # Catch in case of negative oma values. (Probably only occurs with some
    # user-supplied tpar(lmar) values and a "left!" positioned legend.)
    if (any(noma < 0)) {
      noma_orig = noma
      noma[noma < 0] = 0
      # noma_diff = noma-noma_orig
      # nmar = nmar + noma_diff
    }
    # apply changes
    par(oma = noma)
    par(mar = nmar)

    # Now that the margins have been set, arrange facet rows and columns based
    # on our earlier calculations.
    par(mfrow = c(nfacet_rows, nfacet_cols))
  } else if (dynmar) {
    # Dynamic plot margin adjustments
    omar = par("mar")
    omar = omar - c(0, 0, 1, 0) # reduce top whitespace since no facet (title)
    if (type == "spineplot") omar[4] = 2.1 # FIXME catch for spineplot RHS axis labs
    if (par("las") %in% 1:2) {
      # extra whitespace bump on the y axis
      ## overrides for ridge and some types that use integer spacing with (named) axis labels ## FXIME
      if (type == "ridge") {
        yaxlabs = levels(y)
      } else if (!is.null(ylabs)) {
        yaxlabs = if (!is.null(names(ylabs))) names(ylabs) else ylabs 
      } else if (type == "boxplot" && isTRUE(flip) && !is.null(xlabs)) {
        yaxlabs = if (!is.null(names(xlabs))) names(xlabs) else xlabs 
      } else {
        # yaxl = axTicks(2)
        yaxlabs = axisTicks(usr = extendrange(ylim, f = 0.04), log = par("ylog"))
      }
      if (!is.null(yaxl)) yaxlabs = tinylabel(yaxlabs, yaxl)
      # whtsbp = grconvertX(max(strwidth(yaxlabs, "figure")), from = "nfc", to = "lines") - 1
      whtsbp = grconvertX(max(strwidth(yaxlabs, "figure")), from = "nfc", to = "lines") - grconvertX(0, from = "nfc", to = "lines") - 1
      if (whtsbp > 0) {
        omar[2] = omar[2] + whtsbp
      }
    }
    if (par("las") %in% 2:3) {
      # extra whitespace bump on the x axis
      # xaxl = axTicks(1)
      xaxlabs = if (is.null(xlabs)) axisTicks(usr = extendrange(xlim, f = 0.04), log = par("xlog")) else 
        if (!is.null(names(xlabs))) names(xlabs) else xlabs
      if (!is.null(xaxl)) xaxlabs = tinylabel(xaxlabs, xaxl)
      whtsbp = grconvertX(max(strwidth(xaxlabs, "figure")), from = "nfc", to = "lines") - 1
      if (whtsbp > 0) {
        omar[1] = omar[1] + whtsbp
      }
    }
     par(mar = omar)
  }

  ## Loop over the individual facet windows and draw the plot region
  ## components (axes, titles, box, grid, etc.)
  for (ii in ifacet) {
    # See: https://github.com/grantmcdermott/tinyplot/issues/65
    if (nfacets > 1) {
      mfgi = ceiling(ii / nfacet_cols)
      mfgj = ii %% nfacet_cols
      if (mfgj == 0) mfgj = nfacet_cols
      par(mfg = c(mfgi, mfgj))
    }

    ## Set the plot window
    ## Problem: Passing extra args through ... (e.g., legend_args) to plot.window
    ## triggers an annoying warning about unrecognized graphical params.
    # plot.window(
    #   xlim = xlim, ylim = ylim,
    #   asp = asp, log = log,
    #   # ...
    # )
    ## Solution: Only pass on relevant args using name checking and do.call.
    ## Idea borrowed from here: https://stackoverflow.com/a/4128401/4115816
    pdots = dots[names(dots) %in% names(formals(plot.default))]
    ## catch for flipped boxplots...
    if (type == "boxplot" && isTRUE(flip)) {
      log_flip = log
      if (!is.null(log)) {
        if (log == "x") log_flip = "y"
        if (log == "y") log_flip = "x"
      }
      do.call(
        "plot.window",
        c(list(xlim = ylim, ylim = xlim, asp = asp, log = log_flip), pdots)
      )
      xside = 2
      yside = 1
    } else {
      ## ... standard plot window for all other cases
      do.call(
        "plot.window",
        c(list(xlim = xlim, ylim = ylim, asp = asp, log = log), pdots)
      )
      xside = 1
      yside = 2
    }


    # axes, frame.plot and grid
    if (isTRUE(axes) || isTRUE(facet.args[["free"]])) {
      args_x = list(x,
        side = xside,
        type = xaxt,
        labeller = xaxl,
        cex = get_tpar(c("cex.xaxs", "cex.axis"), 0.8, tpar_list = tpars),
        lwd = get_tpar(c("lwd.xaxs", "lwd.axis"), 1, tpar_list = tpars),
        lty = get_tpar(c("lty.xaxs", "lty.axis"), 1, tpar_list = tpars)
      )
      args_y = list(y,
        side = yside,
        type = yaxt,
        labeller = yaxl,
        cex = get_tpar(c("cex.yaxs", "cex.axis"), 0.8, tpar_list = tpars),
        lwd = get_tpar(c("lwd.yaxs", "lwd.axis"), 1, tpar_list = tpars),
        lty = get_tpar(c("lty.yaxs", "lty.axis"), 1, tpar_list = tpars)
      )
      if (!is.null(xaxb)) args_x$at = xaxb
      if (!is.null(yaxb)) args_y$at = yaxb
      type_range_x = type %in% c("barplot", "pointrange", "errorbar", "ribbon", "boxplot", "p", "violin") && !is.null(xlabs)
      type_range_y = !is.null(ylabs) && (type == "p" || (isTRUE(flip) && type %in% c("barplot", "pointrange", "errorbar", "ribbon", "boxplot", "violin")))
      if (type_range_x) {
        args_x = modifyList(args_x, list(at = xlabs, labels = names(xlabs)))
      }
      if (type_range_y) {
        args_y = modifyList(args_y, list(at = ylabs, labels = names(ylabs)))
      }

      if (isTRUE(facet.args[["free"]]) && (par("xlog") || par("ylog"))) {
        warning(
          "\nFree scale axes for faceted plots are currently not supported if the axes are logged. Reverting back to fixed scales.",
          "\nIf support for this feature is important to you, please raise an issue on our GitHub repo:",
          "\nhttps://github.com/grantmcdermott/tinyplot/issues\n"
        )
        facet.args[["free"]] = FALSE
      }

      # Special logic if facets are free...
      if (isTRUE(facet.args[["free"]])) {
        # First, we need to calculate the plot extent and axes range of each
        # individual facet.
        xfree = split(c(x, xmin, xmax), facet)[[ii]]
        yfree = split(c(y, ymin, ymax), facet)[[ii]]
        if (null_xlim) xlim = range(xfree, na.rm = TRUE)
        if (null_ylim) ylim = range(yfree, na.rm = TRUE)
        xext = extendrange(xlim, f = 0.04)
        yext = extendrange(ylim, f = 0.04)
        # We'll save this in a special .fusr env var (list) that we'll re-use
        # when it comes to plotting the actual elements later
        if (ii == 1) {
          fusr = replicate(4, vector("double", length = nfacets), simplify = FALSE)
          assign(".fusr", fusr, envir = get(".tinyplot_env", envir = parent.env(environment())))
        }
        fusr = get(".fusr", envir = get(".tinyplot_env", envir = parent.env(environment())))
        fusr[[ii]] = c(xext, yext)
        assign(".fusr", fusr, envir = get(".tinyplot_env", envir = parent.env(environment())))
        # Explicitly set (override) the current facet extent
        par(usr = fusr[[ii]])
        # if plot frame is true then print axes per normal...
        if (type %in% c("barplot", "pointrange", "errorbar", "ribbon", "boxplot", "p", "violin") && !is.null(xlabs)) {
          tinyAxis(xfree, side = xside, at = xlabs, labels = names(xlabs), type = xaxt, labeller = xaxl)
        } else {
          tinyAxis(xfree, side = xside, type = xaxt, labeller = xaxl)
        }
        if (isTRUE(flip) && type %in% c("barplot", "pointrange", "errorbar", "ribbon", "boxplot", "p", "violin") && !is.null(ylabs)) {
          tinyAxis(yfree, side = yside, at = ylabs, labels = names(ylabs), type = yaxt, labeller = yaxl)
        } else {
          tinyAxis(yfree, side = yside, type = yaxt, labeller = yaxl)
        }

        # For fixed facets we can just reuse the same plot extent and axes limits
      } else if (isTRUE(frame.plot)) {
        # if plot frame is true then print axes per normal...
        do.call(tinyAxis, args_x)
        do.call(tinyAxis, args_y)
      } else {
        # ... else only print the "outside" axes.
        if (ii %in% oxaxis) do.call(tinyAxis, args_x)
        if (ii %in% oyaxis) do.call(tinyAxis, args_y)
      }
    }

    # facet titles
    ## Note: facet titles could be done more simply with mtext... but then we
    ## couldn't adjust background features (e.g., fill), or rotate the rhs
    ## facet grid text. So we're rolling our own "manual" versions with text
    ## and rect.
    if (!is.null(facet)) {
      # Get the four corners of plot area (x1, x2, y1, y2)
      corners = par("usr")
      # catch for logged axes
      xlog = isTRUE(par("xlog"))
      ylog = isTRUE(par("ylog"))
      if (xlog) corners[1:2] = 10^(corners[1:2])
      if (ylog) corners[3:4] = 10^(corners[3:4])
      # special logic for facet grids
      if (is.null(facet_newlines) || facet_newlines == 0) {
        facet_title_lines = 1
      } else {
        facet_title_lines = 1 + facet_newlines
      }
      # different logic for facet grids versus regular facets
      if (isTRUE(attr(facet, "facet_grid"))) {
        ## top facet strips
        if (ii %in% 1:nfacet_cols) {
          if (isTRUE(facet_rect)) {
            line_height = (facet_title_lines + .1) * facet_text / cex_fct_adj
            if (ylog) {
              line_height = grconvertY(line_height, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
              rect_height = corners[4] * line_height
            } else {
              line_height = grconvertY(line_height, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
              rect_height = corners[4] + line_height
            }
            rect(
              corners[1], corners[4], corners[2], rect_height,
              col = facet_bg, border = facet_border,
              xpd = NA
            )
          }
          xpos = if (xlog) 10^(mean(log10(corners[1:2]))) else mean(corners[1:2])
          if (ylog) {
            ypos = grconvertY(0.4, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
            ypos = corners[4] * ypos
          } else {
            ypos = grconvertY(0.4, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
            ypos = corners[4] + ypos
          }
          text(
            x = xpos,
            y = ypos,
            labels = sub("^(.*?)~.*", "\\1", facets[[ii]]),
            adj = c(0.5, 0),
            cex = facet_text / cex_fct_adj,
            col = facet_col,
            font = facet_font,
            xpd = NA,
          )
        }
        ## right facet strips
        if (ii %% nfacet_cols == 0 || ii == nfacets) {
          if (isTRUE(facet_rect)) {
            line_height = (facet_title_lines + .1) * facet_text / cex_fct_adj
            if (xlog) {
              line_height = grconvertX(line_height, from = "lines", to = "user") / grconvertX(0, from = "lines", to = "user")
              rect_width = corners[2] * line_height
            } else {
              line_height = grconvertX(line_height, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user")
              rect_width = corners[2] + line_height
            }
            rect(
              corners[2], corners[3], rect_width, corners[4],
              col = facet_bg, border = facet_border,
              xpd = NA
            )
          }
          if (xlog) {
            xpos = grconvertX(0.4, from = "lines", to = "user") / grconvertX(0, from = "lines", to = "user")
            xpos = corners[2] * xpos
          } else {
            xpos = grconvertX(0.4, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user")
            xpos = corners[2] + xpos
          }
          ypos = if (ylog) 10^(mean(log10(corners[3:4]))) else mean(corners[3:4])
          text(
            x = xpos,
            y = ypos,
            labels = sub("^.*?~(.*)", "\\1", facets[[ii]]),
            srt = 270,
            adj = c(0.5, 0),
            cex = facet_text / cex_fct_adj,
            col = facet_col,
            font = facet_font,
            xpd = NA
          )
        }
      } else {
        if (isTRUE(facet_rect)) {
          line_height = (facet_title_lines + .1) * facet_text / cex_fct_adj
          if (ylog) {
            line_height = grconvertY(line_height, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
            rect_height = corners[4] * line_height
          } else {
            line_height = grconvertY(line_height, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
            rect_height = corners[4] + line_height
          }
          rect(
            corners[1], corners[4], corners[2], rect_height,
            col = facet_bg, border = facet_border,
            xpd = NA
          )
        }
        xpos = if (xlog) 10^(mean(log10(corners[1:2]))) else mean(corners[1:2])
        if (ylog) {
          ypos = grconvertY(0.4, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
          ypos = corners[4] * ypos
        } else {
          ypos = grconvertY(0.4, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
          ypos = corners[4] + ypos
        }
        text(
          x = xpos,
          y = ypos,
          labels = paste(facets[[ii]]),
          adj = c(0.5, 0),
          cex = facet_text / cex_fct_adj,
          col = facet_col,
          font = facet_font,
          xpd = NA
        )
      }
    }

    # plot frame
    if (frame.plot) box()

    # panel grid lines
    if (is.null(grid)) grid = get_tpar("grid", tpar_list = tpars)
    if (!is.null(grid)) {
      if (is.logical(grid)) {
        ## If grid is TRUE create a default grid. Rather than just calling the default grid()
        ## abline(... = pretty(extendrange(...)), ...) is used. Reason: pretty() is generic
        ## and works better for axes based on date/time classes. Exception: For axes in logs,
        ## resort to using grid() which is likely better handled there.
        if (isTRUE(grid)) {
          gnx = gny = NULL
          if (!is.null(xaxb)) {
            abline(v = xaxb, col = get_tpar("grid.col", tpar_list = tpars), lty = get_tpar("grid.lty", tpar_list = tpars), lwd = get_tpar("grid.lwd", tpar_list = tpars))
            gnx = NA
          } else if (!any(c(par("xlog"), type == "boxplot"))) {
            xg = if (!inherits(x, c("POSIXt", "Date"))) axTicks(side = 1) else axTicksDateTime(side = 1, x = x)
            abline(v = xg, col = get_tpar("grid.col", tpar_list = tpars), lty = get_tpar("grid.lty", tpar_list = tpars), lwd = get_tpar("grid.lwd", tpar_list = tpars))
            gnx = NA
          }
          if (!is.null(yaxb)) {
            abline(h = yaxb, col = get_tpar("grid.col", tpar_list = tpars), lty = get_tpar("grid.lty", tpar_list = tpars), lwd = get_tpar("grid.lwd", tpar_list = tpars))
            gny = NA
          } else if (!any(c(par("ylog"), type == "boxplot"))) {
            yg = if (!inherits(y, c("POSIXt", "Date"))) axTicks(side = 2) else axTicksDateTime(side = 2, x = x)
            abline(h = yg, col = get_tpar("grid.col", tpar_list = tpars), lty = get_tpar("grid.lty", tpar_list = tpars), lwd = get_tpar("grid.lwd", tpar_list = tpars))
            gny = NA
          }
          grid(nx = gnx, ny = gny, col = get_tpar("grid.col", tpar_list = tpars), lty = get_tpar("grid.lty", tpar_list = tpars), lwd = get_tpar("grid.lwd", tpar_list = tpars))
        }
      } else {
        grid
      }
    }

    # add any drawn elements
    if (!is.null(draw)) eval(draw)
  } # end of ii facet loop

  return(as.list(environment()))
}


#' @rdname facet
#' @keywords internal
#' @param settings A list of settings as created by `tinyplot()`.
facet_layout = function(settings) {
  # Extract needed variables from settings
  add = settings$add
  facet.args = settings$facet.args
  datapoints = settings$datapoints
  facet_attr = settings$facet_attr

  # Simplify facet if only one unique value
  facet = datapoints$facet
  if (!is.null(facet) && length(unique(facet)) == 1) {
    facet = NULL
    datapoints$facet = NULL
  }

  # Restore facet attributes
  if (!is.null(facet)) {
    attributes(facet) = facet_attr
    attributes(datapoints$facet) = facet_attr
  }

  nfacet_rows = 1
  nfacet_cols = 1
  if (!is.null(facet)) {
    facets = if (is.factor(facet)) levels(facet) else sort(unique(facet))
    ifacet = seq_along(facets)
    nfacets = length(facets)
    if (isTRUE(add)) {
      omfrow = par("mfrow")
      nfacet_rows = omfrow[1]
      nfacet_cols = omfrow[2]
    } else {
      if (isTRUE(attr(facet, "facet_grid"))) {
        facet.args[["nrow"]] = attr(facet, "facet_nrow")
      }
      if (!is.null(facet.args[["nrow"]])) {
        nfacet_rows = facet.args[["nrow"]]
        nfacet_cols = ceiling(nfacets / nfacet_rows)
      } else if (!is.null(facet.args[["ncol"]])) {
        nfacet_cols = facet.args[["ncol"]]
        nfacet_rows = ceiling(nfacets / nfacet_cols)
      } else {
        if (nfacets > 3) {
          nfacet_cols = ceiling(sqrt(nfacets))
          nfacet_rows = ceiling(nfacets / nfacet_cols)
        } else {
          nfacet_rows = 1L
          nfacet_cols = nfacets
        }
      }
    }

    oxaxis = tail(ifacet, nfacet_cols)
    oyaxis = seq(1, nfacets, by = nfacet_cols)

    if (nfacet_rows >= 3 || nfacet_cols >= 3) {
      cex_fct_adj = 0.66
    } else if (nfacet_rows == 2 && nfacet_cols == 2) {
      cex_fct_adj = 0.83
    } else {
      cex_fct_adj = 1
    }
  } else {
    facets = ifacet = nfacets = oxaxis = oyaxis = 1
    cex_fct_adj = 1
  }

  # update settings
  env2env(
    environment(),
    settings,
    c("datapoints", "facets", "ifacet", "nfacets", "nfacet_rows", "nfacet_cols", "oxaxis", "oyaxis", "cex_fct_adj")
  )
}



#
# helper functions
#


# utility function for converting facet formulas into variables
get_facet_fml = function(formula, data = NULL) {
  xfacet = yfacet = NULL

  ## catch one-sided formula ~ x or ~ x | z with no "y" variable
  if (!inherits(formula, "formula")) formula = as.formula(formula)
  no_yfacet = length(formula) == 2L
  fml_rhs = if (no_yfacet) 2L else 3L

  ## set up model frame
  m = match.call(expand.dots = FALSE)

  if (!is.null(data)) {
    m = m[c(1L, match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(m), 0L))]
  }

  m$formula = formula
  ## need stats:: for non-standard evaluation
  m[[1L]] = quote(stats::model.frame)
  mf = eval.parent(m)

  ## extract variables: x, y (if any)
  if (no_yfacet) {
    yfacet_loc = NULL
    xfacet_loc = 1L
  } else {
    yfacet_loc = 1L
    xfacet_loc = 2L
  }
  if (NCOL(mf) < xfacet_loc) stop("formula should specify at least one variable on the right-hand side")
  yfacet = if (no_yfacet) NULL else mf[, yfacet_loc]
  xfacet = mf[, xfacet_loc:NCOL(mf)]

  ## return object
  xfacet = interaction(xfacet, sep = ":")
  if (no_yfacet) {
    ret = xfacet
  } else {
    # yfacet = interaction(yfacet, sep = ":")
    ## NOTE: We "swap" the formula LHS and RHS since mfrow plots rowwise
    ret = interaction(xfacet, yfacet, sep = "~")
    attr(ret, "facet_grid") = TRUE
    attr(ret, "facet_nrow") = length(unique(yfacet))
  }

  return(ret)
}


## internal convenience function to determine whether the current facet panel
## has the position "left", "right", "top", or "bottom" in the facet grid
is_facet_position = function(position, ifacet, facet_window_args) {
  id = facet_window_args$ifacet
  nc = facet_window_args$nfacet_cols
  ni = tail(id, 1L)
  switch(position,
    "left"   = ifacet %in% seq(1L, ni, by = nc),
    "right"  = ifacet %in% pmin(ni, seq(1L, ni, by = nc) + nc - 1L),
    "top"    = ifacet %in% head(id, nc),
    "bottom" = ifacet %in% tail(id, nc),
    NA
  )
}
