# Facet layout structure
#
# This function is called by `tinyplot`. Given some inputs, it returns 
# information about the layout of the facets.
#
facet_layout = function(facet, add = FALSE, facet.args = list()) {
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
  
  list(
    facets = facets,
    ifacet = ifacet,
    nfacets = nfacets,
    nfacet_rows = nfacet_rows,
    nfacet_cols = nfacet_cols,
    oxaxis = oxaxis,
    oyaxis = oyaxis,
    cex_fct_adj = cex_fct_adj
  )
}



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




# internal function to draw window with different facets, grids, axes, etc.

draw_facet_window = function(grid, ...) {

  list2env(list(...), environment())

  if (isFALSE(add)) {
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
      if (isFALSE(frame.plot)) {
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
      if (type == "boxplot" && isTRUE(dots[["horizontal"]])) {
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
      if (isTRUE(axes)) {
        if (isTRUE(frame.plot)) {
          # if plot frame is true then print axes per normal...
          if (type %in% c("pointrange", "errorbar", "ribbon", "boxplot", "p") && !is.null(xlabs)) {
            tinyAxis(x, side = xside, at = xlabs, labels = names(xlabs), type = xaxt)
          } else {
            tinyAxis(x, side = xside, type = xaxt)
          }
          tinyAxis(y, side = yside, type = yaxt)
        } else {
          # ... else only print the "outside" axes.
          if (ii %in% oxaxis) {
            if (type %in% c("pointrange", "errorbar", "ribbon", "boxplot", "p") && !is.null(xlabs)) {
              tinyAxis(x, side = xside, at = xlabs, labels = names(xlabs), type = xaxt)
            } else {
              tinyAxis(x, side = xside, type = xaxt)
            }
          }
          if (ii %in% oyaxis) {
            tinyAxis(y, side = yside, type = yaxt)
          }
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
      if (is.null(grid)) grid = .tpar[["grid"]]
      if (!is.null(grid)) {
        if (is.logical(grid)) {
          ## If grid is TRUE create a default grid. Rather than just calling the default grid()
          ## abline(... = pretty(extendrange(...)), ...) is used. Reason: pretty() is generic
          ## and works better for axes based on date/time classes. Exception: For axes in logs,
          ## resort to using grid() which is likely better handled there.
          if (isTRUE(grid)) {
            gnx = gny = NULL
            if (!par("xlog")) {
              abline(v = pretty(extendrange(x)), col = "lightgray", lty = "dotted", lwd = par("lwd"))
              gnx = NA
            }
            if (!par("ylog")) {
              abline(h = pretty(extendrange(c(y, ymin, ymax))), col = "lightgray", lty = "dotted", lwd = par("lwd"))
              gny = NA
            }
            grid(nx = gnx, ny = gny)
          }
        } else {
          grid
        }
      }
    } # end of ii facet loop
  } # end of add check

  return(as.list(environment()))
}
