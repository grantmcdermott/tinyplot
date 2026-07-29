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
    rev_x = FALSE, rev_y = FALSE,
    asp, log,
    # other args (in approx. alphabetical + group ordering)
    dots,
    draw,
    grid,
    has_legend,
    main,
    sub,
    cap,
    type,
    type_hints = NULL,
    xlab,
    x, xmax, xmin,
    ylab,
    y, ymax, ymin,
    tpars = NULL,
    dynmar_computed = NULL,
    dl_overshoot = 0
    ) {
  
  if (is.null(tpars)) tpars = tpar()
  
  # if add is TRUE, just return inputs without any calculations
  if (isTRUE(add)) {
    return(as.list(environment()))
  }
  
  # validate the `axes` facet argument up front, so a typo doesn't silently fall
  # back to the default behaviour further down
  if (!is.null(facet.args[["axes"]])) {
    .axes_ok = c("all", "outer", "none")
    if (length(facet.args[["axes"]]) != 1L || !facet.args[["axes"]] %in% .axes_ok) {
      warning(
        "`axes` has to be one of ",
        paste(sprintf('"%s"', .axes_ok), collapse = ", "),
        ", e.g. `facet.args = list(axes = \"outer\")`.",
        "\n",
        "Ignoring.",
        "\n"
      )
      facet.args[["axes"]] = NULL
    }
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
  
  ## optionally allow to modify and restore the style of axis interval calculation
  if (!is.null(xaxs) || !is.null(yaxs)) {
    op = par()
    if (!is.null(xaxs)) {
      par(xaxs = xaxs)
      on.exit(par(xaxs = op$xaxs), add = TRUE)
    }
    if (!is.null(yaxs)) {
      par(yaxs = yaxs)
      on.exit(par(yaxs = op$yaxs), add = TRUE)
    }
  }

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

    # Types that draw their own axes may force `frame.plot = FALSE` so the
    # pipeline skips the box (e.g. data_spineplot()), while surfacing the user's
    # real choice via the `framed` hint. Use the hint where given, so the margin
    # logic below matches what the type will actually draw.
    .framed = if (!is.null(type_hints[["framed"]])) {
      isTRUE(type_hints[["framed"]])
    } else {
      isTRUE(frame.plot)
    }

    # Will any *interior* (non-edge) facet draw its own axis on this side? If so
    # that facet needs the tick-label width in its own margin, rather than the
    # single outer allocation that the nmar/noma split below would otherwise
    # make. Mirrors draw_facet_axis(), so the margin and the axis agree.
    .interior_axis = function(side) {
      if (nfacets <= 1) return(FALSE)
      fwa = list(ifacet = ifacet, nfacet_cols = nfacet_cols)
      keep = vapply(
        ifacet,
        function(ii) draw_facet_axis(
          side, ii, fwa,
          framed = .framed,
          free = isTRUE(facet.args[["free"]]),
          axes = facet.args[["axes"]]
        ),
        logical(1L)
      )
      # more panels draw this axis than sit on its outer edge => interior draws
      edge = vapply(
        ifacet,
        function(ii) draw_facet_axis(side, ii, fwa, framed = FALSE, free = FALSE, axes = "outer"),
        logical(1L)
      )
      sum(keep) > sum(edge)
    }

    # Bump top margin for facet strip. Use facet_text (not / cex_fct_adj)
    # because nmar = (fmar + 0.1) / cex_fct_adj already divides — using
    # facet_text directly keeps the inter-panel gap constant as newlines grow.
    fmar[3] = fmar[3] + facet_text
    if (isTRUE(attr(facet, "facet_grid"))) {
      fmar[3] = max(0, fmar[3] - facet_text)
      # Indent for RHS facet_grid title strip if "right!" legend
      if (has_legend && ooma[4] > 0) ooma[4] = ooma[4] + 1
    }
    fmar[3] = fmar[3] + facet_newlines * facet_text

    omar = par("mar")
    
    ## Dynamic plot margin adjustments
    if (dynmar) {
      # Margins were pre-computed in tinyplot.default (dynmar_computed).
      # Use that as the base instead of par("mar") which may have been
      # reset by the before.plot.new hook.
      side.sub = get_tpar("side.sub", tpar_list = tpars, default = 3)
      omar = dynmar_computed
      omar[3] = dynmar_computed[3] + (1 + facet_newlines + 0.1) * facet_text
      # Ensure fmar[3] doesn't exceed omar[3] - 0.1, which would make
      # noma[3] negative and get clamped to 0, creating excess top space.
      if (fmar[3] + 0.1 > omar[3]) fmar[3] = omar[3] - 0.1
      if (par("las") %in% 1:2) {
        # extra whitespace bump on the y axis
        .ylabset = y_axis_labels(type, y, ylabs, xlabs, flip)
        if (!is.null(.ylabset)) {
          yaxlabs = .ylabset[[1L]]
        } else {
          if (isTRUE(facet.args[["free"]]) && null_ylim && !is.null(facet)) {
            # Free scales: measure every facet's ticks and keep the widest set.
            yfree_split = split(c(y, ymin, ymax), facet)
            yaxlabs_all = lapply(yfree_split, function(yf) {
              axisTicks(usr = extendrange(range(yf, na.rm = TRUE), f = 0.04), log = par("ylog"))
            })
            widths = vapply(yaxlabs_all, function(labs) max(strwidth(labs, "inches", cex = par("cex.axis"))), numeric(1L))
            yaxlabs = yaxlabs_all[[which.max(widths)]]
          } else {
            yaxlabs = axisTicks(usr = extendrange(ylim, f = 0.04), log = par("ylog"))
          }
        }
        if (!is.null(yaxl)) yaxlabs = tinylabel(yaxlabs, yaxl)
        # whtsbp = grconvertX(max(strwidth(yaxl, "figure")), from = "nfc", to = "lines") - 1
        whtsbp = grconvertX(max(strwidth(yaxlabs, "figure", cex = par("cex.axis"))), from = "nfc", to = "lines") - grconvertX(0, from = "nfc", to = "lines") - 0.5
        if (whtsbp > 0) {
          omar = omar + c(0, whtsbp, 0, 0) * cex_fct_adj
          fmar[2] = fmar[2] + whtsbp * cex_fct_adj
        }
        # The label width above is reserved once, and the nmar/noma split below
        # hands it to the *outer* margin -- correct when only the leftmost facet
        # draws a y axis. But when interior facets draw their own (e.g. framed
        # panels), each needs that width in its own margin instead, else the
        # labels overflow into the neighbouring panel. Keep the fmar bump in that
        # case; otherwise release it back to the outer margin as before.
        if (!.framed && !isTRUE(facet.args[["free"]]) && !.interior_axis(2)) {
          fmar[2] = fmar[2] - (whtsbp * cex_fct_adj)
        }
      }
      if (par("las") %in% 2:3) {
        # extra whitespace bump on the x axis
        if (is.null(xlabs) && isTRUE(facet.args[["free"]]) && null_xlim && !is.null(facet)) {
          xfree_split = split(c(x, xmin, xmax), facet)
          xaxlabs_all = lapply(xfree_split, function(xf) {
            axisTicks(usr = extendrange(range(xf, na.rm = TRUE), f = 0.04), log = par("xlog"))
          })
          widths = vapply(xaxlabs_all, function(labs) max(strwidth(labs, "inches", cex = par("cex.axis"))), numeric(1L))
          xaxlabs = xaxlabs_all[[which.max(widths)]]
        } else {
          xaxlabs = if (is.null(xlabs)) axisTicks(usr = extendrange(xlim, f = 0.04), log = par("xlog")) else
            if (!is.null(names(xlabs))) names(xlabs) else xlabs
        }
        if (!is.null(xaxl)) xaxlabs = tinylabel(xaxlabs, xaxl)
        whtsbp = grconvertX(max(strwidth(xaxlabs, "figure", cex = par("cex.axis"))), from = "nfc", to = "lines") - 0.5
        if (whtsbp > 0) {
          omar = omar + c(whtsbp, 0, 0, 0) * cex_fct_adj
          fmar[1] = fmar[1] + whtsbp * cex_fct_adj
        }
        # As per the y axis above: keep the label width in fmar when interior
        # facets draw their own x axis, else release it to the outer margin.
        if (!.framed && !isTRUE(facet.args[["free"]]) && !.interior_axis(1)) {
          fmar[1] = fmar[1] - (whtsbp * cex_fct_adj)
        }
      }

      # reserve RHS margin for types with a secondary axis (e.g. spineplot)
      if (isTRUE(type_hints[["has_rhs_axis"]])) omar[4] = 2.1

      # FIXME: Is this causing issues for lhs legends with facet_grid?
      # catch for missing rhs legend
      if (isTRUE(attr(facet, "facet_grid")) && !has_legend) {
        omar[4] = omar[4] + 1
      }
    }

    if (dl_overshoot > 0) {
      fmar[4] = fmar[4] + dl_overshoot
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
    # Dynamic plot margin adjustments (no facets). Margins were pre-computed
    # in tinyplot.default and passed via dynmar_computed; use them directly.
    # Tick-label *width/height* (whtsbp) is added further below.
    side.sub = get_tpar("side.sub", tpar_list = tpars, default = 3)
    omar = dynmar_computed
    # reserve RHS margin for types with a secondary axis (e.g. spineplot)
    if (isTRUE(type_hints[["has_rhs_axis"]])) omar[4] = 2.1
    if (par("las") %in% 1:2) {
      # extra whitespace bump on the y axis
      .ylabset = y_axis_labels(type, y, ylabs, xlabs, flip)
      if (!is.null(.ylabset)) {
        yaxlabs = .ylabset[[1L]]
      } else {
        ylim_usr = if (diff(ylim) == 0 && is.null(yaxb)) ylim + c(-0.5, 0.5) else extendrange(ylim, f = 0.04)
        yaxlabs = axisTicks(usr = ylim_usr, log = par("ylog"))
      }
      if (!is.null(yaxl)) yaxlabs = tinylabel(yaxlabs, yaxl)
      # whtsbp = grconvertX(max(strwidth(yaxlabs, "figure", cex = par("cex.axis"))), from = "nfc", to = "lines") - 1
      whtsbp = grconvertX(max(strwidth(yaxlabs, "figure", cex = par("cex.axis"))), from = "nfc", to = "lines") - grconvertX(0, from = "nfc", to = "lines") - 0.5
      omar[2] = omar[2] + whtsbp
    }
    if (par("las") %in% 2:3) {
      # extra whitespace bump on the x axis
      # xaxl = axTicks(1)
      xlim_usr = if (diff(xlim) == 0 && is.null(xaxb)) xlim + c(-0.5, 0.5) else extendrange(xlim, f = 0.04)
      xaxlabs = if (is.null(xlabs)) axisTicks(usr = xlim_usr, log = par("xlog")) else
        if (!is.null(names(xlabs))) names(xlabs) else xlabs
      if (!is.null(xaxl)) xaxlabs = tinylabel(xaxlabs, xaxl)
      whtsbp = grconvertX(max(strwidth(xaxlabs, "figure", cex = par("cex.axis"))), from = "nfc", to = "lines") - 0.5
      omar[1] = omar[1] + whtsbp
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
      .ca = get_tpar(c("cex.yaxs", "cex.axis"), 0.8, tpar_list = tpars)
      .ymgp_shift = if (par("las") %in% c(0L, 1L)) 0.5 * (.ca - 1) else 0
      args_y = list(y,
        side = yside,
        type = yaxt,
        labeller = yaxl,
        cex = .ca,
        lwd = get_tpar(c("lwd.yaxs", "lwd.axis"), 1, tpar_list = tpars),
        lty = get_tpar(c("lty.yaxs", "lty.axis"), 1, tpar_list = tpars)
      )
      if (!is.null(xaxb)) args_x$at = xaxb
      if (!is.null(yaxb)) args_y$at = yaxb
      # `xlabs` is only non-NULL when a type has placed categorical data on the
      # x-axis, so its presence is the signal to draw labelled ticks.
      type_range_x = !is.null(xlabs)
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
        xfree = if (!is.null(facet)) split(c(x, xmin, xmax), facet)[[ii]] else c(x, xmin, xmax)
        yfree = if (!is.null(facet)) split(c(y, ymin, ymax), facet)[[ii]] else c(y, ymin, ymax)
        if (null_xlim) xlim = range(xfree, na.rm = TRUE)
        if (null_ylim) ylim = range(yfree, na.rm = TRUE)
        # An axis is reversed either via the `rev_x`/`rev_y` flag (e.g. the
        # "reverse" keyword) or when the user supplies descending fixed limits
        # (e.g. xlim = c(10, 0)). The latter must be detected before extendrange()
        # below, which always returns an ascending pair and would otherwise drop
        # the descending order. (#644)
        rev_xext = isTRUE(rev_x) || (!null_xlim && length(xlim) == 2L && xlim[2L] < xlim[1L])
        rev_yext = isTRUE(rev_y) || (!null_ylim && length(ylim) == 2L && ylim[2L] < ylim[1L])
        # extendrange() returns an ascending pair, so reverse afterwards
        xext = extendrange(sort(xlim), f = 0.04)
        yext = extendrange(sort(ylim), f = 0.04)
        # A facet with a single distinct x (or y) value yields a zero-width
        # extent, which par(usr=) rejects. Mirror base plot.window() and pad
        # a degenerate range symmetrically so the facet still draws. (#668)
        if (diff(xext) == 0) xext = xext + c(-1, 1) * (if (xext[1L] == 0) 1 else 0.04 * abs(xext[1L]))
        if (diff(yext) == 0) yext = yext + c(-1, 1) * (if (yext[1L] == 0) 1 else 0.04 * abs(yext[1L]))
        # base axTicks() misbehaves on a reversed usr (it collapses to a single
        # tick), so precompute ticks from the ascending extent and pass them as
        # an explicit `at` below; placement against the reversed usr is fine.
        xat = if (rev_xext) axisTicks(usr = xext, log = par("xlog")) else NULL
        yat = if (rev_yext) axisTicks(usr = yext, log = par("ylog")) else NULL
        if (rev_xext) xext = rev(xext)
        if (rev_yext) yext = rev(yext)
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
        # Free facets each need their own axes, since every panel has its own
        # scale. The one exception is an explicit `axes = "none"` request.
        .free_axes = !identical(facet.args[["axes"]], "none")
        # if plot frame is true then print axes per normal...
        if (.free_axes) {
          if (!is.null(xlabs)) {
            tinyAxis(xfree, side = xside, at = xlabs, labels = names(xlabs), type = xaxt, labeller = xaxl)
          } else if (!is.null(xat)) {
            tinyAxis(xfree, side = xside, at = xat, type = xaxt, labeller = xaxl)
          } else {
            tinyAxis(xfree, side = xside, type = xaxt, labeller = xaxl)
          }
        }
        if (.ymgp_shift > 0) par(mgp = par("mgp") - c(0, .ymgp_shift, 0))
        if (.free_axes) {
          if (isTRUE(flip) && type %in% c("barplot", "pointrange", "errorbar", "ribbon", "boxplot", "p", "violin") && !is.null(ylabs)) {
            tinyAxis(yfree, side = yside, at = ylabs, labels = names(ylabs), type = yaxt, labeller = yaxl)
          } else if (!is.null(yat)) {
            tinyAxis(yfree, side = yside, at = yat, type = yaxt, labeller = yaxl)
          } else {
            tinyAxis(yfree, side = yside, type = yaxt, labeller = yaxl)
          }
        }
        if (.ymgp_shift > 0) par(mgp = par("mgp") + c(0, .ymgp_shift, 0))

        # For fixed facets we can just reuse the same plot extent and axes limits
      } else {
        # Framed panels each print their own axes; frameless ones only print the
        # "outside" ones, else inner axes collide with the neighbouring panel.
        # Note xside/yside may be swapped (flipped boxplots), so gate on the
        # actual side rather than assuming 1/2.
        .fwa = list(ifacet = ifacet, nfacet_cols = nfacet_cols)
        keep_axis = function(side) {
          draw_facet_axis(
            side, ii, .fwa,
            framed = isTRUE(frame.plot),
            axes = facet.args[["axes"]]
          )
        }
        if (keep_axis(xside)) do.call(tinyAxis, args_x)
        if (.ymgp_shift > 0) par(mgp = par("mgp") - c(0, .ymgp_shift, 0))
        if (keep_axis(yside)) do.call(tinyAxis, args_y)
        if (.ymgp_shift > 0) par(mgp = par("mgp") + c(0, .ymgp_shift, 0))
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
          line_height_lines = (facet_title_lines + .1) * facet_text / cex_fct_adj
          if (isTRUE(facet_rect)) {
            if (ylog) {
              line_height = grconvertY(line_height_lines, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
              rect_height = corners[4] * line_height
            } else {
              line_height = grconvertY(line_height_lines, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
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
            ypos = grconvertY(line_height_lines / 2, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
            ypos = corners[4] * ypos
          } else {
            ypos = grconvertY(line_height_lines / 2, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
            ypos = corners[4] + ypos
          }
          text(
            x = xpos,
            y = ypos,
            labels = sub("^(.*?)~.*", "\\1", facets[[ii]]),
            adj = c(0.5, 0.5),
            cex = facet_text / cex_fct_adj,
            col = facet_col,
            font = facet_font,
            xpd = NA,
          )
        }
        ## right facet strips
        if (ii %% nfacet_cols == 0 || ii == nfacets) {
          line_height_lines = (facet_title_lines + .1) * facet_text / cex_fct_adj
          if (isTRUE(facet_rect)) {
            if (xlog) {
              line_height = grconvertX(line_height_lines, from = "lines", to = "user") / grconvertX(0, from = "lines", to = "user")
              rect_width = corners[2] * line_height
            } else {
              line_height = grconvertX(line_height_lines, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user")
              rect_width = corners[2] + line_height
            }
            rect(
              corners[2], corners[3], rect_width, corners[4],
              col = facet_bg, border = facet_border,
              xpd = NA
            )
          }
          if (xlog) {
            xpos = grconvertX(line_height_lines / 2, from = "lines", to = "user") / grconvertX(0, from = "lines", to = "user")
            xpos = corners[2] * xpos
          } else {
            xpos = grconvertX(line_height_lines / 2, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user")
            xpos = corners[2] + xpos
          }
          ypos = if (ylog) 10^(mean(log10(corners[3:4]))) else mean(corners[3:4])
          text(
            x = xpos,
            y = ypos,
            labels = sub("^.*?~(.*)", "\\1", facets[[ii]]),
            srt = 270,
            adj = c(0.5, 0.5),
            cex = facet_text / cex_fct_adj,
            col = facet_col,
            font = facet_font,
            xpd = NA
          )
        }
      } else {
        line_height_lines = (facet_title_lines + .1) * facet_text / cex_fct_adj
        if (isTRUE(facet_rect)) {
          if (ylog) {
            line_height = grconvertY(line_height_lines, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
            rect_height = corners[4] * line_height
          } else {
            line_height = grconvertY(line_height_lines, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
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
          ypos = grconvertY(line_height_lines / 2, from = "lines", to = "user") / grconvertY(0, from = "lines", to = "user")
          ypos = corners[4] * ypos
        } else {
          ypos = grconvertY(line_height_lines / 2, from = "lines", to = "user") - grconvertY(0, from = "lines", to = "user")
          ypos = corners[4] + ypos
        }
        text(
          x = xpos,
          y = ypos,
          labels = paste(facets[[ii]]),
          adj = c(0.5, 0.5),
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
    if (!is.null(grid) && !isFALSE(grid)) {
      gcol = get_tpar("grid.col", tpar_list = tpars)
      glty = get_tpar("grid.lty", tpar_list = tpars)
      glwd = get_tpar("grid.lwd", tpar_list = tpars)

      if (isTRUE(grid)) {
        draw_x = draw_y = TRUE
        fine_x = fine_y = FALSE
      } else if (is.character(grid)) {
        draw_x = grepl("x", grid, fixed = TRUE) || grepl("X", grid, fixed = TRUE)
        draw_y = grepl("y", grid, fixed = TRUE) || grepl("Y", grid, fixed = TRUE)
        fine_x = grepl("x", grid, fixed = TRUE)
        fine_y = grepl("y", grid, fixed = TRUE)
      } else {
        eval(grid) # issue #193
        draw_x = draw_y = FALSE
      }

      if (draw_x || draw_y) {

        if (draw_x) {
          if (!is.null(xaxb)) {
            xg = xaxb
          } else {
            xg = if (!inherits(x, c("POSIXt", "Date"))) axTicks(side = 1) else axTicksDateTime(side = 1, x = x)
          }
          if (fine_x && !par("xlog") && length(xg) >= 2L) {
            xg = as.numeric(xg)
            half = (xg[2L] - xg[1L]) / 2
            xg = seq(xg[1L] - half, xg[length(xg)] + half, by = half)
          }
          abline(v = xg, col = gcol, lty = glty, lwd = glwd)
        }

        if (draw_y) {
          if (!is.null(yaxb)) {
            yg = yaxb
          } else {
            yg = if (!inherits(y, c("POSIXt", "Date"))) axTicks(side = 2) else axTicksDateTime(side = 2, x = x)
          }
          if (fine_y && !par("ylog") && length(yg) >= 2L) {
            yg = as.numeric(yg)
            half = (yg[2L] - yg[1L]) / 2
            yg = seq(yg[1L] - half, yg[length(yg)] + half, by = half)
          }
          abline(h = yg, col = gcol, lty = glty, lwd = glwd)
        }

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


## Categorical y-axis tick labels, for margin measurement.
##
## Used by the whtsbp label-width blocks in tinyplot.default() and
## draw_facet_window(), which each measure strwidth() on the result but otherwise
## differ in how they apply it.
##
## Returns a one-element list wrapping the label set when a type puts categories
## on the y axis, or NULL when it does not and the caller should fall back to its
## own axisTicks() computation. The wrapper matters: `levels(y)` is itself NULL
## for a ridge plot over a *numeric* y, and that empty result must stay
## distinguishable from "this isn't a categorical axis" -- otherwise the caller
## would substitute numeric ticks and bump the margin that the label-less axis
## does not need.
##
## `ylabs` covers the general case of a type that has placed named categories on
## the y axis. The ridge and flipped-boxplot cases are special: ridge takes its
## categories from the y factor's levels, while a flipped boxplot has had its
## categories swapped onto `xlabs` by flip_datapoints().
y_axis_labels = function(type, y, ylabs, xlabs, flip) {
  if (identical(type, "ridge")) {
    return(list(levels(y)))
  }
  if (!is.null(ylabs)) {
    return(list(if (!is.null(names(ylabs))) names(ylabs) else ylabs))
  }
  if (identical(type, "boxplot") && isTRUE(flip) && !is.null(xlabs)) {
    return(list(if (!is.null(names(xlabs))) names(xlabs) else xlabs))
  }
  NULL
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


## Should facet panel `ifacet` draw its own axis on `side`?
##
## Framed panels each get their own axis, since the frame visually contains it.
## Frameless panels only draw on the outer edge, else the inner axes float into
## the neighbouring panel and collide with its labels. Free facets always draw,
## because each panel has its own scale and an outer axis would misreport it.
##
## `axes` is the (optional) user override from `facet.args$axes`, and takes
## precedence over the implicit `framed` rule: "all" keeps a per-panel axis,
## "outer" restricts to the edge, "none" suppresses entirely.
##
## This is the single decision point for every axis-drawing site, including the
## self-drawing types (see draw_spineplot(), draw_ridge()).
draw_facet_axis = function(
    side,
    ifacet,
    facet_window_args,
    framed = TRUE,
    free = FALSE,
    axes = NULL
    ) {
  # an explicit "none" wins over everything, including free scales
  if (identical(axes, "none")) return(FALSE)
  if (identical(axes, "all")) return(TRUE)
  # without layout info there is nothing to suppress (e.g. unfaceted plots)
  if (is.null(facet_window_args)) return(TRUE)
  outer_only = identical(axes, "outer")
  if (!outer_only) {
    if (isTRUE(free) || isTRUE(framed)) return(TRUE)
  }
  # sides 1/3 sit on the x axis (bottom/top), sides 2/4 on the y (left/right)
  position = switch(as.character(side),
    "1" = "bottom",
    "2" = "left",
    "3" = "top",
    "4" = "right",
    NULL
  )
  if (is.null(position)) return(TRUE)
  isTRUE(is_facet_position(position, ifacet, facet_window_args))
}
