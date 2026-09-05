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
    facet_blank = FALSE,
    # axes args
    axes, flip, frame.plot, oxaxis, oyaxis,
    xlabs, xlim, null_xlim, xaxt, xaxs, xaxb, xaxl,
    ylabs, ylim, null_ylim, yaxt, yaxs, yaxb, yaxl,
    rev_x = FALSE, rev_y = FALSE,
    xlim_partial = NULL, ylim_partial = NULL,
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
  

  # if breaks are provided use these (but only if x/ylabs are null)
  if (!is.null(xaxb) && !is.null(xlabs)) xlabs = xaxb
  if (!is.null(yaxb) && !is.null(ylabs)) ylabs = yaxb

  # Split once up front and index per facet below; splitting inside the facet
  # loop is O(n) per facet per axis. The concatenated vectors also give us the
  # all-facet fallback range for empty panels (see facet_free_range()).
  xall = yall = xfree_split = yfree_split = NULL
  if (isTRUE(facet.args[["free"]])) {
    xcat = c(x, xmin, xmax)
    ycat = c(y, ymin, ymax)
    xall = facet_free_range(xcat)
    yall = facet_free_range(ycat)
    if (!is.null(facet)) {
      xfree_split = split(xcat, facet)
      yfree_split = split(ycat, facet)
    }
  }

  # Per-panel category positions, when the panels were re-levelled
  # (facet.args$drop.levels). Like `.fusr` further below, these travel via
  # .tinyplot_env rather than as another argument; see facet_relevel().
  facet_labs = if (facet_drop_levels_on(facet.args)) {
    get_environment_variable(".facet_labs")
  }

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

  # Are only the outer (edge) facet axes drawn? Computed once, up here, because
  # both the margin logic below and the per-panel frame drawing further down need
  # it (and the latter also runs for unfaceted plots). Two variants:
  #  - .outer_axes:     keyed off the structural `frame.plot`; used for the
  #                     box-to-box facet gap.
  #  - .outer_axes_eff: keyed off the `framed` hint where a type sets one (e.g.
  #                     data_spineplot() forces frame.plot = FALSE internally but
  #                     still draws per-panel axes); used for the tick-label width.
  # A per-call `facet.args$axes` wins over the global `tpar("facet.axes")`, which
  # in turn wins over the implicit frame-based rule (i.e. NULL, the default).
  # Resolve it back into `facet.args` so that every downstream consumer -- incl.
  # the self-drawing types, which read it off `facet_window_args` -- sees the
  # same value without each having to redo the lookup.
  facet.args[["axes"]] = facet.args[["axes"]] %||% get_tpar("facet.axes", tpar_list = tpars)
  .axes = facet.args[["axes"]]
  .eff_frame = if (!is.null(type_hints[["framed"]])) type_hints[["framed"]] else frame.plot
  .outer_axes = outer_axes_only(frame.plot, facet.args[["free"]], .axes)
  .outer_axes_eff = outer_axes_only(.eff_frame, facet.args[["free"]], .axes)

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
    # Extra reduction to close up the whitespace that an interior facet axis
    # would otherwise occupy; see outer_axes_only(). This gap is the
    # box-to-box spacing, so it keys off the *structural* `frame.plot` (a type
    # that suppresses its own box wants the tighter gap), not `.eff_frame`.
    if (.outer_axes) {
      fmar = fmar - 0.5
    }

    ooma = par("oma")

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
      # Tick labels are measured at their own side's cex, falling back to the
      # shared par("cex.axis"). Measuring both sides at the shared value clips
      # the wider axis and reserves dead space on the narrower one.
      .cex_xaxs = get_tpar("cex.xaxs", tpar_list = tpars, default = par("cex.axis"))
      .cex_yaxs = get_tpar("cex.yaxs", tpar_list = tpars, default = par("cex.axis"))
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
          if (isTRUE(facet.args[["free"]]) && (null_ylim || !is.null(ylim_partial)) && !is.null(facet)) {
            # Free scales: measure every facet's ticks and keep the widest set.
            yaxlabs_all = lapply(yfree_split, function(yf) {
              usr = extendrange(facet_free_lim(yf, yall, ylim_partial, "ylim"), f = 0.04)
              axisTicks(usr = usr, log = par("ylog"))
            })
            widths = vapply(yaxlabs_all, function(labs) max(strwidth(labs, "inches", cex = .cex_yaxs)), numeric(1L))
            yaxlabs = yaxlabs_all[[which.max(widths)]]
          } else {
            yaxlabs = axisTicks(usr = extendrange(ylim, f = 0.04), log = par("ylog"))
          }
        }
        if (!is.null(yaxl)) yaxlabs = tinylabel(yaxlabs, yaxl)
        # whtsbp = grconvertX(max(strwidth(yaxl, "figure")), from = "nfc", to = "lines") - 1
        whtsbp = grconvertX(max(strwidth(yaxlabs, "figure", cex = .cex_yaxs)), from = "nfc", to = "lines") - grconvertX(0, from = "nfc", to = "lines") - 0.5
        if (whtsbp > 0) {
          omar = omar + c(0, whtsbp, 0, 0) * cex_fct_adj
          fmar[2] = fmar[2] + whtsbp * cex_fct_adj
        }
        # The label width above is reserved once, and the nmar/noma split below
        # hands it to the *outer* margin -- correct when only the leftmost facet
        # draws a y axis. But when interior facets draw their own (e.g. framed
        # panels), each needs that width in its own margin instead, else the
        # labels overflow into the neighbouring panel. So only release it back to
        # the outer margin when interior axes aren't drawn at all; same rule
        # (and same reason) as the inter-facet gap above.
        if (.outer_axes_eff) {
          fmar[2] = fmar[2] - (whtsbp * cex_fct_adj)
        }
      }
      if (par("las") %in% 2:3) {
        # extra whitespace bump on the x axis
        if (is.null(xlabs) && isTRUE(facet.args[["free"]]) && (null_xlim || !is.null(xlim_partial)) && !is.null(facet)) {
          xaxlabs_all = lapply(xfree_split, function(xf) {
            usr = extendrange(facet_free_lim(xf, xall, xlim_partial, "xlim"), f = 0.04)
            axisTicks(usr = usr, log = par("xlog"))
          })
          widths = vapply(xaxlabs_all, function(labs) max(strwidth(labs, "inches", cex = .cex_xaxs)), numeric(1L))
          xaxlabs = xaxlabs_all[[which.max(widths)]]
        } else {
          xaxlabs = if (is.null(xlabs)) axisTicks(usr = extendrange(xlim, f = 0.04), log = par("xlog")) else
            if (!is.null(names(xlabs))) names(xlabs) else xlabs
        }
        if (!is.null(xaxl)) xaxlabs = tinylabel(xaxlabs, xaxl)
        whtsbp = grconvertX(max(strwidth(xaxlabs, "figure", cex = .cex_xaxs)), from = "nfc", to = "lines") - 0.5
        if (whtsbp > 0) {
          omar = omar + c(whtsbp, 0, 0, 0) * cex_fct_adj
          fmar[1] = fmar[1] + whtsbp * cex_fct_adj
        }
        # As per the y axis above: keep the label width in fmar when interior
        # facets draw their own x axis, else release it to the outer margin.
        if (.outer_axes_eff) {
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

    # Facet grids draw their row titles rotated into the RHS margin, so
    # multi-line titles need extra width out there -- the counterpart to the
    # fmar[3] bump that the top strips get above. Without it the rotated title
    # (and its background rect) overflows the figure region. Regular facet
    # titles all sit on top, so only grids need this.
    if (isTRUE(attr(facet, "facet_grid")) && facet_newlines > 0) {
      omar[4] = omar[4] + facet_newlines * facet_text
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
    # Per-side tick-label cex; see the faceted branch above.
    .cex_xaxs = get_tpar("cex.xaxs", tpar_list = tpars, default = par("cex.axis"))
    .cex_yaxs = get_tpar("cex.yaxs", tpar_list = tpars, default = par("cex.axis"))
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
      # whtsbp = grconvertX(max(strwidth(yaxlabs, "figure", cex = .cex_yaxs)), from = "nfc", to = "lines") - 1
      whtsbp = grconvertX(max(strwidth(yaxlabs, "figure", cex = .cex_yaxs)), from = "nfc", to = "lines") - grconvertX(0, from = "nfc", to = "lines") - 0.5
      omar[2] = omar[2] + whtsbp
    }
    if (par("las") %in% 2:3) {
      # extra whitespace bump on the x axis
      # xaxl = axTicks(1)
      xlim_usr = if (diff(xlim) == 0 && is.null(xaxb)) xlim + c(-0.5, 0.5) else extendrange(xlim, f = 0.04)
      xaxlabs = if (is.null(xlabs)) axisTicks(usr = xlim_usr, log = par("xlog")) else
        if (!is.null(names(xlabs))) names(xlabs) else xlabs
      if (!is.null(xaxl)) xaxlabs = tinylabel(xaxlabs, xaxl)
      whtsbp = grconvertX(max(strwidth(xaxlabs, "figure", cex = .cex_xaxs)), from = "nfc", to = "lines") - 0.5
      omar[1] = omar[1] + whtsbp
    }

     par(mar = omar)
  }

  ## Loop over the individual facet windows and draw the plot region
  ## components (axes, titles, box, grid, etc.)
  for (ii in ifacet) {
    # A grid cell that no observation uses keeps its slot, so the surrounding
    # panels stay aligned, but drops its frame and grid lines to read as a gap
    # rather than an empty box.
    #
    # Whether it keeps an axis depends on who owns the axes. Where interior axes
    # are dropped (`.outer_axes`, e.g. tinytheme("float")), the edge axis serves
    # the whole column or row, so the blank cell has to keep drawing it -- moving
    # it to an inner panel would break the alignment of the bottom row. Where
    # every panel draws its own instead (a framed theme, or free scales), no axis
    # is load-bearing beyond its own panel, so a lone rule in a gap is just
    # debris and the cell draws nothing at all.
    .blank = length(facet_blank) >= ii && isTRUE(facet_blank[[ii]])
    .fwa = list(ifacet = ifacet, nfacet_cols = nfacet_cols)
    blank_axis = function(side) {
      if (!.outer_axes || identical(.axes, "none")) return(FALSE)
      pos = switch(as.character(side),
        "1" = "bottom", "2" = "left", "3" = "top", "4" = "right", NULL
      )
      !is.null(pos) && isTRUE(is_facet_position(pos, ii, .fwa))
    }

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
      # Note `cex.axis` rather than `cex`: base `axis()` sizes its tick labels
      # from the former and silently ignores the latter, so passing `cex` here
      # would leave `cex.xaxs`/`cex.yaxs` (and any theme that sets them) with no
      # effect on label size.
      args_x = list(x,
        side = xside,
        type = xaxt,
        labeller = xaxl,
        cex.axis = get_tpar(c("cex.xaxs", "cex.axis"), 0.8, tpar_list = tpars),
        lwd = get_tpar(c("lwd.xaxs", "lwd.axis"), 1, tpar_list = tpars),
        lty = get_tpar(c("lty.xaxs", "lty.axis"), 1, tpar_list = tpars)
      )
      .ca = get_tpar(c("cex.yaxs", "cex.axis"), 0.8, tpar_list = tpars)
      .ymgp_shift = if (par("las") %in% c(0L, 1L)) 0.5 * (.ca - 1) else 0
      args_y = list(y,
        side = yside,
        type = yaxt,
        labeller = yaxl,
        cex.axis = .ca,
        lwd = get_tpar(c("lwd.yaxs", "lwd.axis"), 1, tpar_list = tpars),
        lty = get_tpar(c("lty.yaxs", "lty.axis"), 1, tpar_list = tpars)
      )
      if (!is.null(xaxb)) args_x$at = xaxb
      if (!is.null(yaxb)) args_y$at = yaxb
      # `xlabs`/`ylabs` are only non-NULL when a type has placed categorical data
      # on that axis, so their presence is the signal to draw labelled ticks.
      # The y-side previously listed the eligible types by name, but every type
      # that populates `ylabs` does so precisely because it has categories to
      # label, making the extra condition redundant (#665).
      type_range_x = !is.null(xlabs)
      type_range_y = !is.null(ylabs)
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
        xfree = if (!is.null(facet)) xfree_split[[ii]] else xcat
        yfree = if (!is.null(facet)) yfree_split[[ii]] else ycat
        # A re-levelled panel (facet.args$drop.levels) has its own category
        # positions and labels; otherwise every panel shares the global set. Like
        # `.fusr` below, the maps travel via .tinyplot_env rather than as another
        # pair of arguments; see facet_relevel().
        .fxlabs = facet_labs[["x"]][[ii]] %||% xlabs
        .fylabs = facet_labs[["y"]][[ii]] %||% ylabs
        # `.pad`: room for the geometry drawn around an end category, as
        # lim_args() adds to the fixed limits. A categorical axis is a set rather
        # than a range, so the panel also keeps all of *its* ticks (every
        # category, or just the ones it uses under `drop.levels`).
        .pad = if (identical(type, "boxplot")) c(-0.5, 0.5) else 0
        # Keeping every category (the default) spans the *global* extent, so that
        # the panels are identical and their ticks line up; under `drop.levels`
        # each panel spans only the categories it uses.
        .xall_cat = length(.fxlabs) > 0 && is.null(facet_labs[["x"]])
        .yall_cat = length(.fylabs) > 0 && is.null(facet_labs[["y"]])
        if (null_xlim || !is.null(xlim_partial)) {
          xlim = facet_free_lim(
            if (.xall_cat) xcat else xfree, xall, xlim_partial, "xlim"
          ) + .pad
          if (length(.fxlabs)) xlim = range(c(xlim, .fxlabs))
        }
        if (null_ylim || !is.null(ylim_partial)) {
          ylim = facet_free_lim(
            if (.yall_cat) ycat else yfree, yall, ylim_partial, "ylim"
          )
          if (length(.fylabs)) ylim = range(c(ylim, .fylabs))
        }
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
        .free_axes = !identical(.axes, "none")
        .free_x = .free_axes && (!.blank || blank_axis(xside))
        .free_y = .free_axes && (!.blank || blank_axis(yside))
        # Reuse the args_x/args_y lists built above rather than calling tinyAxis()
        # with a bare handful of arguments, so that free facets pick up the same
        # themed `cex`/`lwd`/`lty` (cex.axis, lwd.axis, lty.axis and their
        # per-side variants) as fixed ones. Only the per-facet bits are
        # overridden: the panel's own data, plus `at`/`labels` where this facet
        # needs explicit ticks.
        if (.free_x) {
          .axf = args_x
          .axf[[1L]] = xfree
          if (!is.null(.fxlabs)) {
            .axf = modifyList(.axf, list(at = .fxlabs, labels = names(.fxlabs)))
          } else if (!is.null(xat)) {
            .axf = modifyList(.axf, list(at = xat))
          } else {
            # a fixed-scale `at` (from xaxb) doesn't apply to this facet's range
            .axf[["at"]] = NULL
          }
          do.call(tinyAxis, .axf)
        }
        if (.ymgp_shift > 0) par(mgp = par("mgp") - c(0, .ymgp_shift, 0))
        if (.free_y) {
          .ayf = args_y
          .ayf[[1L]] = yfree
          # Same signal as the fixed-scale branch above: named `ylabs` means the
          # type put categories on the y-axis. Listing eligible types by name
          # instead not only dropped the labels for unlisted types, it left the
          # `labels` inherited from `args_y` without a matching `at`, which
          # axis() rejects outright. (#679)
          if (!is.null(.fylabs)) {
            .ayf = modifyList(.ayf, list(at = .fylabs, labels = names(.fylabs)))
          } else if (!is.null(yat)) {
            .ayf = modifyList(.ayf, list(at = yat))
          } else {
            .ayf[["at"]] = NULL
          }
          do.call(tinyAxis, .ayf)
        }
        if (.ymgp_shift > 0) par(mgp = par("mgp") + c(0, .ymgp_shift, 0))

        # For fixed facets we can just reuse the same plot extent and axes limits
      } else {
        # Framed panels each print their own axes; frameless ones only print the
        # "outside" ones, else inner axes collide with the neighbouring panel.
        # Note xside/yside may be swapped (flipped boxplots), so gate on the
        # actual side rather than assuming 1/2.
        keep_axis = function(side) {
          if (.blank) return(blank_axis(side))
          draw_facet_axis(
            side, ii, .fwa,
            framed = isTRUE(frame.plot),
            axes = .axes
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
          # a labeller can return plotmath (e.g. tinylabel's "log"), in which
          # case the element has to reach text() as a language object rather
          # than being flattened by paste(); see facet_titles()
          labels = if (is.expression(facets)) facets[[ii]] else paste(facets[[ii]]),
          adj = c(0.5, 0.5),
          cex = facet_text / cex_fct_adj,
          col = facet_col,
          font = facet_font,
          xpd = NA
        )
      }
    }

    # plot frame. For a directional `bty` (e.g. the L of tinytheme("classic")),
    # drop any edge that faces a neighbouring panel rather than the grid's outer
    # boundary, else it floats in the gutter; see draw_facet_box().
    #
    # Only when the interior axes are dropped too, though. A per-panel box also
    # extends that panel's axis rules to the full panel width, so removing it
    # while the axes remain would leave short, inset rules behind. Tie the two
    # together: same condition, so the frame and the axes agree.
    #
    # Fast path: a stray interior edge needs more than one facet, a *directional*
    # bty, and suppressed interior axes. A single panel has no interior edge at
    # all; nor does a full box ("o") or no box ("n"); nor does a plot that still
    # draws its interior axes. All of those defer straight to box(), which is
    # cheaper, exact, and (unlike per-side segments) draws one joined polyline.
    if (frame.plot && !.blank) {
      if (nfacets > 1 && .outer_axes && !(par("bty") %in% c("o", "O", "n", "N"))) {
        draw_facet_box(par("bty"), ii, list(ifacet = ifacet, nfacet_cols = nfacet_cols))
      } else {
        box()
      }
    }

    # panel grid lines
    if (is.null(grid)) grid = get_tpar("grid", tpar_list = tpars)
    if (!is.null(grid) && !isFALSE(grid) && !.blank) {
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
    if (!is.null(draw) && !.blank) eval(draw)
  } # end of ii facet loop

  return(as.list(environment()))
}


## Should each free panel re-level its categorical axes, i.e. keep only the
## categories it actually uses? See facet_relevel().
facet_drop_levels_on = function(facet.args) {
  isTRUE(facet.args[["drop.levels"]] %||% .tpar[["facet.drop.levels"]])
}


## Which category (a position in the global level set) does each row sit at? An
## offset can move a row off its own tick, so prefer the `.xcat`/`.ycat` codes
## stashed by the types that displace one (dodge_positions(), type_jitter(),
## type_violin()) and fall back to the positions only where they are exact
## integers. NULL means "can't tell", and the caller leaves that axis alone.
cat_axis_codes = function(datapoints, ax = "x") {
  v = datapoints[[paste0(".", ax, "cat")]]
  if (is.null(v)) v = datapoints[[ax]]
  if (is.null(v)) return(NULL)
  if (is.factor(v)) return(as.integer(v))
  if (!is.numeric(v)) return(NULL)
  # missing values carry no category, and are not drawn anyway; ignore them here
  vv = v[!is.na(v)]
  if (any(!is.finite(vv)) || any(vv != trunc(vv))) return(NULL)
  as.integer(v)
}


#' @rdname facet
#' @keywords internal
#' @param settings A list of settings as created by `tinyplot()`.
#' @details `facet_relevel` implements `facet.args$drop.levels`: each free facet
#'   keeps only the categories it actually uses, re-levelled as if the panel's
#'   data had been passed through `factor()` on its own. Positions are shifted
#'   rather than recomputed, so a row's offset within its category (dodge,
#'   jitter, boxplot group offsets) and any rectangle width around it survive
#'   untouched.
facet_relevel = function(settings) {
  if (!facet_drop_levels_on(settings[["facet.args"]])) return(invisible())

  datapoints = settings[["datapoints"]]
  facet = datapoints[["facet"]]
  if (is.null(facet) || length(unique(facet)) < 2L || nrow(datapoints) == 0L) {
    return(invisible())
  }
  # Fixed panels share one axis, so per-panel level sets would misalign them.
  if (!isTRUE(settings[["facet.args"]][["free"]])) {
    warning(
      "`facet.args$drop.levels` re-levels each panel's categorical axis ",
      "independently, which requires free scales. Ignoring it, since ",
      "`facet.args$free` is not TRUE.",
      call. = FALSE
    )
    return(invisible())
  }
  facet = as.factor(facet)
  fl = levels(facet)

  # An added layer inherits the base layer's panel maps rather than deriving its
  # own: it has to land on the categories the base layer actually drew, and its
  # own rows may not cover the same ones.
  add = isTRUE(settings[["add"]])

  applied = FALSE
  facet_labs = list()
  for (ax in c("x", "y")) {
    # named `xlabs`/`ylabs` is the signal that a type put categories on this axis
    labs = settings[[paste0(ax, "labs")]]
    if (is.null(labs) || is.null(names(labs))) next
    codes = cat_axis_codes(datapoints, ax)
    if (is.null(codes)) next
    stored = if (add) get_environment_variable(".facet_labs")[[ax]] else NULL
    if (add && is.null(stored)) next

    # each row's category, by name, which is what both paths key off
    row_names = names(labs)[match(codes, unname(labs))]
    delta = numeric(nrow(datapoints))
    labs_by_facet = vector("list", length(fl))
    names(labs_by_facet) = fl
    for (f in fl) {
      idx = which(facet == f)
      if (!length(idx)) next
      if (!is.null(stored)) {
        map = stored[[f]]
      } else {
        # rank within the panel's own levels, i.e. what factor() would have given
        present = sort(unique(codes[idx]))
        nm = names(labs)[match(present, unname(labs))]
        ok = !is.na(nm)
        map = stats::setNames(seq_along(present)[ok], nm[ok])
      }
      if (is.null(map) || !length(map)) next
      # a category the panel does not hold maps to NA, i.e. is simply not drawn
      delta[idx] = unname(map[row_names[idx]]) - codes[idx]
      labs_by_facet[[f]] = map
    }

    for (col in paste0(ax, c("", "min", "max"))) {
      v = datapoints[[col]]
      if (is.null(v)) next
      # a categorical axis is left as a factor by some types (barplot), which
      # then draw from xmin/xmax; the free ranges read it either way
      if (is.factor(v)) datapoints[[col]] = as.integer(v) + delta
      else if (is.numeric(v)) datapoints[[col]] = v + delta
      sv = settings[[col]]
      if (!is.null(sv) && is.numeric(sv) && length(sv) == nrow(datapoints)) {
        settings[[col]] = sv + delta
      }
    }
    facet_labs[[ax]] = labs_by_facet
    applied = TRUE
  }

  # Say so rather than quietly doing nothing: either no axis holds categories, or
  # the type places them itself and so is outside this machinery (type_ridge()).
  if (!applied) {
    warning(
      "`facet.args$drop.levels` had no effect: this plot has no categorical ",
      "axis that tinyplot positions itself",
      if (isTRUE(settings[["type_hints"]][["draws_own_axes"]])) {
        sprintf(" (the \"%s\" type draws its own axis labels)", settings[["type"]])
      } else {
        ""
      },
      ".",
      call. = FALSE
    )
  }

  # Where draw_facet_window() reads them from, and where any layer added on top
  # inherits them; cf. `.fusr` and `xlabs_orig` in align_layer().
  if (applied) set_environment_variable(.facet_labs = facet_labs)

  settings[["datapoints"]] = datapoints
  invisible()
}


## droplevels() for a facet factor. Plain droplevels() strips the attributes a
## facet carries (facet_grid, facet_nrow), so carry them across by hand.
drop_facet_levels = function(f) {
  if (!is.factor(f)) return(f)
  a = attributes(f)
  f = droplevels(f)
  a[["levels"]] = levels(f)
  attributes(f) = a
  f
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

  # Opt-in dropping of facet levels that no observation uses, which otherwise
  # draw an empty panel. A wrapped layout drops the level outright, so the panel
  # goes away. A grid can't: it is a rectangle of rows x columns, so removing a
  # cell would misalign the panels that remain -- instead it keeps the slot and
  # leaves it blank (see `facet_blank` below), reading as a gap.
  .drop = !is.null(facet) &&
    isTRUE(facet.args[["drop"]] %||% .tpar[["facet.drop"]])
  .grid = isTRUE(attr(facet, "facet_grid"))
  if (.drop && !.grid) {
    facet = drop_facet_levels(facet)
    datapoints$facet = drop_facet_levels(datapoints$facet)
  }

  # Which grid cells hold no data, indexed like `ifacet`.
  facet_blank = FALSE
  if (.drop && .grid) {
    facet_blank = tabulate(facet, nlevels(facet)) == 0L
  }

  nfacet_rows = 1
  nfacet_cols = 1
  if (!is.null(facet)) {
    facets = if (is.factor(facet)) levels(facet) else sort(unique(facet))
    # optional labelling and/or "varname = value" prefixing; see facet_titles()
    facets = facet_titles(
      facets,
      labeller = facet.args[["labeller"]] %||% .tpar[["facet.labeller"]],
      prefix = facet.args[["prefix"]] %||% .tpar[["facet.prefix"]],
      facet_vars = settings$facet_vars,
      facet_grid = isTRUE(attr(facet, "facet_grid")),
      sep = facet.args[["sep"]] %||% .tpar[["facet.sep"]]
    )
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
    c("datapoints", "facets", "ifacet", "nfacets", "nfacet_rows", "nfacet_cols", "oxaxis", "oyaxis", "cex_fct_adj", "facet_blank")
  )
}



#
# helper functions
#


## Build the facet strip titles: optionally run the facet values through a
## `tinylabel()` labeller, then optionally prefix them with their variable
## name(s), e.g. "vs = 0" rather than a bare "0". See the `labeller`, `prefix`
## and `sep` entries of `facet.args` in ?tinyplot.
##
## `labels` are the raw facet titles as computed in facet_layout(), i.e. the
## levels (or sorted unique values) of the facet variable. `facet_vars` is the
## list that sanitize_facet() resolves for the plot: an "x" element (the
## variables behind the regular / top strip labels) and, for facet grids, a "y"
## element (those behind the right strip labels).
##
## Composite titles are taken apart before either step is applied, so that a
## labeller sees the individual facet *values* rather than the glued-together
## string: grid titles split on "~" (matching the sub() patterns that
## draw_facet_window() uses to split them again at draw time), and each side
## then splits on the ":" that interaction() used for multi-variable facets.
##
## `labeller` and `prefix` both accept either one value for every facet
## variable, or one per variable -- positionally, in the order the variables
## appear in the `facet` specification, or named for the variables they apply
## to. See match_facet_vars().
##
## `sep` separates the variables of a multi-variable title, whether or not they
## are prefixed, e.g. "\n" to stack them on separate lines. It defaults to the
## ":" that interaction() glued them with upstream, or to ", " once they carry
## their own names, which is easier to read (compare "vs = 0, am = 1" against
## "vs = 0:am = 1"). The name and its value are always joined by " = ".
##
## A labeller that returns plotmath (tinylabel's "log") survives as an
## expression for a single unprefixed facet variable, which draw_facet_window()
## hands to text() as-is; every other route glues strings together and so
## deparses it.
facet_titles = function(
    labels,
    labeller = NULL,
    prefix = NULL,
    facet_vars = NULL,
    facet_grid = FALSE,
    sep = NULL) {
  has_prefix = !(is.null(prefix) || isFALSE(prefix))
  if (is.null(labeller) && !has_prefix && is.null(sep)) return(labels)

  ## separator between the variables of a multi-variable title (see above)
  if (is.null(sep)) sep = if (has_prefix) ", " else ":"
  if (isTRUE(facet_grid) && grepl("~", sep, fixed = TRUE)) {
    stop(
      "`facet.args$sep` cannot contain a \"~\" for facet grids, since that is ",
      "the separator between the top and right strip titles.",
      call. = FALSE
    )
  }

  ## `facet_vars` holds one entry per side: each variable's levels, keyed by the
  ## variable's name. Flattened here into *formula* order, i.e. the order the
  ## user wrote them -- for a grid the LHS (drawn as the right-hand strips)
  ## comes first, since get_facet_fml() swaps the sides internally to plot
  ## rowwise. Splitting a per-variable input back out takes the y side off the
  ## front to match.
  nx = length(facet_vars[["x"]])
  ny = length(facet_vars[["y"]])
  lvls = c(facet_vars[["y"]], facet_vars[["x"]])
  vars = names(lvls)
  split_sides = function(x) {
    if (is.null(x)) return(list(x = NULL, y = NULL))
    if (length(vars) == 0L) return(list(x = x, y = x))
    list(x = x[ny + seq_len(nx)], y = x[seq_len(ny)])
  }

  xnms = names(facet_vars[["x"]])
  ynms = names(facet_vars[["y"]])
  if (has_prefix) {
    ## Also guards the internal entry point: anything that is neither a flag nor
    ## name(s) used to fall through the branches below and be silently ignored,
    ## leaving the public assert load-bearing for correctness rather than just
    ## for error quality.
    assert_facet_prefix(prefix, name = "facet.args$prefix")
    if (is.character(prefix) || is.list(prefix)) {
      pnms = match_facet_vars(prefix, vars, "facet.args$prefix")
      ## a named input can name only some of the variables; the rest fall back
      ## to their own (deparsed) name, exactly as `prefix = TRUE` would
      if (length(vars) > 0L) {
        pnms = lapply(seq_along(pnms), function(i) pnms[[i]] %||% vars[[i]])
      }
      pnms = as.character(unlist(pnms))
      if (length(vars) == 0L) {
        ## no known variable names (e.g. a facet variable that upstream methods
        ## construct themselves): the string simply becomes the single prefix
        xnms = pnms
        ynms = NULL
        nx = 1L
      } else {
        sides = split_sides(pnms)
        xnms = sides[["x"]]
        ynms = sides[["y"]]
      }
    } else if (nx + ny == 0L) {
      ## prefix = TRUE, but we couldn't determine any variable names
      has_prefix = FALSE
    }
  }

  ## per-variable labellers, split across the two sides the same way
  labellers = if (is.null(labeller)) {
    NULL
  } else {
    match_facet_vars(labeller, vars, "facet.args$labeller")
  }
  sides = split_sides(labellers)
  lvl_sides = split_sides(lvls)

  if (isTRUE(facet_grid)) {
    labels = as.character(labels)
    xlabs = facet_titles_side(
      sub("^(.*?)~.*", "\\1", labels), xnms, sides[["x"]], has_prefix, sep,
      lvl_sides[["x"]]
    )
    ylabs = facet_titles_side(
      sub("^.*?~(.*)", "\\1", labels), ynms, sides[["y"]], has_prefix, sep,
      lvl_sides[["y"]]
    )
    paste0(xlabs, "~", ylabs)
  } else {
    facet_titles_side(labels, xnms, sides[["x"]], has_prefix, sep, lvl_sides[["x"]])
  }
}


## Map the components of a composite facet title back onto the values they came
## from. Splitting a title necessarily works on strings, since interaction()
## flattened the variables into level labels upstream -- which would otherwise
## hand a labeller "0" where an unsplit, single-variable facet hands it 0, so
## that e.g. `labeller = as.logical` yields NA on the one and FALSE on the other
## (#295). Restoring from the variable's own values keeps the two consistent.
##
## Only character input is restored (an unsplit facet still holds its original
## values), and only when every component maps cleanly, so a level that defies
## the round trip is left as the string it already was.
restore_facet_values = function(x, lvls) {
  if (is.null(lvls) || !is.character(x)) return(x)
  idx = match(x, as.character(lvls))
  if (anyNA(idx)) return(x)
  return(lvls[idx])
}


## A facet variable's distinct values, in the order that interaction() lays its
## levels out. Kept alongside the variable names so that facet_titles_side() can
## map a component of a composite title back to the value it came from; see
## restore_facet_values() there.
facet_var_levels = function(v) {
  if (is.factor(v)) levels(v) else sort(unique(v))
}


## The same, as the single-variable list that `facet_vars` expects.
facet_var_list = function(v, name) {
  out = list(facet_var_levels(v))
  names(out) = name
  return(out)
}


## Resolve a per-variable `facet.args` input (`prefix` strings, `labeller`
## functions) into a list with one element per facet variable.
##
## Values can be supplied positionally, in the order the variables appear in the
## `facet` specification -- for a grid that is the formula LHS first, then the
## RHS, i.e. the order the user wrote them rather than the order tinyplot
## happens to store them in. A single value is recycled across every variable.
##
## Alternatively they can be *named* for the variables they apply to, in which
## case order is irrelevant and naming only some of the variables is fine: the
## rest come back as NULL, for the caller to fill in with its own default (the
## variable's own name for `prefix`, no labelling for `labeller`).
match_facet_vars = function(x, vars, arg) {
  ## NB: as.list() on a function returns its formals, so single functions have
  ## to be wrapped by hand rather than coerced
  x = if (is.function(x)) list(x) else if (is.list(x)) x else as.list(x)
  nms = names(x)

  if (!is.null(nms) && any(nzchar(nms))) {
    if (!all(nzchar(nms))) {
      stop("`", arg, "` should be either fully named or fully unnamed.", call. = FALSE)
    }
    if (length(vars) == 0L) {
      stop(
        "`", arg, "` cannot be named here, since the facet variable name(s) ",
        "could not be determined.",
        call. = FALSE
      )
    }
    unknown = setdiff(nms, vars)
    if (length(unknown) > 0L) {
      stop(
        "`", arg, "` was named for unknown facet variable(s): ",
        paste(unknown, collapse = ", "),
        ". Available facet variable(s): ", paste(vars, collapse = ", "), ".",
        call. = FALSE
      )
    }
    out = vector("list", length(vars))
    names(out) = vars
    out[nms] = x
    return(out)
  }

  nvars = max(length(vars), 1L)
  if (length(x) == 1L) x = rep(x, nvars)
  if (length(x) != nvars) {
    stop(
      "`", arg, "` should be a single value, or one per facet variable (",
      nvars, " here). Alternatively, name the values for the facet ",
      "variable(s) they apply to.",
      call. = FALSE
    )
  }
  return(x)
}


## Workhorse for facet_titles(): label and/or prefix one side of a facet title.
##
## The single-variable case (by far the most common) hands the values to
## tinylabel() untouched, i.e. still numeric, Date, etc., so that class-specific
## labellers work. Multi-variable sides have to be split into their components
## first, which means going through character; the labeller is then applied down
## each component in turn -- one call per component, not per label -- since
## labellers like "comma" and date formats derive a consistent format from the
## whole vector. Components are then rejoined with `sep`, which is why the split
## has to happen even when nothing is being labelled or prefixed. Any label that
## doesn't split into as many components as we have names for it (e.g. a level
## that itself contains a ":") is left alone.
##
## `labellers` is the per-variable list from match_facet_vars(), i.e. one element
## per component of this side (or NULL for no labelling at all). `levels` is the
## matching list of each variable's own values, used to undo the stringification
## that splitting a composite title imposes; see restore_facet_values().
facet_titles_side = function(
    labels,
    nms,
    labellers = NULL,
    has_prefix = FALSE,
    sep = ":",
    levels = NULL) {
  n = length(nms)
  at = function(x, j) if (is.null(x) || length(x) < j) NULL else x[[j]]

  if (n <= 1L) {
    labels = restore_facet_values(labels, at(levels, 1L))
    labels = tinylabel(labels, at(labellers, 1L))
    if (isTRUE(has_prefix) && n == 1L) labels = paste0(nms, " = ", labels)
    return(labels)
  }

  parts = strsplit(as.character(labels), ":", fixed = TRUE)
  ok = lengths(parts) == n
  out = as.character(labels)
  if (any(ok)) {
    mat = do.call(rbind, parts[ok])
    cols = lapply(
      seq_len(n),
      function(j) {
        vals = restore_facet_values(mat[, j], at(levels, j))
        as.character(tinylabel(vals, at(labellers, j)))
      }
    )
    if (isTRUE(has_prefix)) {
      cols = Map(function(nm, vals) paste0(nm, " = ", vals), nms, cols)
    }
    out[ok] = do.call(paste, c(cols, list(sep = sep)))
  }
  return(out)
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

  ## each facet variable's levels, keyed by its name; see facet_titles()
  xfacet_vars = lapply(mf[xfacet_loc:NCOL(mf)], facet_var_levels)
  yfacet_vars = if (no_yfacet) NULL else lapply(mf[yfacet_loc], facet_var_levels)

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
  attr(ret, "facet_vars") = list(x = xfacet_vars, y = yfacet_vars)

  return(ret)
}


## Are a facet's interior tick labels visually anchored?
##
## draw_facet_axis() keys the "outer facets only" rule off framing, on the basis
## that an unframed interior axis floats free and collides with the neighbouring
## panel. `frame.plot` is only a proxy for that, though: sanitize_axes() derives
## it as `all(c(xaxt, yaxt) %in% c("s", "a"))`, so `axes = "ticks"` reports
## FALSE despite drawing tick marks that anchor the labels perfectly well. Only
## the "l" (labels) and "n" (none) styles are genuinely bare.
##
## A cleaner long-term fix would drop `frame.plot` from this decision entirely in
## favour of an explicit "would inner axes float?" flag, but that changes
## behaviour more broadly; see SCRATCH/facet-margin-slack.md.
##
## Note that this is deliberately *not* consulted by the generic fixed-facet draw
## site (the `keep_axis()` block in draw_facet_window()), which keys off
## `frame.plot` directly and so stays outer-only under `axes = "t"`. Only the
## margin logic and the self-drawing types (draw_spineplot(), draw_ridge()) route
## through here. That divergence is intentional, and follows from what the axis
## means in each case:
##
##   - Generic facets share one scale, so a single edge axis is correct. Drawing
##     one per panel is both redundant and collision-prone: with no frame the
##     interior tick rows land in the neighbouring panel's data region.
##   - The self-drawing types put *per-panel* categories on their axes, so every
##     panel needs its own to be readable at all.
##
## Consequence: under `axes = "t"` the margin block keeps a per-facet label width
## that the generic draw site never uses. The nmar/noma split absorbs it, so
## there is no visible effect -- but don't assume the two consumers agree here,
## because they don't.
facet_axes_framed = function(frame.plot, xaxt, yaxt) {
  if (any(c(xaxt, yaxt) == "t")) return(TRUE)
  isTRUE(frame.plot)
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


## internal convenience function for the data extent of a single free facet.
## A facet can hold no data at all: a grid draws the full cross-product of its
## two variables, and an unused factor level does the same for one variable.
## range() then returns c(Inf, -Inf), which par(usr=) rejects. Fall back to the
## all-facet range so the empty panel draws like its neighbours, or to c(0, 1)
## if even that is unusable (e.g. every value missing). (#705) The fallback is
## itself a facet_free_range() result, so is already known to be finite.
facet_free_range = function(v, fallback = NULL) {
  # some types (e.g. barplot) leave a categorical axis as a factor
  if (is.factor(v)) v = as.integer(v)
  vf = v[is.finite(v)]
  if (length(vf)) return(range(vf))
  if (!is.null(fallback)) return(fallback)
  c(0, 1)
}


## As above, but re-resolving a partial limit (scalar, or one NA) against the
## facet's own range instead of the global one, which would collapse a free axis
## back to a shared scale.
facet_free_lim = function(v, fallback, partial = NULL, arg = "xlim") {
  rng = facet_free_range(v, fallback)
  if (is.null(partial)) rng else resolve_lim(partial, rng, arg)
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


## Draw a facet panel's plot frame, dropping any edge that faces a neighbour.
##
## For a directional `bty` (i.e. anything but the full box "o"), a per-panel
## frame leaves stray lines floating in the gutter between facets -- e.g.
## tinytheme("classic") draws an L in every panel, so interior panels show a bare
## vertical/horizontal rule. Only the edges that sit on the facet grid's *outer*
## boundary are wanted, matching how ggplot2 renders `theme_classic()` facets.
##
## This can't be delegated back to box(): `bty` has no code for a single edge
## ("o" = all four, "l" = left+bottom, "7" = top+right, "u" = 3 sides, "c"/"]" =
## bracket, "n" = none), yet a 2x2 grid needs left-only and bottom-only panels.
## So decompose `bty` into its constituent sides and draw the survivors with
## segments() along par("usr").
##
## `bty = "o"` keeps calling box() directly: a full box on every panel is the
## conventional faceted look, and it has no interior-facing edge problem.
draw_facet_box = function(bty, ifacet, facet_window_args) {
  sides = switch(
    bty,
    # GBox() case-folds these, so accept both cases for the letter codes.
    "o" = , "O" = c("bottom", "left", "top", "right"),
    "l" = , "L" = c("bottom", "left"),
    "7" = c("top", "right"),
    "u" = , "U" = c("bottom", "left", "right"),
    # "c" opens to the right, so it draws top/left/bottom (same as "["); "]"
    # opens to the left, so bottom/right/top. Verified against base box().
    "c" = , "C" = , "[" = c("bottom", "left", "top"),
    "]" = c("bottom", "top", "right"),
    "n" = , "N" = character(0L),
    # unknown/unsupported code: fall back to base R's own handling
    NULL
  )
  if (is.null(sides)) {
    box(bty = bty)
    return(invisible(NULL))
  }
  if (!length(sides)) return(invisible(NULL))
  # A full box has no interior-facing edge to drop, so keep base R's version
  # (identical output, and it draws the frame as a single polygon).
  if (bty %in% c("o", "O")) {
    box()
    return(invisible(NULL))
  }
  # Drop the edges that abut another panel rather than the grid's outer boundary
  if (!is.null(facet_window_args)) {
    sides = sides[vapply(
      sides,
      function(s) isTRUE(is_facet_position(s, ifacet, facet_window_args)),
      logical(1L)
    )]
  }
  if (!length(sides)) return(invisible(NULL))
  u = par("usr")
  if (par("xlog")) u[1:2] = 10^u[1:2]
  if (par("ylog")) u[3:4] = 10^u[3:4]
  # `xpd = 2` mirrors what C_box() does before calling GBox() ("force clipping to
  # device region"). The frame lies exactly *on* the plot region boundary, so
  # under the default xpd = FALSE it gets clipped to that same boundary, losing
  # half its stroke width and rendering visibly thin.
  for (s in sides) {
    switch(s,
      "bottom" = segments(u[1], u[3], u[2], u[3], xpd = 2),
      "top"    = segments(u[1], u[4], u[2], u[4], xpd = 2),
      "left"   = segments(u[1], u[3], u[1], u[4], xpd = 2),
      "right"  = segments(u[2], u[3], u[2], u[4], xpd = 2)
    )
  }
  invisible(NULL)
}


## Are only the outer (edge) facet axes drawn, i.e. no interior axes?
##
## Drives the inter-facet gap (fmar), which must shrink exactly when no interior
## axis is drawn to fill it. This has to mirror what the pipeline *actually*
## draws, which is not a single rule: free scales draw per-panel via `.free_axes`
## (ignoring "outer"), while fixed scales follow draw_facet_axis(). Hence the
## `none` and `free` short-circuits below come before the "outer"/frame checks.
## (For "none", strictly *no* axes are drawn, but the gap-tightening is the same.)
outer_axes_only = function(frame.plot, free, axes) {
  if (identical(axes, "none")) return(TRUE)   # no axes at all (free or fixed)
  if (isTRUE(free)) return(FALSE)             # free scales draw per-panel
  if (identical(axes, "all")) return(FALSE)   # per-panel axes forced on
  if (identical(axes, "outer")) return(TRUE)  # interior axes off, edges kept
  isFALSE(frame.plot)                         # frameless => interior dropped
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
