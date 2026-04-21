draw_title = function(main, sub, xlab, ylab, legend, legend_args, opar) {
  # main title
  # Note that we include a special catch for the main title if legend is
  # "top!" (and main is specified in the first place).
  legend_eval = tryCatch(eval(legend), error = function(e) NULL)
  # Extra bit of footwork if user passed legend = legend(...) instead of
  # legend = list(...), since the call environment is tricky
  if (is.null(legend_eval)) {
    legend_eval = tryCatch(paste0(legend)[[2]], error = function(e) NULL)
  }

  adj_title = !is.null(legend) && ((is.character(legend) && legend == "top!") || (!is.null(legend_args[["x"]]) && legend_args[["x"]] == "top!") || (is.list(legend_eval) && legend_eval[[1]] == "top!"))

  # For the "top!" legend case, bump main title up to make space for the
  # legend beneath it: Take the normal main title line gap (i.e., 1.7 lines)
  # and add the difference between original top margin and new one (i.e.,
  # which should equal the height of the new legend). Note that we also
  # include a 0.1 epsilon bump, which we're using to reset the tinyplot
  # window in case of recursive "top!" calls. (See draw_legend code.)

  if (isTRUE(adj_title)) {
    line_main = par("mar")[3] - opar[["mar"]][3] + 1.7 + 0.1
  } else if (isTRUE(get_tpar("dynmar", FALSE))) {
    # Anchor main at a fixed line above the plot box so it stays in the same
    # position regardless of whether sub is also present (the sub branch
    # below adds a +1.2 shift on top of this baseline when needed).
    line_main = get_tpar("mgp")[3] + 0.7 - 0.1
  } else {
    line_main = NULL
  }

  # When sub sits on top (side.sub == 3), push main up by the sub row height
  # so main is above sub rather than overlapping it.
  if (!is.null(sub) && isTRUE(get_tpar("side.sub", 1) == 3)) {
    if (is.null(line_main)) line_main = get_tpar("mgp")[3] + 0.7 - 0.1
    line_main = line_main + 1.2
  }

  if (!is.null(sub)) {
    if (isTRUE(get_tpar("side.sub", 1) == 3)) {
      line_sub = get_tpar("line.sub", 0.7)
    } else {
      line_sub = get_tpar("line.sub", 4)
    }
    args = list(
      text = sub,
      line = line_sub,
      cex = get_tpar("cex.sub", 1.2),
      col = get_tpar("col.sub", "black"),
      adj = get_tpar(c("adj.sub", "adj")),
      font = get_tpar("font.sub", 1),
      side = get_tpar("side.sub", 1),
      las = 1
    )
    args = Filter(function(x) !is.null(x), args)
    do.call(mtext, args)
  }

  if (!is.null(main)) {
    main_lines = text_line_count(main)
    if (main_lines > 1L) {
      # Keep line 1 aligned with single-line titles by shifting the centered
      # multi-line block downward by half its extra line height.
      if (is.null(line_main)) line_main = get_tpar("mgp")[3] + 1.1
      line_main = line_main - (main_lines - 1) / 2
    }
    args = list(
      main = main,
      line = line_main,
      cex.main = get_tpar("cex.main", 1.4),
      col.main = get_tpar("col.main", "black"),
      font.main = get_tpar("font.main", 2),
      adj = get_tpar(c("adj.main", "adj"), 3))
    args = Filter(function(x) !is.null(x), args)
    do.call(title, args)
  }


  # Axis titles. For multi-line labels, base R places line 1 at
  # `line = mgp[1] - (N-1)*cex`, which pushes line 1 up into the tick-label
  # zone. Shift `line` down so line 1 aligns with where a single-line xlab
  # would be (and the extra lines extend below).
  args = list(xlab = xlab)
  xlab_lines = text_line_count(xlab)
  if (xlab_lines > 1L) {
    cex_xlab = get_tpar(c("cex.xlab", "cex.lab"), 1)
    args[["line"]] = get_tpar("mgp")[1] + (xlab_lines - 1) * cex_xlab
  }
  args[["adj"]] = get_tpar(c("adj.xlab", "adj"))
  do.call(title, args)

  # ylab: base R already places multi-line text correctly (outermost line at
  # mgp[1], subsequent lines closer to the plot), so no line shift needed.
  args = list(ylab = ylab)
  args[["adj"]] = get_tpar(c("adj.ylab", "adj"))
  do.call(title, args)
}
