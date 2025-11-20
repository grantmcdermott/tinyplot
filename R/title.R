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
  } else {
    line_main = NULL
  }

  if (!is.null(sub)) {
    if (isTRUE(get_tpar("side.sub", 1) == 3)) {
      if (is.null(line_main)) line_main = par("mgp")[3] + 1.7 - .1
      line_main = line_main + 1.2
    }
    if (isTRUE(get_tpar("side.sub", 1) == 3)) {
      line_sub = get_tpar("line.sub", 1.7)
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


  # Axis titles
  args = list(xlab = xlab)
  args[["adj"]] = get_tpar(c("adj.xlab", "adj"))
  do.call(title, args)
  args = list(ylab = ylab)
  args[["adj"]] = get_tpar(c("adj.ylab", "adj"))
  do.call(title, args)
}
