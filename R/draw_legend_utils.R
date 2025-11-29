restore_margin_outer = function() {
  par(omd = c(0,1,0,1))
}


restore_margin_inner = function(ooma, topmar_epsilon = 0.1) {
  ooma = par("oma")
  omar = par("mar")

  if (!any(ooma != 0)) return(invisible(NULL))

  ## restore inner margin defaults
  ## (in case the plot region/margins were affected by the preceding tinyplot call)
  if (any(ooma != 0)) {
    if (ooma[1] != 0 && omar[1] == par("mgp")[1] + 1 * par("cex.lab")) {
      omar[1] = 5.1
    }
    if (ooma[2] != 0 && omar[2] == par("mgp")[1] + 1 * par("cex.lab")) {
      omar[2] = 4.1
    }
    if (ooma[3] == topmar_epsilon && omar[3] != 4.1) {
      omar[3] = 4.1
    }
    if (ooma[4] != 0 && omar[4] == 0) {
      omar[4] = 2.1
    }
    par(mar = omar)
  }
  ## restore outer margin defaults (with a catch for custom mfrow plots)
  if (all(par("mfrow") == c(1, 1))) {
    par(omd = c(0, 1, 0, 1))
  }
}


compute_legend_args = function(
  legend,
  legend_args,
  by_dep,
  lgnd_labs,
  labeller = NULL,
  type,
  pch,
  lty,
  lwd,
  col,
  bg,
  cex,
  gradient
) {
  legend_args = sanitize_legend(legend, legend_args)
  ## Use `!exists` rather than `is.null` for title in case user specified no title
  if (!exists("title", where = legend_args)) legend_args[["title"]] = by_dep
  legend_args[["pch"]] = legend_args[["pch"]] %||% pch
  legend_args[["lty"]] = legend_args[["lty"]] %||% lty
  legend_args[["col"]] = legend_args[["col"]] %||% col
  legend_args[["bty"]] = legend_args[["bty"]] %||% "n"
  legend_args[["horiz"]] = legend_args[["horiz"]] %||% FALSE
  legend_args[["xpd"]] = legend_args[["xpd"]] %||% NA
  if (!isTRUE(type %in% c("p", "ribbon", "polygon", "polypath"))) {
    legend_args[["lwd"]] = legend_args[["lwd"]] %||% lwd
  }
  if (is.null(type) || type %in% c("p", "pointrange", "errorbar", "text")) {
    legend_args[["pt.cex"]] = legend_args[["pt.cex"]] %||% (cex %||% par("cex"))
  }
  if (isTRUE(type %in% c("rect", "ribbon", "boxplot", "hist", "histogram", "spineplot", "ridge", "violin")) || gradient) {
    legend_args[["pch"]] = 22
    legend_args[["pt.cex"]] = legend_args[["pt.cex"]] %||% 3.5
    legend_args[["y.intersp"]] = legend_args[["y.intersp"]] %||% 1.25
    legend_args[["seg.len"]] = legend_args[["seg.len"]] %||% 1.25
  }
  if (isTRUE(type %in% c("ribbon", "hist", "histogram", "spineplot"))) {
    legend_args[["pt.lwd"]] = legend_args[["pt.lwd"]] %||% 0
  }
  if (identical(type, "p")) {
    legend_args[["pt.lwd"]] = legend_args[["pt.lwd"]] %||% lwd
  }
  if (identical(type, "n") && isFALSE(gradient)) {
    legend_args[["pch"]] = legend_args[["pch"]] %||% par("pch")
  }
  if (identical(type, "spineplot")) {
    legend_args[["pt.bg"]] = legend_args[["pt.bg"]] %||% legend_args[["col"]]
  }
  if (identical(type, "ridge") && isFALSE(gradient)) {
    legend_args[["pt.bg"]] = legend_args[["pt.bg"]] %||% sapply(legend_args[["col"]], function(ccol) seq_palette(ccol, n = 2)[2])
  }
  legend_args[["pt.bg"]] = legend_args[["pt.bg"]] %||% bg
  legend_args[["legend"]] = legend_args[["legend"]] %||% lgnd_labs
  if (length(lgnd_labs) != length(eval(legend_args[["legend"]]))) {
    warning(
      "\nUser-supplied legend labels do not match the number of groups.\n",
      "Defaulting to automatic labels determined by the group splits in `by`,\n"
    )
    legend_args[["legend"]] = lgnd_labs
  }
  if (!is.null(legend_args[["labeller"]])) {
    labeller = legend_args[["labeller"]]
    legend_args[["labeller"]] = NULL
    legend_args[["legend"]] = tinylabel(legend_args[["legend"]], labeller = labeller)
  }
  if (isTRUE(gradient)) {
    legend_args[["ncol"]] = NULL
  }
  # flag for multicolumn legend
  mcol_flag = !is.null(legend_args[["ncol"]]) && legend_args[["ncol"]] > 1
  # flag for (extra) user inset (also used for dual legends)
  user_inset = !is.null(legend_args[["inset"]])

  # placement flags and anchor normalization (no par() calls here)
  outer_side = outer_end = outer_right = outer_bottom = FALSE
  if (grepl("right!$|left!$", legend_args[["x"]])) {
    outer_side = TRUE
    outer_right = grepl("right!$", legend_args[["x"]])
  } else if (grepl("bottom!$|top!$", legend_args[["x"]])) {
    outer_end = TRUE
    outer_bottom = grepl("bottom!$", legend_args[["x"]])
  }

  ## Switch position anchor (we'll adjust relative to the _opposite_ side below)
  if (outer_end) {
    if (outer_bottom) {
      legend_args[["x"]] = gsub("bottom!$", "top", legend_args[["x"]])
    }
    if (!outer_bottom) {
      legend_args[["x"]] = gsub("top!$", "bottom", legend_args[["x"]])
    }

    # enforce horizontal legend if user hasn't specified ncol arg
    # (exception: gradient legends at bottom/top are always horizontal)
    if (is.null(legend_args[["ncol"]]) || gradient) legend_args[["horiz"]] = TRUE

  } else if (outer_side) {
    if (outer_right) {
      legend_args[["x"]] = gsub("right!$", "left", legend_args[["x"]])
    }
    if (!outer_right) {
      legend_args[["x"]] = gsub("left!$", "right", legend_args[["x"]])
    }
  } else {
      legend_args[["inset"]] = 0
  }

  # Additional tweaks for horiz and/or multi-column legends
  if (isTRUE(legend_args[["horiz"]]) ||  mcol_flag) {
    # tighter horizontal labelling
    # See: https://github.com/grantmcdermott/tinyplot/issues/434
    if (!gradient) {
      legend_args[["text.width"]] = NA
      # Add a space to all labs except the outer most right ones
      nlabs = length(legend_args[["legend"]])
      nidx = nlabs
      if (mcol_flag) nidx = tail(1:nlabs, (nlabs %/% legend_args[["ncol"]]))
      legend_args[["legend"]][-nidx] = paste(legend_args[["legend"]][-nidx], " ")
    }
    # catch for horizontal ribbon legend spacing
    if (type=="ribbon") {
      if (legend_args[["pt.lwd"]] == 1) {
        legend_args[["x.intersp"]] = 1
      } else {
        legend_args[["x.intersp"]] = 0.5
      }
    } else if (gradient) {
      legend_args[["x.intersp"]] = 0.5
    }
  }
  
  list(
    legend_args = legend_args,
    mcol_flag = mcol_flag,
    user_inset = user_inset,
    outer_side = outer_side,
    outer_end = outer_end,
    outer_right = outer_right,
    outer_bottom = outer_bottom
  )
}
