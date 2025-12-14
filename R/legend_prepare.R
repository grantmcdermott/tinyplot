prepare_legend_context = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "col",
      "by_continuous",
      "by",
      "bubble",
      "null_by",
      "legend",
      "legend_args",
      "bubble_cex",
      "cex_fct_adj",
      "cex_dep",
      "add",
      "sub",
      "ngrps",
      "datapoints",
      "ylab"
    )
  )

  ncolors = length(col)
  lgnd_labs = rep(NA, times = ncolors)

  if (isTRUE(by_continuous)) {
    nlabs = 5
    ubyvar = unique(by)
    byvar_range = range(ubyvar)
    pbyvar = pretty(byvar_range, n = nlabs)
    pbyvar = pbyvar[pbyvar >= byvar_range[1] & pbyvar <= byvar_range[2]]
    if (length(ubyvar) == 2 && all(ubyvar %in% pbyvar)) {
      pbyvar = ubyvar
    } else if (length(pbyvar) > nlabs) {
      pbyvar = pbyvar[seq_along(pbyvar) %% 2 == 0]
    }
    pidx = rescale_num(c(byvar_range, pbyvar), to = c(1, ncolors))[-c(1:2)]
    pidx = round(pidx)
    lgnd_labs[pidx] = pbyvar
  }

  has_legend = FALSE
  dual_legend = bubble && !null_by && !isFALSE(legend)
  lgnd_cex = NULL

  if (isFALSE(legend)) {
    legend = "none"
  } else if (isTRUE(legend)) {
    legend = NULL
  }

  if (!is.null(legend) && is.character(legend) && legend == "none") {
    legend_args[["x"]] = "none"
    dual_legend = FALSE
  }

  if (null_by) {
    if (bubble && !dual_legend) {
      legend_args[["title"]] = cex_dep
      lgnd_labs = names(bubble_cex)
      lgnd_cex = bubble_cex * cex_fct_adj
    } else if (is.null(legend)) {
      legend = "none"
      legend_args[["x"]] = "none"
    }
  }

  legend_draw_flag = (is.null(legend) || !is.character(legend) || legend != "none" || bubble) && !isTRUE(add)
  has_sub = !is.null(sub)

  if (legend_draw_flag && isFALSE(by_continuous) && (!bubble || dual_legend)) {
    if (ngrps > 1) {
      lgnd_labs = if (is.factor(datapoints$by)) levels(datapoints$by) else unique(datapoints$by)
    } else {
      lgnd_labs = ylab
    }
  }

  env2env(
    environment(),
    settings,
    c(
      "lgnd_labs",
      "has_legend",
      "dual_legend",
      "lgnd_cex",
      "legend",
      "legend_args",
      "legend_draw_flag",
      "has_sub"
    )
  )
}

prepare_dual_legend = function(settings) {
  env2env(
    settings,
    environment(),
    c(
      "legend",
      "legend_args",
      "by_dep",
      "lgnd_labs",
      "type",
      "pch",
      "lty",
      "lwd",
      "col",
      "bg",
      "by_continuous",
      "lgnd_cex",
      "cex_dep",
      "bubble_cex",
      "cex_fct_adj",
      "bubble_alpha",
      "bubble_bg_alpha",
      "has_sub"
    )
  )

  legend_args = sanitize_legend(legend, legend_args)

  lgby = list(
    legend_args = modifyList(
      legend_args,
      list(x.intersp = 1, y.intersp = 1),
      keep.null = TRUE
    ),
    by_dep = by_dep,
    lgnd_labs = lgnd_labs,
    type = type,
    pch = pch,
    lty = lty,
    lwd = lwd,
    col = col,
    bg = bg,
    gradient = by_continuous,
    cex = lgnd_cex,
    has_sub = has_sub
  )

  lgbub = list(
    legend_args = modifyList(
      legend_args,
      list(title = cex_dep, ncol = 1),
      keep.null = TRUE
    ),
    lgnd_labs = names(bubble_cex),
    type = type,
    pch = pch,
    lty = lty,
    lwd = lwd,
    col = adjustcolor(par("col"), alpha.f = bubble_alpha),
    bg = adjustcolor(par("col"), alpha.f = bubble_bg_alpha),
    cex = bubble_cex * cex_fct_adj,
    has_sub = has_sub,
    draw = FALSE
  )

  env2env(environment(), settings, c("legend_args", "lgby", "lgbub"))
}
