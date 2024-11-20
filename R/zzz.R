#' Operations on package load
#' @importFrom utils globalVariables
#' @param libname library name
#' @param pkgname package name name
#' @keywords internal
#' @noRd
.onLoad = function(libname, pkgname) {
  # https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
  # https://stackoverflow.com/questions/49056642/r-how-to-make-variable-available-to-namespace-at-loading-time?noredirect=1&lq=1
  tnypltptns = parent.env(environment())
  assign(".tinyplot_env", new.env(), envir = tnypltptns)
  .tpar = new.env()
  .tpar_persistent = new.env()

  assign(".tpar", .tpar, envir = tnypltptns)
  assign(".tpar_persistent", .tpar_persistent, envir = tnypltptns)

  init_tpar_persistent()

  assign(".saved_par_before", NULL, envir = get(".tinyplot_env", envir = parent.env(environment())))
  assign(".saved_par_after", NULL, envir = get(".tinyplot_env", envir = parent.env(environment())))
  assign(".last_call", NULL, envir = get(".tinyplot_env", envir = parent.env(environment())))

  globalVariables(c(
    "add",
    "asp",
    "axes",
    "by_continuous",
    "by_ordered",
    "cex_fct_adj",
    "dots",
    "draw",
    "facet_bg",
    "facet_border",
    "facet_col",
    "facet_font",
    "facet_newlines",
    "facet_rect",
    "facet_text",
    "facet.args",
    "facet",
    "facets",
    "fill",
    "flip",
    "frame.plot",
    "has_legend",
    "ifacet",
    "nfacet_cols",
    "nfacet_rows",
    "nfacets",
    "ngrps",
    "oxaxis",
    "oyaxis",
    "ribbon.alpha",
    "split_data",
    "type",
    "x",
    "xaxs",
    "xaxt",
    "xlabs",
    "xlim",
    "xlvls",
    "xmax",
    "xmin",
    "y",
    "yaxs",
    "yaxt",
    "ylabs",
    "ylim",
    "ymax",
    "ymin"
  ))
}
