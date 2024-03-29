.onLoad <- function(libname, pkgname) {
  
  # https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
  # https://stackoverflow.com/questions/49056642/r-how-to-make-variable-available-to-namespace-at-loading-time?noredirect=1&lq=1
  tnypltptns <- parent.env(environment())
  assign(".tinyplot_env", new.env(), envir = tnypltptns)
  .tpar <- new.env()
  
  # Facet margin, i.e. gap between the individual facet windows
  .tpar$fmar <- if(is.null(getOption("tinyplot_fmar"))) c(1,1,1,1) else as.numeric(getOption("tinyplot_fmar"))
  
  # Other facet options
  .tpar$facet.cex <- if(is.null(getOption("tinyplot_facet.cex"))) 1 else as.numeric(getOption("tinyplot_facet.cex"))
  .tpar$facet.font <- if(is.null(getOption("tinyplot_facet.font"))) NULL else as.numeric(getOption("tinyplot_facet.font"))
  .tpar$facet.col <- if(is.null(getOption("tinyplot_facet.col"))) NULL else getOption("tinyplot_facet.col")
  .tpar$facet.bg <- if(is.null(getOption("tinyplot_facet.bg"))) NULL else getOption("tinyplot_facet.bg")
  .tpar$facet.border <- if(is.null(getOption("tinyplot_facet.border"))) NA else getOption("tinyplot_facet.border")
  
  .tpar$grid <- if(is.null(getOption("tinyplot_grid"))) FALSE else as.logical(getOption("tinyplot_grid"))
  
  # Legend margin, i.e. gap between the legend and the plot elements
  .tpar$lmar <- if(is.null(getOption("tinyplot_lmar"))) c(1.0, 0.1) else as.numeric(getOption("tinyplot_lmar"))
  
  assign(".tpar", .tpar, envir = tnypltptns)
  
}