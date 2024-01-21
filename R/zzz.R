.onLoad <- function(libname, pkgname) {
  
  # https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
  # https://stackoverflow.com/questions/49056642/r-how-to-make-variable-available-to-namespace-at-loading-time?noredirect=1&lq=1
  plt2ptns <- parent.env(environment())
  assign(".plot2_env", new.env(), envir = plt2ptns)
  .par2 <- new.env()
  
  # Facet margin, i.e. gap between the individual facet windows
  .par2$fmar <- if(is.null(getOption("plot2_fmar"))) c(1,1,1,1) else as.numeric(getOption("plot2_fmar"))
  
  # .par2$grid <- if(is.null(getOption("plot2_grid"))) FALSE else as.logical(getOption("plot2_grid"))
  
  .par2$last_facet_par <- if(is.null(getOption("plot2_last_facet_par"))) NULL else getOption("plot2_last_facet_par")
  
  # Legend margin, i.e. gap between the legend and the plot elements
  .par2$lmar <- if(is.null(getOption("plot2_lmar"))) c(1.0, 0.1) else as.numeric(getOption("plot2_lmar"))
  
  assign(".par2", .par2, envir = plt2ptns)
  
}