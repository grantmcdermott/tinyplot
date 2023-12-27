par2 = function(...) {
  
  opts = list(...)
  par2_old = as.list(.par2)
  nam = names(opts)
  
  if (length(opts$grid)) {
    grid = as.logical(opts$grid)
    if(!is.logical(grid)) stop("grid needs to be logical")
    .par2$grid = grid
  }
  
  if (length(opts$last_facet_par)) {
    # last_facet_par = as.list(opts$last_facet_par)
    last_facet_par = opts$last_facet_par
    if(!(is.null(last_facet_par) || is.list(last_facet_par))) stop("last_facet_par needs to be NULL or a list")
    .par2$last_facet_par = last_facet_par
  }
  
  if (length(opts$lmar)) {
    lmar = as.numeric(opts$lmar)
    if(!is.numeric(lmar)) stop("lmar needs to be numeric")
    if(length(lmar)!=2) stop("lmar needs to be of length 2, i.e. c(inner, outer)")
    .par2$lmar = lmar
  }
  
  ## Like par(), we want the return object to be dependent on inputs...
  
  # User didn't assign any new values, but may have requested explicit (print
  # of) some existing value(s)
  if (is.null(nam)) {
    if (!is.null(opts) && length(opts)!=0) {
      # specific values requested
      ret = (`names<-`(lapply(opts, function(x) .par2[[x]]), opts))
      if (length(ret)==1) ret = ret[[1]]
      return(ret)
    } else {
      # no specific request; return all existing values invisibly
      return(invisible(par2_old))
    }
  # assign new values, but still return old values for saving existing settings
  # a la opar = par(param = new_value)
  } else {
    `names<-`(lapply(nam, function(x) .par2[[x]]), nam)
    return(invisible(par2_old))
  }
  
}



