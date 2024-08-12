area_args = function(datapoints) {
  dp = datapoints
  dp$ymax = dp$y
  dp$ymin = rep.int(0, nrow(dp))
  out = list(
    datapoints = dp,
    ymax = dp$ymax,
    ymin = dp$ymin,
    type = "ribbon"
  )
  return(out)
}