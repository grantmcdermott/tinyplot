type_area = function(datapoints) {
  datapoints$ymax = datapoints$y
  datapoints$ymin = rep.int(0, nrow(datapoints))
  out = list(
    datapoints = datapoints,
    ymax = datapoints$ymax,
    ymin = datapoints$ymin,
    type = "ribbon"
  )
  return(out)
}