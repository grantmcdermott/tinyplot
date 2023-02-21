## a placeholder
plot2 = function(fml, 
                 data, 
                 by,
                 type = "p",
                 title = NULL, 
                 box = NULL, 
                 grid = NULL, 
                 pal = NULL,
                 pal.args = NULL,
                 ...) {
  
  # NSE
  nl = as.list(seq_along(data))
  names(nl) = names(data)
  by = eval(substitute(by), nl, parent.frame())
  if (is.numeric(by)) by = names(data)[by]
  
  # split data by by var(s)
  split_data = split(data, data[[by]])
  
  # colour palette
  if (is.null(pal)) pal = "Dark 2"
  pal = hcl.colors(length(split_data), palette = pal)
  
  # x and y vars
  xvar = paste(fml[3])
  yvar = paste(fml[2])
  
  # plot window
  plot.new()
  plot.window(xlim = range(data[[xvar]]), ylim = range(data[[yvar]]), ...)
  axis(1)
  axis(2)
  
  if (!is.null(grid)) grid
  if (!is.null(box)) box
  
  # draw the points/lines
  if (type %in% c("p", "o", "b")) invisible(
    lapply(seq_along(split_data), function(i) points(fml, split_data[[i]], col = pal[i], type = type))
  )
  if (type %in% c("l", "o", "b")) invisible(
    lapply(seq_along(split_data), function(i) lines(fml, split_data[[i]], col = pal[i], type = type))
  )
  
  if (!is.null(title)) title
  
}