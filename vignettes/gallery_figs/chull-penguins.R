library("tinyplot")
  
plt(
  body_mass ~ bill_len | species, penguins,
  yaxl = ',',
  main = 'A convex hull of penguins',
  sub = 'Species stick together',
  theme = 'web',
  legend = 'bottom!'
)
plt_add(type = 'chull', fill = 0.1, col = NA)
