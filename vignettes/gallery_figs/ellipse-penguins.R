library("tinyplot")
  
plt(
  body_mass ~ bill_len | species, penguins,
  yaxl = ',',
  main = 'A waddle of penguins',
  sub = 'Species stick together',
  theme = 'web',
  legend = list('top!', title = FALSE)
)
plt_add(type = 'ellipse', fill = 0.1)
# plt_add(type = 'chull', fill = 0.1, col = NA) ## another option
