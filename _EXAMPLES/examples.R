plot2(
  Temp ~ Day | Month, 
  data = airquality, 
  type = "b"
)

plot2(
  Temp ~ Day | Month, 
  data = airquality, 
  type = "b", pch = 17,
  grid = grid(), frame.plot = FALSE,
  palette = "Tropic"
)

plot2(
  Temp ~ Day | Month, 
  data = airquality, 
  type = "b", pch = 17,
  grid = grid(), frame.plot = FALSE,
  palette = "Tropic", 
  legend.position = "right!"
)

plot2(
  Temp ~ Day | Month, 
  data = airquality, 
  type = "b", pch = 17,
  grid = grid(), frame.plot = FALSE,
  palette = "Tropic", 
  legend.position = "right!", legend.args = list(bty = "n")
)

plot2(
  Temp ~ Day | Month, 
  data = airquality, 
  type = "b", pch = 17,
  grid = grid(), frame.plot = FALSE,
  palette = "Tropic", 
  legend.position = "right!", legend.args = list(bty = "n", title = NULL)
)

plot(mtcars$mpg, mtcars$wt)
plot2(mtcars$mpg, mtcars$wt)

plot(mpg ~ wt, mtcars)
plot2(mpg ~ wt, mtcars)
plot2(mpg ~ wt, mtcars, pch = 19, palette.args = list(alpha=0.3))
plot2(mpg ~ wt, mtcars, pch = 19, palette.args = list(alpha=0.3), col = "blue")
plot2(mpg ~ wt, mtcars, grid = grid(), frame.plot = FALSE)


plot2(mpg~wt, mtcars, legend.position = "bottom!")
plot2(mpg~wt, mtcars, legend.position = "bottom", pch = 17)
plot2(mpg~wt, mtcars, legend.position = "right")
plot2(mpg~wt, mtcars, legend.position = "right!")

plot2(mtcars$wt, mtcars$mpg, by = mtcars$cyl)
plot2(mtcars$wt, mtcars$mpg, by = mtcars$cyl, legend.position = "none")
plot2(mpg ~ wt | cyl, data = mtcars)

plot2(
  mtcars$wt, mtcars$mpg, by = mtcars$cyl, 
  type = "b", main = "Cars", 
  palette ="Classic Tableau"
)
plot2(
  mpg ~ wt | cyl, data = mtcars, 
  type = "b", main = "Cars", 
  palette = "Classic Tableau"
)

plot2(
  mpg ~ wt | cyl, mtcars, 
  type = "b", main = "Cars", 
  palette = "Classic Tableau", 
  legend.position = "right!"
)

plot2(
  mpg ~ wt | cyl, 
  mtcars,
  pch = 19, 
  legend.position = "right!", legend.args = list(bty="n"),
  type = "b", main = "Cars", 
  palette = "Classic Tableau", palette.args = list(alpha = 0.3),
  frame.plot = FALSE, grid = grid()
)

# dev.off()
# 
# debugonce(plot2)

set.seed(123456L)
# 60 time periods, 30 individuals, and 5 waves of treatment
tmax = 60; imax = 30; nlvls = 5
dat = 
  expand.grid(time = 1:tmax, id = 1:imax) |>
  within({
    
    cohort      = NA
    effect      = NA
    first_treat = NA
    
    for (chrt in 1:imax) {
      cohort = ifelse(id==chrt, sample.int(nlvls, 1), cohort)
    }
    
    for (lvls in 1:nlvls) {
      effect      = ifelse(cohort==lvls, sample(2:10, 1), effect)
      first_treat = ifelse(cohort==lvls, sample(1:(tmax+20), 1), first_treat)
    }
    
    first_treat = ifelse(first_treat>tmax, Inf, first_treat)
    treat       = time>=first_treat
    rel_time    = time - first_treat
    y           = id + time + ifelse(treat, effect*rel_time, 0) + rnorm(imax*tmax)
    
    rm(chrt, lvls, cohort, effect)
  })

plot2(
  y ~ time | id, dat, type = "l", legend.position = "none",
  grid = grid(), frame.plot = FALSE
)

plot2(
  y ~ time | id, dat, type = "l", legend.position = "none",
  grid = grid(), frame.plot = FALSE, palette = "Tropic"
)

library(basetheme)
# https://github.com/karoliskoncevicius/basetheme/wiki/Theme-List
basetheme("dark")
plot2(
  y ~ time | id, dat, 
  type = "l", legend.position = "none",
  # grid = grid(), frame.plot = FALSE, 
  palette = "Tropic"
)
basetheme(NULL)
