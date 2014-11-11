
library(mvtnorm)
library(plyr)
library(abind)
library(mclust)
library(ggplot2)
library(Rmixmod)
library(compositions)
library(gridExtra)
library(EMMIXuskew)
# library(xtable)
# source('combiMod.R')
#source('R/coda-functions/ggplot_ternary.R')

devtools::load_all('../../packages/mixpack')

emmix_mixture = function(mm){
  function(x, y){
    dfmmst(c(x,y), mm$mu, mm$sigma, mm$delta, mm$dof, mm$pro)
  }
}
# La funció evalua una funció func en una graella de punts definida per x.points i y.points
# * x.points: x_i a evaluar per cada y_j
# * y.points: y_i a evaluar per cada x_j
# * func: funció a evaluar
# * ...: paràmetres de la funció func
z.grid = function(x.points, y.points, func, ...){
  z.points =  matrix(0, nrow=length(x.points), ncol=length(y.points))
  for (i in 1:length(x.points)) {
    for (j in 1:length(y.points)) {
      for(k in 1:6){
        z.points[i,j] = func(x.points[i], y.points[j], ...)
      }
    }
  }
  return(z.points)
}


load('data/selected-glass-data.RData')

ang = pi/4
rot = matrix(c(cos(ang), -sin(ang), sin(ang), cos(ang)), nrow=2)
B = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x)))) %*% rot

set.seed(1)
ilrX = data.frame(ilr(X, V = B))
ilrX = rbind(ilrX, ilrX[sample(1:nrow(ilrX), nrow(ilrX), replace = TRUE),])

set.seed(SEED)

#ilrX = rbind(ilrX, ilrX)

# mm_max = fmmst(g = 3, ilrX)
# for(i in 1:20){
mm = fmmst(g = 3, ilrX, itmax=1000)

save(mm, SEED, file=sprintf("data/fit-seed%03d.RData", SEED))