library(mvtnorm)
library(plyr)
library(abind)
library(mclust)
library(ggplot2)
library(Rmixmod)
library(compositions)
library(gridExtra)
library(mixsmsn)
library(sn)
# library(xtable)
# source('combiMod.R')
#source('R/coda-functions/ggplot_ternary.R')

devtools::load_all('../../packages/mixpack')
mixsmsn_mixture = function(mm){
  function(x, y){
    sum(mm$pii * laply(1:3, 
                       function(i, x, y) 
                         dmsn(c(x,y), 
                             xi=mm$mu[[i]], 
                             Omega=mm$Sigma[[i]], 
                             alpha=mm$shape[[i]]), x, y))
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

ilrX = data.frame(ilr_coordinates(X))
#ilrX = rbind(ilrX, ilrX)

#mm_max = fmmst(g = 3, ilrX)
# for(i in 1:20){
set.seed(1); 
mm = smsn.mmix(ilrX, g=3, group = TRUE, iter.max=5000,
               list(iter.max = 200, n.start = 20, algorithm = "Hartigan-Wong"))
mix.contour(ilrX, mm)
#  mm = fmmst(g = 3, ilrX, itmax=1000, nkmeans=50)
#   if(mm_max$loglik < mm$loglik)
#     mm_max = mm
# }
# mm = mm_max


steps = 150
x.points = seq(-2, -1, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(1, 3.5, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
#z1.points = z.grid(x1.points, y1.points, mclust_mixture(m1))
z.points_mm = z.grid(x.points, y.points, mixsmsn_mixture(mm))


seqq = pretty(range(z.points_mm), n=40)

cl = contourLines(x.points, y.points, z.points_mm, levels = seqq)

save(cl, mm, file='data/coda_skew_mixture.RData')
