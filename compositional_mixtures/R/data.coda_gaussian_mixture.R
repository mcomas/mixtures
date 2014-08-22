library(mvtnorm)
library(plyr)
library(abind)
library(mclust)
library(ggplot2)
library(Rmixmod)
library(compositions)
library(gridExtra)
# library(xtable)
# source('combiMod.R')
#source('R/coda-functions/ggplot_ternary.R')
devtools::load_all('../../packages/mixpack')

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
set.seed(1); 
mm = mixmodCluster(ilrX, nbCluster=3, models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"),
                   strategy=mixmodStrategy(algo='EM', nbTry=100, initMethod='random'))

steps = 150
x.points = seq(-2, -1, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(1, 3.5, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466

f = dmixnorm_solution_func(mm) 
z.points_mm <- z.grid(x.points, y.points, function(x,y) f(c(x,y)) )

seqq = pretty(range(z.points_mm), n=50)

cl = contourLines(x.points, y.points, z.points_mm, levels = seqq)
save(cl, mm, file='data/coda_gaussian_mixture.RData')
