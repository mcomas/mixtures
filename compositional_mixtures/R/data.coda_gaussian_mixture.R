library(mvtnorm)
library(plyr)
require(abind)
library(mclust)
require(ggplot2)
library(Rmixmod)
library(compositions)
require(gridExtra)
# library(xtable)
# source('combiMod.R')
#source('R/coda-functions/ggplot_ternary.R')

# La funció evalua una mixtura gaussiana amb parametres:
# * pi: vector que conté les proporcions de mixtura (ha de sumar 1)
# * mu: array 2-dimensional que, per cada segona component, conté les mitjanes de 
# cada una de les normals de la mixtura
# * sigma: array 3-dimensional que, per cada tercera component, conté les sigmes de
# cada una de les normals de la mixtura.
normal_mixture = function(x, y, pi, mu, sigma){
  sum(pi * sapply(1:length(pi), function(i){
    dmvnorm(c(x,y),
            mean = mu[,i],
            sigma = sigma[,,i])
  }))
}

mclust_mixture = function(mclust_solution){
  func_pi = mclust_solution$parameters$pro
  func_mean = mclust_solution$parameters$mean
  func_sigma = mclust_solution$parameters$variance$sigma
  function(x, y){
    normal_mixture(x, y, 
                   func_pi,
                   func_mean,
                   func_sigma)
  }
}
rmixmod_mixture = function(mixmod_solution){
  func_pi = mixmod_solution@bestResult@parameters@proportions
  func_mean = t(mixmod_solution@bestResult@parameters@mean)
  func_sigma = do.call('abind', list(mixmod_solution@bestResult@parameters@variance, along = 3))
  function(x, y){
    normal_mixture(x, y, 
                   func_pi,
                   func_mean,
                   func_sigma)
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

ilrX = data.frame(ilr(X))
set.seed(1); 
mm = mixmodCluster(ilrX, nbCluster=3, models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"),
                   strategy=mixmodStrategy(algo='EM', nbTry=100, initMethod='random'))



steps = 50
x.points = seq(1.20, 1.85, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(-3.15, -1.40, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
#z1.points = z.grid(x1.points, y1.points, mclust_mixture(m1))
z.points_mm = z.grid(x.points, y.points, rmixmod_mixture(mm))


seqq = pretty(range(z.points_mm), n=50)

cl = contourLines(x.points, y.points, z.points_mm, levels = seqq)
save(cl, file='data/coda_gaussian_mixture.RData')
