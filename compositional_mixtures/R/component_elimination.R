library(mvtnorm)
library(plyr)
library(abind)
library(mclust)
library(Rmixmod)
library(grid)
library(ggplot2)
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

X = 100*X
#m1 = Mclust(X[,-1], G=3, modelNames='VVV')
mm1 = mixmodCluster(X[,-1], nbCluster=3, models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"),
                    strategy=mixmodStrategy(algo='EM', nbTry=100, initMethod='random', seed=6))
#m2 = Mclust(X[,-2], G=3, modelNames='VVV')
mm2 = mixmodCluster(X[,-2], nbCluster=3, models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"),
                    strategy=mixmodStrategy(algo='EM', nbTry=100, initMethod='random', seed=2))
#m3 = Mclust(X[,-3], G=3, modelNames='VVV')
mm3 = mixmodCluster(X[,-3], nbCluster=3, models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"),
                    strategy=mixmodStrategy(algo='EM', nbTry=100, initMethod='random', seed=2))


steps = 100
x1.points = seq(80, 94, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y1.points = seq(-3, 7, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
#z1.points = z.grid(x1.points, y1.points, mclust_mixture(m1))
z1.points_mm = z.grid(x1.points, y1.points, rmixmod_mixture(mm1))

x2.points = seq(5, 17, length.out=steps) #range(X[,1]) # 0.0678326 0.1450958
y2.points = seq(-3, 7, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
#z2.points = z.grid(x2.points, y2.points, mclust_mixture(m2))
z2.points_mm = z.grid(x2.points, y2.points, rmixmod_mixture(mm2))

x3.points = seq(5, 17, length.out=steps) #range(X[,1]) # 0.0678326 0.1450958
y3.points = seq(80, 94, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
#z3.points = z.grid(x3.points, y3.points, mclust_mixture(m3))
z3.points_mm = z.grid(x3.points, y3.points, rmixmod_mixture(mm3))

#seq1 = seq2 = exp(seq(log(0.05), log(max(max(z1.points), max(z2.points))), length.out=10))
seq1 = pretty(range(z1.points_mm), n=50)
seq2 = pretty(range(z2.points_mm), n=50)
seq3 = pretty(range(z3.points_mm), n=50)
seq1 = c(0.0005, 0.0010, 0.0025, 0.0050, 0.01, seq1[2:length(seq1)])
seq2 = c(0.0005, 0.0010, 0.0025, 0.0050, 0.01, seq2[2:length(seq2)])
seq3 = c(0.0005, 0.0010, 0.0025, 0.0050, 0.01, seq3[2:length(seq3)])
#cl1 = contourLines(x1.points, y1.points, z1.points, levels = seq1)
#cl2 = contourLines(x2.points, y2.points, z2.points, levels = seq2)
#cl3 = contourLines(x3.points, y3.points, z3.points, levels = seq3)
cl1_mm = contourLines(x1.points, y1.points, z1.points_mm, levels = seq1)
cl2_mm = contourLines(x2.points, y2.points, z2.points_mm, levels = seq2)
cl3_mm = contourLines(x3.points, y3.points, z3.points_mm, levels = seq3)

save(mm1, mm2, mm3, cl1_mm, cl2_mm, cl3_mm, file='data/component_elimination.RData')
# par(mfrow=c(1,2), mar=c(2,2,2,2))
# # Without first component
# df1 = data.frame( x = X[,2], z = X[,3])
# plot(df1, ylim=c(0,0.05))
# l_ply(1:length(cl1), function(i){lines(data.frame(x = cl1[[i]]$x, y = cl1[[i]]$y), col='grey')})
# points(df1)
# 
# # Without second component
# df2 = data.frame( x =-X[,1], z = X[,3])
# plot(df2, ylim=c(0,0.05))
# l_ply(1:length(cl2), function(i){lines(data.frame(x = -cl2[[i]]$x, y = cl2[[i]]$y), col='grey')})
# points(df2)
# par(mfrow=c(1,1))

