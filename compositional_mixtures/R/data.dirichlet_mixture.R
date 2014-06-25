library(mvtnorm)
library(plyr)
require(abind)
require(gtools)
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
# La funció evalua una mixtura de dirichlets amb parametres:
# * pi: vector que conté les proporcions de mixtura (ha de sumar 1)
# * alpha: array 2-dimensional que, per cada segona component, conté les alpha
# del la dirichlet
dirichlet_mixture = function(x, y, pi, alpha){
  z = 1 - x - y
  sum(pi * sapply(1:length(pi), function(i){
    ddirichlet(c(x,y,z),
               alpha = alpha[,i])
  }))
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

partition = c(3, 2, 3, 3, 3, 2, 3, 3, 3, 3, 3, 2, 3, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 
              2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 3, 3, 1, 1, 1, 1, 3, 2, 2, 2, 3, 3)
pi = c(0.1827603, 0.4202961, 0.3969436)
alpha = matrix(c(1879.5881, 15891.1905, 277.7232, 
                 201.9020,  1653.0252 ,   48.3203,
                 18.52423, 147.26741,   3.49948), ncol=3)

steps = 150
x.points = seq(0.05, 0.17, length.out=steps) #range(X[,1]) # 0.0678326 0.1450958
y.points = seq(0.80, 0.94, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
z.points = z.grid(x.points, y.points, dirichlet_mixture, pi, alpha)

cl = contourLines(x.points, y.points, z.points,
                  levels = c(100, 200, 400, 800, 1600, 3200, 6400, 12000 ))

save(cl, file='data/dirichlet_mixture.RData')
