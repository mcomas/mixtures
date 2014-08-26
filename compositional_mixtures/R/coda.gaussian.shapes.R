library(mvtnorm)
library(plyr)
library(abind)
library(gtools)
library(ggtern)
library(grid)
library(compositions)

devtools::load_all('../../packages/mixpack')

options("tern.discard.external" = FALSE)

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

steps = 50
x0 = 0; y0 = 0;
x.points = seq(x0-2, x0+2, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(y0-2, y0+2, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
circular = function (x) dmvnorm(x, mean = c(x0, y0), sigma = diag(c(0.2,0.2)))
z.points_mm <- z.grid(x.points, y.points, function(x,y) circular(c(x,y)) )
#seqq = pretty(range(z.points_mm), n=3)
cl1 = contourLines(x.points, y.points, z.points_mm, levels = c(0.2, 0.4, 0.6))

x0 = 1.5; y0 = 1.5;
x.points = seq(x0-2, x0+2, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(y0-2, y0+2, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
circular = function (x) dmvnorm(x, mean = c(x0, y0), sigma = diag(c(0.2,0.2)))
z.points_mm <- z.grid(x.points, y.points, function(x,y) circular(c(x,y)) )
#seqq = pretty(range(z.points_mm), n=3)
cl2 = contourLines(x.points, y.points, z.points_mm, levels = c(0.2, 0.4, 0.6))

x0 = -1; y0 = -1;
x.points = seq(x0-2, x0+2, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(y0-2, y0+2, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
circular = function (x) dmvnorm(x, mean = c(x0, y0), sigma = matrix(c(0.3,-0.18, -0.18, 0.2), nrow=2))
z.points_mm <- z.grid(x.points, y.points, function(x,y) circular(c(x,y)) )
#seqq = pretty(range(z.points_mm), n=4)
cl3 = contourLines(x.points, y.points, z.points_mm, levels = c(0.2, 0.5, 0.8))

x0 = 1.5; y0 = -0.5;
x.points = seq(x0-2, x0+2, length.out=steps) #range(X[,2]) # 0.8386535 0.9095565
y.points = seq(y0-2, y0+2, length.out=steps) #range(X[,3]) # 0.007071446 0.044158466
circular = function (x) dmvnorm(x, mean = c(x0, y0), sigma = matrix(c(0.2,0.18, 0.18, 0.2), nrow=2))
z.points_mm <- z.grid(x.points, y.points, function(x,y) circular(c(x,y)) )
#seqq = pretty(range(z.points_mm), n=4)
cl4 = contourLines(x.points, y.points, z.points_mm, levels = c(0.2, 0.7, 1.2))

p.ilr = ggplot()
l_ply(cl1, function(v){
  df.c = data.frame('coord.1' = v$x, 'coord.2' = v$y)
  p.ilr <<- p.ilr+geom_path(data=df.c, aes(x=coord.1, y=coord.2), size=0.8)})
l_ply(cl2, function(v){
  df.c = data.frame('coord.1' = v$x, 'coord.2' = v$y)
  p.ilr <<- p.ilr+geom_path(data=df.c, aes(x=coord.1, y=coord.2), colour='red', size=0.8)})
l_ply(cl3, function(v){
  df.c = data.frame('coord.1' = v$x, 'coord.2' = v$y)
  p.ilr <<- p.ilr+geom_path(data=df.c, aes(x=coord.1, y=coord.2), colour='blue', size=0.8)})
l_ply(cl4, function(v){
  df.c = data.frame('coord.1' = v$x, 'coord.2' = v$y)
  p.ilr <<- p.ilr+geom_path(data=df.c, aes(x=coord.1, y=coord.2), colour='orange', size=0.8)})

p.ilr = p.ilr + theme_bw() + 
  xlab( expression(paste( sqrt(1 / 2),'  ', log(paste(' ',  x / y, ' ') )) )) +
  ylab( expression(paste( sqrt(2 / 3),'  ', log(paste(' ',  sqrt(paste(x,'·',  y)) / z,' ') )) ))


p.tern = ggtern()
l_ply(cl1, function(v){
  df.c = data.frame(ilrInv(data.frame(v$x,v$y),
                           V = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x))))))
  names(df.c) = c('x', 'y', 'z')
  p.tern <<- p.tern+geom_path(data=df.c, aes(x=x, y=z, z=y), size=0.8)})
l_ply(cl2, function(v){
  df.c = data.frame(ilrInv(data.frame(v$x,v$y),
                           V = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x))))))
  names(df.c) = c('x', 'y', 'z')
  p.tern <<- p.tern+geom_path(data=df.c, aes(x=x, y=z, z=y), colour='red', size=0.8)})
l_ply(cl3, function(v){
  df.c = data.frame(ilrInv(data.frame(v$x,v$y),
                           V = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x))))))
  names(df.c) = c('x', 'y', 'z')
  p.tern <<- p.tern+geom_path(data=df.c, aes(x=x, y=z, z=y), colour='blue', size=0.8)})
l_ply(cl4, function(v){
  df.c = data.frame(ilrInv(data.frame(v$x,v$y),
                           V = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x))))))
  names(df.c) = c('x', 'y', 'z')
  p.tern <<- p.tern+geom_path(data=df.c, aes(x=x, y=z, z=y), colour='orange', size=0.8)})

p.tern = p.tern + theme_bw()

pdf(file='figures/ilr_coordinates.pdf', width=8.1, height=4.9, pointsize=25)
grid.newpage()
vpa_ <- viewport(width = 0.6, height = 1, x = 0.25, y = 0.55)
vpb_ <- viewport(width = 0.5, height = 0.8, x = 0.75, y = 0.53) 
print(p.tern, vp = vpa_)
print(p.ilr, vp = vpb_)
dev.off()