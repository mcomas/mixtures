library(mvtnorm)
library(plyr)
require(abind)
library(mclust)
library(Rmixmod)
require(grid)
require(ggplot2)
require(ggtern)
require(gridExtra)

#source('R/coda-functions/ggplot_ternary.R')
load('data/selected-glass-data.RData')
devtools::load_all('../../packages/mixpack')
load(file='data/component_elimination.RData')
X = 100 * X
p1 = ggplot(data=X, aes(x=Si, y=Al))+geom_point()
l_ply(cl1_mm, function(v){
  df.c = data.frame('Si' = v$x, 'Al' = v$y, 'glasses'=NA)
  p1 <<- p1+geom_path(data=df.c, aes(x=Si, y=Al), alpha=0.4)})
p1 = p1+geom_hline(aes(yintercept=0), linetype=2)

p2 = ggplot(data=X, aes(x=Ca, y=Si))+geom_point()
l_ply(cl3_mm, function(v){
  df.c = data.frame('Ca' = v$x, 'Si' = v$y, 'glasses'=NA)
  p2 <<- p2+geom_path(data=df.c, aes(x=Ca, y=Si), alpha=0.4)})

dl = data.frame('Ca' = seq(min(pmin(X$Ca, 100-X$Si)),max(pmax(X$Ca, 100- X$Si)), 0.5))
dl$Si = 100 - dl$Ca
p2 = p2+geom_line(data=dl, aes(x=Ca, y=Si), linetype=2)


p3 = ggplot(data=X, aes(x=Al, y=Ca))+geom_point()
l_ply(cl2_mm, function(v){
  df.c = data.frame('Al' = v$y, 'Ca' = v$x, 'glasses'=NA)
  p3 <<- p3+geom_path(data=df.c, aes(x=Al, y=Ca), alpha=0.4)})
p3 = p3+geom_vline(aes(xintercept=0), linetype=2)

p1 = p1 + theme_classic()
p2 = p2 + theme_classic()
p3 = p3 + theme_classic()

pdf(file='figures/elim_component_real.pdf', width=10, height=3, pointsize=10)
grid.arrange(p1, p2, p3, ncol=3)
dev.off()
