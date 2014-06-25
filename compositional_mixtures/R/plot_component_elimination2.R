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
devtools::load_all('../mixpack')
load(file='data/component_elimination.RData')

p1 = ggplot(data=X, aes(x=Si, y=Al))+geom_point()
l_ply(cl1_mm, function(v){
  df.c = data.frame('Si' = v$x, 'Al' = v$y, 'glasses'=NA)
  p1 <<- p1+geom_path(data=df.c, aes(x=Si, y=Al), alpha=0.4)})
p1 = p1+geom_hline(aes(yintercept=0), linetype=2)

p2 = ggplot(data=X, aes(x=Ca, y=Si))+geom_point()
l_ply(cl3_mm, function(v){
  df.c = data.frame('Ca' = v$x, 'Si' = v$y, 'glasses'=NA)
  p2 <<- p2+geom_path(data=df.c, aes(x=Ca, y=Si), alpha=0.4)})

p3 = ggplot(data=X, aes(x=Al, y=Ca))+geom_point()
l_ply(cl2_mm, function(v){
  df.c = data.frame('Al' = v$y, 'Ca' = v$x, 'glasses'=NA)
  p3 <<- p3+geom_path(data=df.c, aes(x=Al, y=Ca), alpha=0.4)})
p3 = p3+geom_vline(aes(xintercept=0), linetype=2)

pdf(file='figures/elim_component_real.pdf', width=17, height=5, pointsize=25)
grid.arrange(p1 + theme_bw(), p2 + theme_bw(), p3 + theme_bw(), ncol=3)
dev.off()
