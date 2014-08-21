library(mvtnorm)
library(plyr)
library(abind)
library(mclust)
library(Rmixmod)
library(grid)
library(ggplot2)
library(ggtern)
library(gridExtra)

#source('R/coda-functions/ggplot_ternary.R')
load('data/selected-glass-data.RData')
devtools::load_all('../../packages/mixpack')
load(file='data/component_elimination.RData')



options("tern.discard.external" = FALSE)  ####<<<<<<< THIS LINE SHOULD ENABLE WHAT YOU SEEK

df = X
#df$glasses = y
df.win = data.frame(
  Ca=c(0.2, 0.0, 0.0),
  Si=c(0.8, 0.8, 1.0 ),
  Al=c(0.0, 0.2, 0.0))
p0 = ggtern(data=df, aes(x=Ca, y=Al, z=Si), )+geom_point(size=1.5) + 
  theme_rgbw()


pb = p0 + 
  limit_tern(T=.2, L=.2, R=1, breaks=c(seq(.05,.15,.05), seq(.85,.95,.05))) +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 12),
        legend.position="bottom",
        legend.key = element_blank())

l_ply(cl1_mm, function(v){
  df.c = data.frame('Ca' = 1-v$x/100-v$y/100, 'Si' = v$x/100, 'Al' = v$y/100, 'glasses'=NA)
  pb <<- pb+geom_path(data=df.c, aes(x=Ca, y=Al, z=Si), alpha=0.4)})

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

pb = pb + theme_classic()
p1 = p1 + theme_classic()
p2 = p2 + theme_classic()
p3 = p3 + theme_classic()


pdf(file='figures/elim_component_all.pdf', width=8.5, height=8.5, pointsize=10)
grid.newpage()
vpa_ <- viewport(width = 0.5, height = 0.5, x = 0.25, y = 0.75)
vpb_ <- viewport(width = 0.5, height = 0.5, x = 0.75, y = 0.75)
vpc_ <- viewport(width = 0.5, height = 0.5, x = 0.25, y = 0.25)
vpd_ <- viewport(width = 0.5, height = 0.5, x = 0.78, y = 0.25)
print(p1, vp = vpa_)
print(p2, vp = vpb_)
print(p3, vp = vpc_)
print(pb, vp = vpd_)
dev.off()
