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
p0 = ggtern(data=df, aes(x=Ca, y=Al, z=Si), )+geom_point(size=1.1) + 
  theme_rgbw()

# df.sample = data.frame(rmixmod_rmixnorm(100000, mm3))
# names(df.sample) = c('Ca', 'Si')
# df.sample$Al = 1-df.sample$Ca-df.sample$Si

pa = p0 + geom_polygon(data=df.win, aes(x=Ca, y=Al, z=Si), color="red",alpha=0.25,size=1,linetype=1) +
  limit_tern( breaks=seq(0.2,0.8,0.2), minor_breaks=seq(0,1,0.05))

l_ply(cl1_mm, function(v){
  df.c = data.frame('Ca' = 1-v$x/100-v$y/100, 'Si' = v$x/100, 'Al' = v$y/100, 'glasses'=NA)
  pa <<- pa+geom_path(data=df.c, aes(x=Ca, y=Al, z=Si), alpha=0.4)})


pb = p0 + geom_point(size=3) + 
  limit_tern(T=.2, L=.2, R=1, breaks=c(seq(.05,.15,.05), seq(.85,.95,.05))) +
  ggtitle("Forensic Glass data set") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 12),
        legend.position="bottom",
        legend.key = element_blank())

l_ply(cl1_mm, function(v){
  df.c = data.frame('Ca' = 1-v$x/100-v$y/100, 'Si' = v$x/100, 'Al' = v$y/100, 'glasses'=NA)
  pb <<- pb+geom_path(data=df.c, aes(x=Ca, y=Al, z=Si), alpha=0.4)})

pa = pa+theme_classic()
pb = pb+theme_classic()

pdf(file='figures/elim_component_ternary.pdf', width=6.5, height=4, pointsize=10)
grid.newpage()
vpa_ <- viewport(width = 0.46, height = 0.46, x = 0.15, y = 0.74)  # the larger map
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.49 )  # the inset in upper right
print(pb, vp = vpb_)
print(pa, vp = vpa_)
dev.off()
