library(mvtnorm)
library(plyr)
require(abind)
require(gtools)
require(ggtern)
require(grid)
require(compositions)
#source('R/coda-functions/ggplot_ternary.R')
load('data/selected-glass-data.RData')
load('data/coda_skew_t_mixture.RData')
devtools::load_all('../../packages/mixpack')
#pretty(range(z.points), n=50))

cl.t = cl
cl = list()
for(i in 1:length(cl.t)){
  if(cl.t[[i]]$level != 0)
    cl = c(cl, cl.t[i])
}
options("tern.discard.external" = FALSE)

df.ilr = data.frame(ilr_coordinates(X))
p.ilr = ggplot(data=df.ilr, aes(x=coord.1, y=coord.2)) + geom_point() + theme_bw()
l_ply(cl, function(v){
  df.c = data.frame('coord.1' = v$x, 'coord.2' = v$y)
  p.ilr <<- p.ilr+geom_path(data=df.c, alpha=0.4)})
p.ilr = p.ilr +
  xlab( expression(paste( sqrt(1 / 2),'  ', ln(paste(' ',  Ca / Si, ' ') )) )) +
  ylab( expression(paste( sqrt(2 / 3),'  ', ln(paste(' ',  sqrt(paste(Ca,'·',  Si)) / Al,' ') )) ))

df = X

p0 = ggtern(data=df, aes(x=Ca, y=Al, z=Si), )+geom_point(size=1.5) + 
  theme_bw()

df.win = data.frame(
  Ca=c(0.2, 0.0, 0.0),
  Si=c(0.8, 0.8, 1.0 ),
  Al=c(0.0, 0.2, 0.0))
pa = p0 + geom_polygon(data=df.win, aes(x=Ca, y=Al, z=Si), color="red",alpha=0.25,size=1,linetype=1) +
  limit_tern( breaks=seq(0.2,0.8,0.2), minor_breaks=seq(0,1,0.05))

l_ply(cl, function(v){
  df.c = data.frame(ilrInv(data.frame(v$x,v$y),
                           V = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x))))))
  names(df.c) = c('Ca', 'Si', 'Al')
  #df.c = data.frame('Ca' = v$x, 'Si' = v$y, 'Al' = 1-v$x-v$y, 'glasses'=NA)
  pa <<- pa+geom_path(data=df.c, aes(x=Ca, y=Al, z=Si), alpha=0.4)})

pb = p0 + 
  limit_tern(T=.2, L=.2, R=1, breaks=c(seq(.05,.15,.05), seq(.85,.95,.05))) +
#  ggtitle("Forensic Glass data set") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 12),
        legend.position="bottom",
        legend.key = element_blank())

l_ply(cl, function(v){
  df.c = data.frame(ilrInv(data.frame(v$x,v$y),
                           V = do.call('cbind',llply(ilr_basis(3), function(x) log(x) - mean(log(x))))))
  names(df.c) = c('Ca', 'Si', 'Al')
  #df.c = data.frame('Ca' = v$x, 'Si' = v$y, 'Al' = 1-v$x-v$y, 'glasses'=NA)
  pb <<- pb+geom_path(data=df.c, aes(x=Ca, y=Al, z=Si), alpha=0.4)})


pdf(file='figures/coda_skew_t_mixture.pdf', width=17, height=10, pointsize=25)
grid.newpage()
vpa_ <- viewport(width = 0.5, height = 0.8, x = 0.25, y = 0.5)
vpb_ <- viewport(width = 0.5, height = 1, x = 0.75, y = 0.55)
print(pb, vp = vpb_)
print(p.ilr, vp = vpa_)
# grid.newpage()
# vpa_ <- viewport(width = 0.46, height = 0.46, x = 0.15, y = 0.74)  
# vpc_ <- viewport(width = 0.35, height = 0.46, x = 0.85, y = 0.65)  
# vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.49 )  
# print(pb, vp = vpb_)
# print(pa, vp = vpa_)
# print(p.ilr, vp= vpc_)
dev.off()