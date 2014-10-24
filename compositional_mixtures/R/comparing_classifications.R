load('data/selected-glass-data.RData')

load(file='data/component_elimination.RData')

classifications = list()
classifications[['original']] = as.numeric(y)

classifications[['elim_norm']] = mm1@bestResult@partition

load('data/dirichlet_mixture_parameters.RData')
classifications[['dirichlet']] = partition

load('data/coda_gaussian_mixture.RData')
classifications[['coda_norm']] = mm@bestResult@partition

load('data/coda_skew_mixture.RData')
classifications[['coda_skew']] = mm$group

load('data/coda_skew_t_mixture.RData')
classifications[['coda_tskew']] = mm$clusters[1:nrow(X)]

library(plyr); library(dplyr)
#library(ggplot2)
library(ggtern)

df = ldply(classifications, function(part){
  data.frame(X, 'class' = as.factor(part))
})

df$.id = factor(df$.id, 
                levels = c('original', 'elim_norm', 'dirichlet', 'coda_norm', 'coda_skew', 'coda_tskew'),
                labels = c('Original', 'Real space Gaussian', 'Dirichlet', 'Simplicial Gaussian',
                           'Simplicial Skew', 'Simplicial t-Skew'))

df.win = data.frame(
  Ca=c(0.2, 0.0, 0.0),
  Si=c(0.8, 0.8, 1.0 ),
  Al=c(0.0, 0.2, 0.0),
  glasses=c(NA,NA,NA))

p0 = ggtern(data=df, aes(Ca, Al, Si, shape=class)) + theme_rgbw() +
  scale_shape_manual(values=c(20, 0, 4))
p0 + geom_point(size=4) + coord_tern(Tlim=c(0,.2), Llim=c(0.0,0.2), Rlim=c(0.8,1.0)) +
  scale_T_continuous(breaks=seq(0.05,0.15,0.05),minor_breaks=seq(0,1,0.05)) + 
  scale_L_continuous(breaks=seq(0.05,0.15,0.05),minor_breaks=seq(0,1,0.05)) + 
  scale_R_continuous(breaks=seq(0.85,0.95,0.05),minor_breaks=seq(0,1,0.05)) + 
  #  ggtitle("Forensic Glass data set") + 
  theme_bw() + guides(shape=FALSE) +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.title = element_text(colour="black", size = 15, face='bold'),
        legend.text = element_text(colour="black", size = 15),
        #legend.position="bottom",
        legend.key = element_blank()) +
  facet_wrap(~.id)

library(MixSim)
ldply(classifications, function(part){
  data.frame(RandIndex(part, classifications$original))
#  data.frame('rand.index' = rand.index(part, classifications$original),
#             'adj.rand.index' = adj.rand.index(part, classifications$original))
})

