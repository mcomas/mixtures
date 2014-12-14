# setwd("/home/marc/research/subjects/mixtures/combining_mixtures")
if(!exists('SEED')) SEED = 70
if(!exists('MAXOVERLAP')) MAXOVERLAP = 0.01
if(!exists('K0')) K0 = 3
if(!exists('Kf')) Kf = 6  ## Clusters utilitzats en el darrer pas
if(!exists('DIM')) DIM = 2
if(!exists('NSIM')) NSIM = 100
if(!exists('NDATA')) NDATA = 500

if(!exists('OFILE')) OFILE = 'data/sim-01.RData'

library(MixSim)
library(plyr)
library(reshape2)
library(ggplot2)
library(Rmixmod)
library(stringr)
library(Hmisc)
devtools::load_all('../../packages/mixpack')

set.seed(SEED)

ms = MixSim(MaxOmega=MAXOVERLAP, K=K0, p=DIM, PiLow=0.01, sph=TRUE)
SIM = simdataset(n=NDATA, Pi=ms$Pi, Mu=ms$Mu, S=ms$S)

df = data.frame(100*SIM$X)

mixt = mixmodCluster(df, 
                     nbCluster=Kf, 
                     strategy=mixmodStrategy(nbTry = 10, seed = 1))

P  = round(mixt@bestResult@parameters@proportions, 2)

M  = round(mixt@bestResult@parameters@mean, 2)

S = llply(mixt@bestResult@parameters@variance, round, 2)

sink(file = 'tex/partition-example-pars.tex')
for(i in 1:6){
  cat('\\[\n')
  cat('\\begin{array}{l@{\\hskip 0.1in}l@{\\hskip 0.1in}c }\n')
  
  cat(sprintf("\\hat{\\pi}_%d = %s, & \\hat{\\m\\mu}_%d = \\left(%s\\right), & \\hat{\\m\\Sigma}_%d = \\left(\n%s\\right), \\\\ & &\\\\ \n",
              i, paste(P[i], collapse = ','),
              i, paste(M[i,], collapse = ','),
              i, str_replace_all(latexTabular(S[[i]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))
  
  cat('\\end{array}\n')
  cat('\\]\n')
}
sink()

p <- ggplot() + 
  geom_point(data=df, aes(x=X1, y=X2), alpha=0.3) + 
  geom_point(data=data.frame(M), aes(x=X1, y=X2), col = 'black', shape = 3, size=7) + 
  theme_bw()

xlimits = seq(-30,130, 0.5)
ylimits = seq(-30,130, 0.5)
cm = expand.grid(X1 = xlimits, X2 = ylimits)
cm$z = dmixnorm_rmixmod(cm, mixmod_solution = mixt)

p.c6 <- p + stat_contour(data=cm, aes(x=X1, y=X2, z=z), col='blue')  +
  geom_point(data=data.frame(M), aes(x=X1, y=X2), col = 'black', shape = 3, size=7)
ggsave(p.c6, filename = 'figures/partition-example-mixture.pdf', width = 7, height=6)

CN = ldply(1:6, function(i){
  cn = expand.grid(X1 = xlimits, X2 = ylimits)
  cn$z = P[i] * dmvnorm(cn, mean = M[i,], sigma = S[[i]])
  cn$id = sprintf('n_{%s}',i)
  cn
})

ii = apply( mixt@bestResult@proba, 1, which.max)
df$id = laply(ii, function(i) sprintf('n_{%s}',paste(i, collapse=',')) )

p.all <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.2) +
    geom_point(data=df, aes(x=X1, y=X2), alpha=0.8, size=1) +
    stat_contour(data=CN, aes(x=X1, y=X2, z=z, col=id), alpha=0.6) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") 
ggsave(p.all, filename = 'figures/partition-example-part6.pdf', width = 10, height=2.5)

partition = list(c(1,3), 2, 4, 5, 6)

CN5 = ldply(partition, function(part){
  cn = expand.grid(X1 = xlimits, X2 = ylimits)
  cn$z = dmixnorm_rmixmod(cn, mixt, part = part) #dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_{%s}',paste(part, collapse=','))
  cn
})

ii = apply( prop_partition(mixt@bestResult@proba, partition), 1, which.max)
df$id = laply(ii, function(i) sprintf('n_{%s}',paste(partition[[i]], collapse=',')) )

( p.cn5 <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    geom_point(data=df, aes(x=X1, y=X2), alpha=0.8, size=1) +
    stat_contour(data=CN5, aes(x=X1, y=X2, z=z, col=id), alpha=0.8) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
