if(!exists('MAXOVERLAP')) MAXOVERLAP = 0.01
if(!exists('NSIM')) NSIM = 100
if(!exists('NDATA')) NDATA = 500
if(!exists('K0')) K0 = 3
if(!exists('Kf')) Kf = 9
if(!exists('DIM')) DIM = 5
if(!exists('SEED')) SEED = 78
if(!exists('OFILE')) OFILE = 'data/sim-04.RData'

# The following script generates `NSIM` simulations each one containing:
# - A dataset of dimension `DIM` with `NDATA` observations generated from a mixture with `K0` *spherical*
# components with max overlapping `MAXOVERLAP`
# - The component from which each observation was generated (a number between 1 and K0)
# - The posterior probability after adjusting a mixture with Kf *spherical* components to the data

library(MixSim)
library(plyr)
library(reshape2)
library(ggplot2)
library(Rmixmod)

set.seed(SEED)
simulation = llply(1:NSIM, function(i){
  ms = MixSim(MaxOmega=MAXOVERLAP, K=K0, p=DIM, PiLow=0.01, sph=TRUE)
  sim = simdataset(n=NDATA, Pi=ms$Pi, Mu=ms$Mu, S=ms$S)
  
  repeat{
    suppressWarnings(
      tau <- mixmodCluster(data.frame(sim$X), 
                           nbCluster=Kf, 
                           models=mixmodGaussianModel(listModels="Gaussian_pk_Lk_I"))@bestResult@proba)
    if(dim(tau)[1] != 0) break
  }
  tau[tau == 0] = .Machine$double.xmin  
  list('data' = sim$X, 'g' = sim$id, 'tau' = tau)
})
save(simulation, file=OFILE)