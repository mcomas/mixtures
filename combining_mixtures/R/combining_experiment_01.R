# setwd("/home/marc/research/subjects/mixtures/combining_mixtures")
if(!exists('SEED')) SEED = 78
if(!exists('MAXOVERLAP')) MAXOVERLAP = 0.01
if(!exists('K0')) K0 = 3
if(!exists('Kf')) Kf = 9  ## Clusters utilitzats en el darrer pas
if(!exists('DIM')) DIM = 5
if(!exists('NSIM')) NSIM = 100
if(!exists('NDATA')) NDATA = 500

if(!exists('OFILE')) OFILE = 'data/sim-01.RData'

require(MixSim)
require(plyr)
require(reshape2)
require(ggplot2)
require(Rmixmod)


set.seed(SEED)

ms = MixSim(MaxOmega=MAXOVERLAP, K=K0, p=DIM, PiLow=0.01, sph=TRUE)
SIM = simdataset(n=NDATA, Pi=ms$Pi, Mu=ms$Mu, S=ms$S)

pdf(file='figures/experiment_01_sim_dat.pdf', width=8, height=7)
plot(princomp(SIM$X)$scores, col=SIM$id, main='Simulated data')
dev.off()

#suppressMessages( source('R/combining_partitions.R') ) 
devtools::load_all('../mixpack')

simulation = function(step, varphi, theta){
  ## A mixture model is adjusted to data. The adjustment is reapeted until the end.
  repeat{
    suppressWarnings(
      tau <- mixmodCluster(data.frame(SIM$X), 
                           nbCluster=Kf, 
                           strategy=mixmodStrategy(nbTry = 3))@bestResult@proba)
    if(dim(tau)[1] != 0) break
  }
  tau[tau==0] = .Machine$double.xmin
  hp = get_hierarchical_partition(tau, varphi, theta)
  
  df = data.frame(princomp(SIM$X)$scores[,1:2])
  names(df) = c('X1', 'X2')
  df$cluster = cluster_partition(tau, hp[[K0]])
  
  data.frame(RandIndex(df$cluster, SIM$id)  )
}

sim.entropy = ldply(1:NSIM, simulation, 
                    varphi = function(v_tau, a) 1,
                    theta = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b]) )

sim.entropy.prop = ldply(1:NSIM, simulation, 
                    varphi = function(v_tau, a) v_tau[a],
                    theta = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b]) )

sim.entropy.dichotomic = ldply(1:NSIM, simulation, 
                               varphi = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
                               theta = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b]) )

sim.atchison.prop = ldply(1:NSIM, simulation, 
                     varphi = function(v_tau, a) v_tau[a],
                     theta = function(v_tau, a, b) log(v_tau[a] / v_tau[b])^2 )

sim.atchison.dichotomic = ldply(1:NSIM, simulation, 
                     varphi = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
                     theta = function(v_tau, a, b) log(v_tau[a] / v_tau[b])^2 )

save(sim.entropy, sim.entropy.prop, sim.entropy.dichotomic, sim.atchison.prop, sim.atchison.dichotomic, 
     file=OFILE)
