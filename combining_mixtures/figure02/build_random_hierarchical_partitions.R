# setwd("/home/marc/research/subjects/mixtures/combining_mixtures")
if(!exists('IFILE')) IFILE = 'data/sim-mo_01-10-spherical-fig02.RData'


source('figure02/models.R')
load(IFILE)

library(plyr)
library(mixpack)

ar_index = function(tau,hp, g){
  n_orig = length(table(g))
  MixSim::RandIndex(cluster_partition(hp[[n_orig]]), g)$AR
}



LEVEL = max(Reduce('c', llply(simulation, function(sim) sim$g)))

results = list()

## Hierarchical partition calculation
HP = llply(simulation, function(sim){
  tm = system.time(hp <- get_random_hierarchical_partition(K = ncol(sim$tau)))
  list(
    'time' = tm,
    'hp_clust' = cluster_partition(tau = sim$tau, partition = hp[[LEVEL]]),
    'g' = sim$g )
})

## Timer calculation
timer = ldply(HP, function(hp) hp$time)

library(MixSim)

cluster = ldply(HP, function(hp) unlist(RandIndex(hp$hp_clust, hp$g)))

cluster$agreement = laply(HP, function(dd) ClassProp(dd$hp_clust, dd$g))
cluster$varinf = laply(HP, function(dd) VarInf(dd$hp_clust, dd$g))


save(HP, timer, cluster, file=OFILE)
