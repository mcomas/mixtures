# setwd("/home/marc/research/subjects/mixtures/combining_mixtures")
if(!exists('IFILE')) IFILE = 'data/sim-mo_01-10-spherical.RData'
if(!exists('OFILE')) OFILE = 'data/hp_sim-mo_01-10-spherical.RData'

load(IFILE)

library(plyr)
devtools::load_all('../../packages/mixpack')

ar_index = function(tau,hp, g){
  n_orig = length(table(g))
  MixSim::RandIndex(cluster_partition(hp[[n_orig]]), g)$AR
}

l_confusion = list(
  'cnst' = function(v_tau, a, b) 1,
  'entr' = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b]),
  'demp' = function(v_tau, a, b) if(which.max(v_tau) == b) 0 else 1,
  'coda' = function(v_tau, a, b) log(v_tau[a] / v_tau[b]),
  'prop' = function(v_tau, a, b) -v_tau[b]
)

# Weighing functions
l_weight = list(
  'cnst' = function(v_tau, a) 1,
  'dich' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
  'prop' = function(v_tau, a) v_tau[a]
)

LEVEL = max(simulation[[1]]$g)

results = list()
for(lambda in names(l_confusion)){
  for(omega in names(l_weight)){
    f_lambda = l_confusion[[lambda]]
    f_omega = l_weight[[omega]]
    HP = llply(simulation, function(sim){
      tm = system.time(hp <- get_hierarchical_partition(tau = sim$tau, varphi = f_omega, theta = f_lambda))
      list(
        'time' = tm,
        'hp_clust' = cluster_partition(tau = sim$tau, partition = hp[[LEVEL]]),
        'g' = sim$g )
    })
    results[[sprintf('%s-%s', lambda, omega)]] = list(
      'hp' = HP,
      'cluster' = ldply(HP, function(hp) unlist(MixSim::RandIndex(hp$hp_clust, hp$g))),
      'timer' = ldply(HP, function(hp) hp$time) )
  }
}

save(results, file=OFILE)