get_hierarchical_partition_new = function(tau, 
              varphi, 
              theta){
  ctau = tau
  K = ncol(ctau)
  partitions = list()
  partitions[[K]] = as.list(1:K)
  names(partitions[[K]]) = laply(partitions[[K]], part_name)
  for(k in K:2){
    COMB = t(expand.grid(1:k, 1:k))
    COMB = COMB[, COMB[1,] != COMB[2,]]
    rownames(COMB) = c('a', 'b')
    colnames(COMB) = col.names=apply(COMB, 2, paste, collapse='-')
    to_merge = which.max( v <- aaply(COMB, 2, function(ind){
      a = ind[1]; b = ind[2]
      sum( apply(ctau, 1, function(v_tau) varphi(v_tau, a) * theta(v_tau, a, b) ) ) / sum( apply(ctau, 1, function(v_tau) varphi(v_tau, a) ) )
    }) )
    part = COMB[,to_merge]
    partitions[[k-1]] = b_absorbes_a(partitions[[k]], part['a'], part['b'] )
    ctau[,part['b']] = ctau[,part['a']] + ctau[,part['b']]
    ctau  = ctau[,-part['a']]
  }
  class(partitions) = 'hpartition'
  partitions
}

library(mclust)

data(Baudry_etal_2010_JCGS_examples)

library(devtools)
load_all('../../packages/mixpack')

m = Mclust(ex4.1)
### Entropy
varphi = function(v_tau, a) 1
theta = function(v_tau, a, b) xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b])
hp = get_hierarchical_partition_new(tau = m$z, varphi, theta)

plot.hpartition(hp, tau = m$z, data = ex4.1)

### DEMP
varphi = function(v_tau, a) v_tau[a]
theta = function(v_tau, a, b) if(which.max(v_tau) == b) 1 else 0
hp = get_hierarchical_partition_new(tau = m$z, varphi, theta)

plot.hpartition(hp, tau = m$z, data = ex4.1)

### DEMP.mod
varphi = function(v_tau, a) v_tau[a]
theta = function(v_tau, a, b) v_tau[b] * (v_tau[a] + v_tau[b])^1
hp = get_hierarchical_partition_new(tau = m$z, varphi, theta)

plot.hpartition(hp, tau = m$z, data = ex4.1)

### DEMP.mod tau
varphi = function(v_tau, a) v_tau[a]
theta = function(v_tau, a, b) v_tau[b]
hp = get_hierarchical_partition_new(tau = m$z, varphi, theta)

plot.hpartition(hp, tau = m$z, data = ex4.1)

### Log
varphi = function(v_tau, a) v_tau[a]
theta = function(v_tau, a, b) log(v_tau[b] / v_tau[a])
hp = get_hierarchical_partition_new(tau = m$z, varphi, theta)

plot.hpartition(hp, tau = m$z, data = ex4.1)
