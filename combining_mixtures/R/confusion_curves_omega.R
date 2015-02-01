if(!exists('METHOD')) METHOD = 'CI'
if(!exists('N')) N = 1000
if(!exists('SEED')) SEED = 1
if(!exists('DIM')) DIM = 1

library(MixSim)
library(ggplot2)
library(reshape2)
## Models 
mods = list()
mods[['CI']] = list('omega' = function(v_tau, a) 1,
                    'lambda' = function(v_tau, a, b) v_tau[a] * (v_tau[a] + v_tau[b])^-1)

mods[['DEMP']] = list('omega' = function(v_tau, a) v_tau[a],
                      'lambda' = function(v_tau, a, b) if(which.max(v_tau) == a) 1 else 0)
xlog = function(x) x * log(x)
mods[['Entropy']] = list('omega' = function(v_tau, a) 1,
                         'lambda' = function(v_tau, a, b) xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b]))

mods[['Log']] =  list('omega' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
                      'lambda' = function(v_tau.log, a, b) v_tau.log[a] - v_tau.log[b])

MULTI.SIM = lapply(1:100, function(SIM){
  lapply(seq(0.001, 0.999, length.out = 100), function(overlap_mean){
    ms = MixSim(BarOmega = overlap_mean, K=2, p=DIM, PiLow=0.01, sph=TRUE)
    list(
      'omega' = ms$BarOmega,
      'A' = list(
        'p' = ms$Pi[1],
        'f' = function(x) dnorm(x, ms$Mu[1], ms$S[1]),
        'f.log' = function(x) dnorm(x, ms$Mu[1], ms$S[1], log=TRUE),
        'x' = rnorm(N, ms$Mu[1], ms$S[1])),
      'B' = list(
        'p' = ms$Pi[2],
        'f' = function(x) dnorm(x, ms$Mu[2], ms$S[2]),
        'f.log' = function(x) dnorm(x, ms$Mu[2], ms$S[2], log=TRUE),
        'x' = rnorm(N, ms$Mu[2], ms$S[2])))
  })
})


## Distributions
C = list(
  'f' = function(x) dnorm(x, -1, 0.5),
  'f.log' = function(x) dnorm(x, -1, 0.5, log = TRUE),
  'x' = rnorm(N, -1, 0.5))

post = function(pA, pC, fA, fC, B) data.frame(
  'A' = (pA * fA(B$x))/(pA * fA(B$x) + (1-pA) * B$f(B$x) + pC * fC(B$x)),
  'B' = ((1-pA-pC) * B$f(B$x))/(pA * fA(B$x) + (1-pA) * B$f(B$x) + pC * fC(B$x)),
  'C' = (pC * fC(B$x))/(pA * fA(B$x) + (1-pA) * B$f(B$x) + pC * fC(B$x)))

post_log = function(pA, pC, fA.log, fC.log, B) data.frame(
  'A' = log(pA) + fA.log(B$x),
  'B' = log(1-pA-pC) + B$f.log(B$x),
  'C' = log(pC) + fC.log(B$x))

# pCs = c(0, 0.2, 0.4, 0.6, 0.8)
# ldfC = lapply(pCs, function(pC){
  #pAs = seq(0.001, (1-pC)*0.999, length.out = 100)

df = data.frame('omega' = Reduce('+', lapply(MULTI.SIM, function(SIM) sapply(SIM, function(sim) sim$omega ))) / 100)

ll = lapply(MULTI.SIM, function(SIM)
  sapply(SIM, function(sim) mean(apply(post(sim$A$p, 0, sim$A$f, C$f, sim$B), 1, mods[['CI']]$lambda, 1, 2))))
df$CI = apply(do.call('cbind', ll), 1, mean, na.rm=TRUE)

ll = lapply(MULTI.SIM, function(SIM)
  sapply(SIM, function(sim) mean(apply(post(sim$A$p, 0, sim$A$f, C$f, sim$B), 1, mods[['DEMP']]$lambda, 1, 2))))
df$DEMP = apply(do.call('cbind', ll), 1, mean, na.rm=TRUE)

ll = lapply(MULTI.SIM, function(SIM)
  sapply(SIM, function(sim) mean(apply(post(sim$A$p, 0, sim$A$f, C$f, sim$B), 1, mods[['Entropy']]$lambda, 1, 2))))
df$Entropy = apply(do.call('cbind', ll), 1, mean, na.rm=TRUE)

ll = lapply(MULTI.SIM, function(SIM)
  sapply(SIM, function(sim) mean(apply(post_log(sim$A$p, 0, sim$A$f.log, C$f.log, sim$B), 1, mods[['Log']]$lambda, 1, 2))))
df$Log = -apply(do.call('cbind', ll), 1, mean, na.rm=TRUE)

#df$ci = sapply(SIM, function(sim) mean(apply(post(sim$A$p, pC, sim$A$f, C$f, sim$B), 1, f_lambda, 1, 2)))
df.melt = melt(df, id.vars = 'omega')

save(df.melt, file=sprintf("data/index_confusion_curves-%s_%010d_%03d.RData", METHOD, N, SEED))

ggplot(data=df.melt) + geom_point(aes(x=omega, y=value)) + facet_grid(variable~., scale='free') + 
ylab('Confusion value') + xlab(expression(paste(omega,' (overlapping measure)')))




