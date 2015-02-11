if(!exists('METHOD')) METHOD = 'CI'
if(!exists('N')) N = 1000
if(!exists('SEED')) SEED = 1

## Models 
mods = list()
xlog = function(x) x * log(x)
mods[['Entropy']] = list('omega' = function(v_tau, a) 1,
                         'lambda' = function(v_tau, a, b) +xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b]))

mods[['DEMP']] = list('omega' = function(v_tau, a) v_tau[a],
                      'lambda' = function(v_tau, a, b) if(which.max(v_tau) == a) 1 else 0)

mods[['Log']] =  list('omega' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
                      'lambda' = function(v_tau.log, a, b) v_tau.log[a] - v_tau.log[b])

## Distributions
A = list(
  'f' = function(x) dnorm(x, 0, 1),
  'f.log' = function(x) dnorm(x, 0, 1, log=TRUE),
  'x' = rnorm(N,0, 1))

B = function(lambda) list(
  'f' = function(x) dnorm(x, lambda, 1),
  'f.log' = function(x) dnorm(x, lambda, log=TRUE),
  'x' = rnorm(N, lambda, 1))

post = function(x, fA, fB, pA = 0.5) data.frame(
  'A' = (pA * fA(x))/(pA * fA(x) + (1-pA) * fB(x)),
  'B' = ((1-pA) * fB(x))/(pA * fA(x) + (1-pA) * fB(x) ))

post_log = function(x, fA.log, fB.log, pA = 0.5) data.frame(
  'A' = log(pA) + fA.log(x),
  'B' = log(1-pA) + fB.log(x))

meth = mods[['Log']]


df = data.frame('mu' = seq(0, 3, length.out = 100))
ENT = lapply(names(mods), function(nm_meth){
  meth = mods[[nm_meth]]
  if(nm_meth == 'Log'){
    lapply(c(0.9, 0.75, 0.5, 0.25, 0.1), 
           function(Pi) 
             sapply(df$mu,
                    function(i){
                      x = c(A$x[seq(1,Pi*N)], B(i)$x[1:(N-Pi*N)])
                      tau.log = post_log(x, fA.log =  A$f.log, fB.log = B(i)$f.log, pA = Pi)
                      sum(apply(tau.log, 1,
                                function(y) meth$omega(y,1) * meth$lambda(y, 1, 2))) / 
                        sum(apply(tau.log, 1,
                                  function(y) meth$omega(y,1)))
                    }))
  }else{
    lapply(c(0.9, 0.75, 0.5, 0.25, 0.1), 
           function(Pi) 
                 sapply(df$mu,
                        function(i){
                          x = c(A$x[seq(1,Pi*N)], B(i)$x[1:(N-Pi*N)])
                          tau = post(x, fA = A$f, fB = B(i)$f, pA = Pi)
                          sum(apply(tau, 1,
                                    function(y) meth$omega(y,1) * meth$lambda(y, 1, 2))) / 
                            sum(apply(tau, 1,
                                      function(y) meth$omega(y,1)))
                        }))
  }
})
names(ENT) = names(mods)

save(ENT, file='data/FIG_ENTROPY.RData')

df.ent = do.call('cbind', ENT[['Entropy']])
library(reshape2)
library(ggplot2)
d = melt( cbind(df, df.ent), id.vars = 'mu')
ggplot(data=d, aes(x=mu, y=value, col=variable)) + geom_line()

df.ent = do.call('cbind', ENT[['DEMP']])
library(reshape2)
library(ggplot2)
d = melt( cbind(df, df.ent), id.vars = 'mu')
ggplot(data=d, aes(x=mu, y=value, col=variable)) + geom_line()

df.ent = do.call('cbind', ENT[['Log']])
library(reshape2)
library(ggplot2)
d = melt( cbind(df, df.ent), id.vars = 'mu')

ggplot(data=na.omit(d), aes(x=mu, y=value, col=variable)) + geom_line()
