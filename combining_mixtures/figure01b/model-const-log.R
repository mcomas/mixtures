mods[['const-log']] =  list('omega' = function(v_tau.log, a) 1,
                      'lambda' = function(v_tau.log, a, b) v_tau.log[b] - v_tau.log[a])

A = list(
  'f' = function(x) dnorm(x, 0, 1, log=TRUE),
  'x' = rnorm(N,0, 1))

B = function(lambda) list(
  'f' = function(x) dnorm(x, lambda, log=TRUE),
  'x' = rnorm(N, lambda, 1))

C = list(
  'f' = function(x) dnorm(x, 3, 1, log=TRUE),
  'x' = rnorm(N,3, 1))

post = function(x, fA.log, fB.log, fC.log, pA = 0.5) data.frame(
  'A' = log(pA) + fA.log(x),
  'B' = log(0.5-pA) + fB.log(x),
  'C' = log(0.5) + fC.log(x) )