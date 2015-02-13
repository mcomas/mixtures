mods[['Log']] =  list('omega' = function(v_tau.log, a) exp(v_tau.log[a]),
                      'lambda' = function(v_tau.log, a, b) v_tau.log[b] - v_tau.log[a])

A = list(
  'f' = function(x) dnorm(x, 0, 1, log=TRUE),
  'x' = rnorm(N,0, 1))

B = function(lambda) list(
  'f' = function(x) dnorm(x, lambda, log=TRUE),
  'x' = rnorm(N, lambda, 1))

post = function(x, fA.log, fB.log, pA = 0.5) data.frame(
  'A' = log(pA) + fA.log(x),
  'B' = log(1-pA) + fB.log(x))