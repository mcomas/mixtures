mods[['DEMP.mod']] = list('omega' = function(v_tau, a) v_tau[a],
                          'lambda' = function(v_tau, a, b) v_tau[b] * (v_tau[a] + v_tau[b])^-1)

A = list(
  'f' = function(x) dnorm(x, 0, 1),
  'x' = rnorm(N,0, 1))

B = function(lambda) list(
  'f' = function(x) dnorm(x, lambda, 1),
  'x' = rnorm(N, lambda, 1))

post = function(x, fA, fB, pA = 0.5) data.frame(
  'A' = (pA * fA(x))/(pA * fA(x) + (1-pA) * fB(x)),
  'B' = ((1-pA) * fB(x))/(pA * fA(x) + (1-pA) * fB(x) ))