mods[['dich-demp']] = list('omega' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
                            'lambda' = function(v_tau, a, b) if(which.max(v_tau) == b) 1 else 0)

A = list(
  'f' = function(x) dnorm(x, 0, 1),
  'x' = rnorm(N,0, 1))

B = function(lambda) list(
  'f' = function(x) dnorm(x, lambda, 1),
  'x' = rnorm(N, lambda, 1))

C = list(
  'f' = function(x) dnorm(x, 3, 1),
  'x' = rnorm(N,3, 1))

post = function(x, fA, fB, fC, pA = 0.5) data.frame(
  'A' = (pA * fA(x))/(pA * fA(x) + (0.5-pA) * fB(x) + 0.5 * fC(x)),
  'B' = ((0.5-pA) * fB(x))/(pA * fA(x) + (0.5-pA) * fB(x) + 0.5 * fC(x) ),
  'C' = (0.5 * fC(x))/(pA * fA(x) + (0.5-pA) * fB(x) + 0.5 * fC(x)) )

# xlog = function(x) x * log(x)
# l_confusion = list(
#   'entr' = function(v_tau, a, b) xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b]),
#   'demp' = function(v_tau, a, b) if(which.max(v_tau) == b) 1 else 0,
#   'demp.mod' = function(v_tau, a, b) v_tau[b] * (v_tau[a] + v_tau[b])^-1,
#   'coda' = function(v_tau, a, b) log(v_tau[b] / v_tau[a]),
#   'coda.norm' = function(v_tau, a, b) -log(v_tau[b] / v_tau[a])^2,
#   'prop' = function(v_tau, a, b) v_tau[b] )
# 
# # Weighing functions
# l_weight = list(
#   'cnst' = function(v_tau, a) 1,
#   'prop' = function(v_tau, a) v_tau[a],
#   'dich' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0
# )