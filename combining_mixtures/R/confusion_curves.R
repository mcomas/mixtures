if(!exists('METHOD')) METHOD = 'CI'
if(!exists('N')) N = 100
if(!exists('SEED')) SEED = 1

## Models 
mods = list()
mods[['CI']] = list('omega' = function(v_tau, a) 1,
                    'lambda' = function(v_tau, a, b) v_tau[a] * (v_tau[a] + v_tau[b])^-1)

mods[['DEMP']] = list('omega' = function(v_tau, a) v_tau[a],
                      'lambda' = function(v_tau, a, b) if(which.max(v_tau) == a) 1 else 0)
xlog = function(x) x * log(x)
mods[['Entropy']] = list('omega' = function(v_tau, a) 1,
                        'lambda' = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b]))

mods[['Log']] =  list('omega' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
                      'lambda' = function(v_tau, a, b) log(v_tau[a] / v_tau[b]))

## Distributions
A = list(
  'f' = function(x) dnorm(x, 0, 1),
  'x' = rnorm(N, 0, 1))

B1 = list(
  'f' = function(x) dnorm(x, 1, 0.5),
  'x' = rnorm(N, 1, 0.5))

B2 = list(
  'f' = function(x) dnorm(x, 0.5, 0.5),
  'x' = rnorm(N, 0.5, 0.5))

B3 = list(
  'f' = function(x) dnorm(x, 0, 0.25),
  'x' = rnorm(N, 0, 0.25))

C = list(
  'f' = function(x) dnorm(x, -1, 0.5),
  'x' = rnorm(N, -1, 0.5))

f_lambda = mods[[METHOD]]$lambda
post = function(pA, pC, fA, fC, B) data.frame(
  'A' = (pA * fA(B$x))/(pA * fA(B$x) + (1-pA) * B$f(B$x) + pC * fC(B$x)),
  'B' = ((1-pA-pC) * B$f(B$x))/(pA * fA(B$x) + (1-pA) * B$f(B$x) + pC * fC(B$x)),
  'C' = (pC * fC(B$x))/(pA * fA(B$x) + (1-pA) * B$f(B$x) + pC * fC(B$x)))

pCs = c(0, 0.2, 0.4, 0.6, 0.8)
ldfC = lapply(pCs, function(pC){
  pAs = seq(0.001, (1-pC)*0.999, length.out = 100)
  ldf = lapply(list(B1, B2, B3, A), function(B){
    df = data.frame('pA' = pAs)
    df$ci = sapply(df$pA, function(pA) 
      mean(apply(post(pA, pC, A$f, C$f, B), 1, f_lambda, 1, 2)))
    df
  })
  df = do.call('rbind', ldf)
  df$scenario = factor(rep(c('B1', 'B2', 'B3', 'A'), each = length(pAs)), levels = c('B1', 'B2', 'B3', 'A'))
  df
})

dfC = do.call('rbind', ldfC)
dfC$pC = rep(c(0, 0.2, 0.4, 0.6, 0.8), sapply(ldfC, nrow))
dfC$scaled_pA = dfC$pA / ( dfC$pA  + (1 - dfC$pA - dfC$pC) )

save(dfC, file=sprintf("data/index_confusion-%s_%010d_%03d.RData", METHOD, N, SEED))
