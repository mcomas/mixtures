if(!exists('METHOD')) METHOD = 'prop-demp'
if(!exists('PI')) PI = 0.9
if(!exists('N')) N = 100
if(!exists('SEED')) SEED = 1

set.seed(SEED)
## Models 

mods = list()
source(sprintf('figure01b/model-%s.R', METHOD))

meth = mods[[METHOD]]
N2 = N/2
PI2 = PI/2

df = data.frame('mu' = seq(0, 3, length.out = 500))
df$ent = sapply(df$mu,
                function(i){
                  x = c(A$x[seq(1,PI*N2)], B(i)$x[1:(N2-PI*N2)], C$x[seq(1,N2)])
                  tau.log = post(x, A$f, B(i)$f, C$f, pA = PI2)
                  sum(apply(tau.log, 1,
                            function(y) meth$omega(y,1) * meth$lambda(y, 1, 2))) / 
                    sum(apply(tau.log, 1,
                              function(y) meth$omega(y,1)))
                })
