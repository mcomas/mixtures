if(!exists('METHOD')) METHOD = 'DEMP'
if(!exists('PI')) PI = 0.9
if(!exists('N')) N = 100000
if(!exists('SEED')) SEED = 1

set.seed(SEED)
## Models 

mods = list()
source(sprintf('figure01/model-%s.R', METHOD))

meth = mods[[METHOD]]

df = data.frame('mu' = seq(0, 3, length.out = 500))
df$ent = sapply(df$mu,
                function(i){
                  x = c(A$x[seq(1,PI*N)], B(i)$x[1:(N-PI*N)])
                  tau.log = post(x, A$f, B(i)$f, pA = PI)
                  sum(apply(tau.log, 1,
                            function(y) meth$omega(y,1) * meth$lambda(y, 1, 2))) / 
                    sum(apply(tau.log, 1,
                              function(y) meth$omega(y,1)))
                })
