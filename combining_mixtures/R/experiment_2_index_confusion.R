library(reshape2)
library(ggplot2)
library(plyr)

mods = list(
  'ci' = list('omega' = function(v_tau, a) 1,
              'lambda' = function(v_tau, a, b) v_tau[a]),
  'DEMP' = list('omega' = function(v_tau, a) v_tau[a],
                'lambda' = function(v_tau, a, b) if(which.max(v_tau) == b) 0 else 1),
  'Baudry' = list('omega' = function(v_tau, a) 1,
                  'lambda' = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b])),
  'Log' = list('omega' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
               'lambda' = function(v_tau, a, b) log(v_tau[a] / v_tau[b])),
  'KL' =  list('omega' = function(v_tau, a) v_tau[a],
                 'lambda' = function(v_tau, a, b) log(v_tau[a] / v_tau[b])),
  'ab' = list('omega' = function(v_tau, a) v_tau[a],
             'lambda' = function(v_tau, a, b) -v_tau[b]))

N = 10000

fA = function(x) dnorm(x, 0, 1)
xA = rnorm(N, 1, 0.5)

fB1 = function(x) dnorm(x, 1, 0.5)
xB1 = rnorm(N, 1, 0.5)

fB2 = function(x) dnorm(x, 0.5, 0.5)
xB2 = rnorm(N, 0.5, 0.5)

fB3 = function(x) dnorm(x, 0, 0.25)
xB3 = rnorm(N, 0, 0.25)

post = function(pA, fA, fB, xB) data.frame(
  'A' = (pA * fA(xB))/(pA * fA(xB) + (1-pA) * fB(xB)),
  'B' = ((1-pA) * fB(xB))/(pA * fA(xB) + (1-pA) * fB(xB)) ) 

df = ldply(seq(0,1,0.01), function(pA){
  df.post0 = post(pA, fA, fA, xA)
  df.post1 = post(pA, fA, fB1, xB1)
  df.post2 = post(pA, fA, fB2, xB2)
  df.post3 = post(pA, fA, fB3, xB3)
  res0 = laply(mods, function(mod){
    W = apply(df.post0, 1, mod$omega, 1) 
    mean(W * apply(df.post0, 1, mod$lambda, 1, 2)) /
      mean(W)
  })
  res1 = laply(mods, function(mod){
    W = apply(df.post1, 1, mod$omega, 1) 
    mean(W * apply(df.post1, 1, mod$lambda, 1, 2)) /
      mean(W)
  })
  res2 = laply(mods, function(mod){
    W = apply(df.post2, 1, mod$omega, 1) 
    mean(W * apply(df.post2, 1, mod$lambda, 1, 2)) /
      mean(W)
  })
  res3 = laply(mods, function(mod){
    W = apply(df.post3, 1, mod$omega, 1) 
    mean(W * apply(df.post3, 1, mod$lambda, 1, 2)) /
      mean(W)
  })
  rbind(c(pA, 0, res0),
        c(pA, 1, res1),
        c(pA, 2, res2),
        c(pA, 3, res3))
})
names(df) = c('p_A', 'f_B', names(mods))


# id = data.frame('p_A' = unique(df$p_A),
#            'f_B' = 'f_A', 
#            'ci' = unique(df$p_A),
#            'DEMP' = unique(df$p_A),
#            'Baudry' = -2 * unique(df$p_A) * log(2),
#            'Log' = 0,
#            'KL' = 0)
           
df$f_B = mapvalues(df$f_B, from=0:3, to=c('A', 'B1','B2', 'B3'))

df.rest = melt(df, id.vars = c('p_A', 'f_B'))
ggplot(data=df.rest) +  
  geom_line(aes(x=p_A, y=value, col=f_B)) + 
  facet_wrap(~variable,  scales = 'free') + theme_bw()

df.f = data.frame( 'x' = seq(-3, 3, 0.01))
df.f$A = fA(df.f$x)
df.f$B1 = fB1(df.f$x)
df.f$B2 = fB2(df.f$x)
df.f$B3 = fB3(df.f$x)

save(df, df.f, file='data/confusion_index.RData')
df.f.res = melt(df.f, id.vars = 'x')
ggplot(data=df.f.res, aes(x=x, y=value, col=variable)) + geom_line() +
  theme_bw()
