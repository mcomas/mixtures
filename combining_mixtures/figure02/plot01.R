library(plyr)
cluster = ldply(1:50, function(maxoverlap){
  ldply(10 , function(nsim){
    ldply('spherical', function(mixt){
      ldply(c('entr', 'demp', 'demp.mod', 'coda', 'coda.norm', 'prop'), function(lambda){
        ldply(c('cnst', 'prop', 'dich'), function(omega){
          load(sprintf("data/hp_sim-%02d-%d-%s-%s-%s-fig02.RData", maxoverlap, nsim, mixt, lambda, omega))
          cluster$maxoverlap = maxoverlap
          cluster$lambda = lambda
          cluster$omega = omega
          cluster
        })
      })
      
    })
  })
})
random = ldply(1:50, function(maxoverlap){
  ldply(10 , function(nsim){
    ldply('spherical', function(mixt){
      load(sprintf("data/rnd_hp_sim-%02d-%d-%s-fig02.RData", maxoverlap, nsim, mixt))
      cluster$maxoverlap = maxoverlap
      cluster
    })
  })
})
library(reshape2)
library(dplyr)

mean.cluster = group_by(cluster, maxoverlap, lambda, omega) %>% summarize(
  'R' = mean(R),
  'AR' = mean(AR),
  'F' = mean(F),
  'M' = mean(M),
  'agreement' = mean(agreement),
  'varinf' = mean(varinf))

mean.random = group_by(cluster, maxoverlap) %>% summarize(
  'R' = mean(R),
  'AR' = mean(AR),
  'F' = mean(F),
  'M' = mean(M),
  'agreement' = mean(agreement),
  'varinf' = mean(varinf))

df.cluster = melt(mean.cluster, id.vars = c('maxoverlap', 'lambda', 'omega'))
df.random = melt(mean.random, id.vars = c('maxoverlap'))

library(ggplot2)

ggplot() + 
  geom_line(data = subset(df.cluster, variable == 'R'), aes(x=maxoverlap, y=value)) + 
  geom_line(data = subset(df.random, variable == 'R'), aes(x=maxoverlap, value), col='red') + 
  facet_grid(omega~lambda)

ggplot() + 
  geom_line(data = subset(df.cluster, variable == 'AR'), aes(x=maxoverlap, y=value)) + 
  geom_line(data = subset(df.random, variable == 'AR'), aes(x=maxoverlap, value), col='red') + 
  facet_grid(omega~lambda)

ggplot(data= subset(df.cluster, variable == 'F')) + geom_line(aes(x=maxoverlap, y=value)) + 
  facet_grid(omega~lambda)

ggplot(data= subset(df.cluster, variable == 'M')) + geom_line(aes(x=maxoverlap, y=value)) + 
  facet_grid(omega~lambda)

ggplot() + 
  geom_line(data = subset(df.cluster, variable == 'agreement'), aes(x=maxoverlap, y=value)) + 
  geom_line(data = subset(df.random, variable == 'agreement'), aes(x=maxoverlap, value), col='red') + 
  facet_grid(omega~lambda)

ggplot(data= subset(df.cluster, variable == 'varinf')) + geom_line(aes(x=maxoverlap, y=value)) + 
  facet_grid(omega~lambda)
  
  



nsim = 10
mixt = 'spherical'
lambda = c('entr', 'demp', 'demp.mod', 'coda', 'prop')
omega = c('cnst', 'prop', 'dich')

sprintf("data/hp_sim-%02d-%d-%s-%s-%s-fig02.RData", maxoverlap, nsim, mixt, lambda, omega)
