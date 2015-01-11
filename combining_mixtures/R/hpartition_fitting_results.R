library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

omega = 1:30

df = ldply(omega, function(om){
  load( sprintf("data/hp_sim-mo_%02d-10-spherical.RData", om) )
  d = ldply(results, function(d) d$cluster)
  d$omega = om
  d
})
df = cbind(df, ldply(str_split(df$.id, '-'), function(v){ names(v) = c('lambda', 'weight'); v }))
df$lambda = factor(df$lambda, levels=c('prop', 'coda', 'demp', 'entr'))
group_by(df, lambda, weight) %>% summarise('mean' = mean(AR)) %>% dcast(lambda~weight, value.var = 'mean')

