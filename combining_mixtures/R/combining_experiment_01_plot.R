library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

df.cluster = ldply(1:40, function(mo){
  load(sprintf('data/hp_sim-mo_%02d-1000-spherical.RData', mo))
  res = ldply(results, function(d) d$cluster )
  res$mo = mo
  res
})
V = ldply(strsplit(df.cluster$.id, '-', fixed = TRUE))
names(V) = c('confusion', 'weight')

df.cluster = cbind(df.cluster, V)

ggplot(data = df.cluster, aes(y=AR, x=as.factor(mo))) +
  geom_boxplot(outlier.size = 0) + facet_grid(confusion~weight) + 
  scale_x_discrete(breaks=seq(5, 40 ,5))

RES = ldply(split(df.cluster, df.cluster$.id), function(df){
  g = glm(AR~mo, data = df, family = gaussian())
  data.frame('c0' = g$coefficients[1], 'mo' = g$coefficients[2], 'confusion' = df$confusion[1], 'weight' = df$weight[1])
})


noquote(
matrix(
  sprintf("%7.4f + %7.4f x",
        as.matrix(dcast(data=RES[,c('confusion', 'weight', 'c0')], 
                        formula = confusion~weight)[,-1]),
        as.matrix(dcast(data=RES[,c('confusion', 'weight', 'mo')], 
                        formula = confusion~weight)[,-1]) ),
  nrow=3))

