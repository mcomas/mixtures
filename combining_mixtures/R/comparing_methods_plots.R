apply(combn(unique(df$.id), 2), 2, function(v){
  df1 = df[df$.id == v[1],]
  df2 = df[df$.id == v[2],]
  Reduce('c', llply(results, function(d) laply(d$hp, function(dd) ClassProp(dd$hp_clust, dd$g)) ))
})

library(MixSim)
library(reshape)
library(plyr)
library(dplyr)
library(ggplot2)
v = combn(unique(df$.id), 2)[,1]
om = 1


combn(unique(df$.id), 2)

DD = ldply(1:30, function(mo){
  load( sprintf("data/hp_sim-mo_%02d-100-spherical.RData", mo) )
  
  RR = apply(expand.grid(names(results), names(results)), 1, function(v){
    a1 = llply(results[[v[1]]]$hp, function(dd) dd$hp_clust)
    a2 = llply(results[[v[2]]]$hp, function(dd) dd$hp_clust)
    data.frame(
      'v1' = v[1],
      'v2' = v[2],
      'agreement' = laply(1:100, function(i) ClassProp(a1[[i]], a2[[i]])))
  })
  LRR = ldply(RR)
  LRR$mo = mo
  LRR
})

sel = DD$v1 %in% c("cnst-cnst", "cnst-dich", "cnst-prop") | DD$v2 %in% c("cnst-cnst", "cnst-dich", "cnst-prop")
DD.res = group_by(DD[!sel,], v1, v2, mo) %>% 
  summarise( 'median' = median(agreement),
             'Q1' = quantile(agreement, 0.25),
             'Q3' = quantile(agreement, 0.75) )

DD.res[as.numeric(DD.res$v1) > as.numeric(DD.res$v2), ] = NA
p = ggplot(data=na.omit(DD.res), aes(x=as.factor(mo), y=median)) + geom_point() + 
  geom_errorbar(aes(ymin=Q1, ymax=Q3))+
  facet_grid(v1~v2)

ggsave(filename = 'out.pdf', p, height = 20, width = 20)


llply(results, function(d) laply(d$hp, function(dd) ClassProp(dd$hp_clust, dd$g)) )
