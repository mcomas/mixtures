library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)
library(MixSim)

omega = 1:30

df = ldply(omega, function(om){
  load( sprintf("data/hp_sim-mo_%02d-100-spherical.RData", om) )
  d = ldply(results, function(d) d$cluster)
  d$agreement = Reduce('c', llply(results, function(d) laply(d$hp, function(dd) ClassProp(dd$hp_clust, dd$g)) ))
  d$varinf = Reduce('c', llply(results, function(d) laply(d$hp, function(dd) VarInf(dd$hp_clust, dd$g)) ))
  d$omega = om
  d
})
df = cbind(df, ldply(str_split(df$.id, '-'), function(v){ names(v) = c('lambda', 'weight'); v }))
df = df[!(df$lambda %in% c('demp2', 'cnst')),]

df.sub = df[df$omega %in% c(10,20,30,40),]
p.AR = ggplot(df) + geom_boxplot(aes(x = as.factor(omega), y=AR), outlier.size=0) + 
  facet_grid(lambda~weight) + theme_bw() +
  xlab('Omega') + ylab('adjusted Rand\'s index') +
  scale_x_discrete(breaks=seq(5, 50, 5))

p.R = ggplot(df) + geom_boxplot(aes(x = as.factor(omega), y=R), outlier.size=0) + 
  facet_grid(lambda~weight) + theme_bw() +
  xlab('Omega') + ylab('Rand\'s index') +
  scale_x_discrete(breaks=seq(5, 50, 5))

p.F = ggplot(df) + geom_boxplot(aes(x = as.factor(omega), y=F), outlier.size=0) + 
  facet_grid(lambda~weight) + theme_bw() +
  xlab('Omega') + ylab('Fowlkes and Mallows index') +
  scale_x_discrete(breaks=seq(5, 50, 5))

p.M = ggplot(df) + geom_boxplot(aes(x = as.factor(omega), y=M), outlier.size=0) + 
  facet_grid(lambda~weight) + theme_bw() +
  xlab('Omega') + ylab('Mirkin metric') +
  scale_x_discrete(breaks=seq(5, 50, 5))

p.agreement = ggplot(data=df) + geom_boxplot(aes(x = as.factor(omega), y=agreement), outlier.size=0) + 
  facet_grid(lambda~weight) + theme_bw() +
  xlab('Omega') + ylab('Partition Agreement') +
  scale_x_discrete(breaks=seq(5, 50, 5))

p.varinf = ggplot(data=df) + geom_boxplot(aes(x = as.factor(omega), y=varinf), outlier.size=0) + 
  facet_grid(lambda~weight) + theme_bw() +
  xlab('Omega') + ylab('Variation of information') +
  scale_x_discrete(breaks=seq(5, 50, 5))


  
ggsave(filename = "class.R.png", p.R)
ggsave(filename = "class.AR.png", p.AR)
ggsave(filename = "class.F.png", p.F)
ggsave(filename = "class.M.png", p.M)
ggsave(filename = "class.agreement.png", p.agreement)
ggsave(filename = "class.varinf.png", p.varinf)

# df.AR = group_by(df, lambda, weight, omega) %>% summarise( 
#   'median' = median(AR),
#   'min' = min(AR),
#   'max' = max(AR),
#   'Q1' = quantile(AR, 0.25),
#   'Q3' = quantile(AR, 0.75))
# 
# 
# ggplot(df.AR) + geom_line(aes(x=omega, y= median, col=lambda, linetype=weight))
# 
# ggplot(df.AR) + geom_line(aes(x=omega, y= median)) + geom_line(aes(x=omega, y= Q1), col='red') + 
#   geom_line(aes(x=omega, y= Q3), col='green') + 
#   facet_grid(lambda~weight)
# 
# df.AR %>% dcast( omega~.id) 
# 
# results = rbind(df1, df2)
# df = melt(results, measure.vars=names(results)[1:7])
# 
# ggplot(df, aes(x=variable, y=value))+geom_boxplot()+xlab(NULL)+ylab('Obtained score') +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle=45, vjust=0.5),
#         strip.background = element_rect(colour="white", fill="white"))+
#   facet_grid(shape~omega, labeller = label_parsed)