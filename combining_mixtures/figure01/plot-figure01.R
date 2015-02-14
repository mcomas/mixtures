library(plyr)
library(ggplot2)

df = ldply(c('Entropy', 'DEMP', 'DEMP.mod', 'Log'), function(METH){
  df = ldply(c(90, 80, 70, 60, 50, 40, 30, 20, 10), function(PI){
    load(sprintf('data/data-fig01_%s-%03d-100000-001.RData', METH, PI))
    df$pi = PI
    df
  })
  df$meth = METH
  df
})
df = df[df$mu != 0,]
df$pi = df$pi / 100
df$meth = factor(df$meth, levels=c('Entropy', 'DEMP', 'DEMP.mod', 'Log'))
p = ggplot(data=df, aes(x=mu, y=ent, linetype=as.factor(pi))) + geom_line() + #, col=as.factor(pi)
  facet_wrap(~meth, scales='free') + theme_classic() + ylab('Given score') + xlab(expression(lambda)) +
  #scale_color_discrete(name=expression(paste(pi[a],' value'))) + 
  scale_linetype_discrete(name=expression(paste(pi[a],' value')))

ggsave(p, filename = 'figure01/fig01.eps', width = 7, height = 5)

df = df[df$meth == 'Log',]

df$ent = gtools::inv.logit( df$ent )

p.log = ggplot(data=df, aes(x=mu, y=ent, linetype=as.factor(pi))) + geom_line() + #, col=as.factor(pi)
  theme_classic() + ylab(expression(paste('Given score (', logit^-1, ')'))) + xlab(expression(lambda)) +
  #scale_color_discrete(name=expression(paste(pi[a],' value'))) + 
  scale_linetype_discrete(name=expression(paste(pi[a],' value')))

ggsave(p.log, filename = 'figure01/fig02.eps', width = 5, height = 3.5)
