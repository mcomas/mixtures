library(plyr)
library(ggplot2)

df = ldply(c('const', 'prop', 'dich'), function(METH1){
  ldply(c('entr', 'demp', 'degp', 'log', 'norm'), function(METH2){
    ldply(c(90, 80, 70, 60, 50, 40, 30, 20, 10), function(PI){
      load(sprintf('data/data-fig01_%s-%s-%03d-100000-001.RData', METH1, METH2, PI))
      df$pi = PI
      df$meth1 = METH1
      df$meth2 = METH2
      df
    })
  })
})
df = df[df$mu != 0,]
df$pi = df$pi / 100
df$meth1 = factor(df$meth1, levels=c('const', 'prop', 'dich'))
df$meth2 = factor(df$meth2, levels=c('entr', 'demp', 'degp', 'log', 'norm'))

p = ggplot(data=df, aes(x=mu, y=ent, linetype=as.factor(pi))) + geom_line() + #, col=as.factor(pi)
  facet_grid(meth2~meth1, scales='free') + theme_bw() + ylab('Given score') + xlab(expression(mu[b])) +
  #scale_color_discrete(name=expression(paste(pi[a],' value'))) + 
  scale_linetype_discrete(name=expression(paste(pi[a],' value'))) + theme(
    'legend.position' = 'top',
    'legend.key' = element_rect(color='white'),
    'strip.background' = element_rect(fill='white', color='black'))

ggsave(p, filename = 'figure01/fig01all.eps', width = 7, height = 10)

# df = df[df$meth == 'Log',]
# 
# df$ent = gtools::inv.logit( df$ent )
# 
# p.log = ggplot(data=df, aes(x=mu, y=ent, linetype=as.factor(pi))) + geom_line() + #, col=as.factor(pi)
#   theme_classic() + ylab(expression(paste('Given score (', logit^-1, ')'))) + xlab(expression(lambda)) +
#   #scale_color_discrete(name=expression(paste(pi[a],' value'))) + 
#   scale_linetype_discrete(name=expression(paste(pi[a],' value')))
# 
# ggsave(p.log, filename = 'figure01/fig02.eps', width = 5, height = 3.5)
