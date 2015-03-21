library(plyr)
cluster = ldply(1:50, function(maxoverlap){
  ldply(1000 , function(nsim){
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
  ldply(1000, function(nsim){
    ldply('spherical', function(mixt){
      load(sprintf("data/rnd_hp_sim-%02d-%d-%s-fig02.RData", maxoverlap, nsim, mixt))
      cluster$maxoverlap = maxoverlap
      cluster
    })
  })
})
library(reshape2)
library(dplyr)

summ = function(.data) summarise(.data,
  'mean' = mean(value),
  'median' = median(value),
  'first' = quantile(value, 0.25),
  'third' = quantile(value, 0.75))

df.cluster = melt(cluster, id.vars = c('maxoverlap', 'lambda', 'omega')) %>% 
  group_by(variable, maxoverlap, lambda, omega) 
df.cluster.agg = df.cluster %>% summ()

df.random = melt(random, id.vars = c('maxoverlap')) %>%
  group_by(variable, maxoverlap) 
df.random.agg = df.random %>% summ()

df.random_comb = ldply(c('entr', 'demp', 'demp.mod', 'coda', 'coda.norm', 'prop'), function(lambda){
  d = ldply(c('cnst', 'prop', 'dich'), function(omega){
    d = df.random
    d$omega = omega
    d
  })
  d$lambda = lambda
  d
})
df.random.agg_comb = ldply(c('entr', 'demp', 'demp.mod', 'coda', 'coda.norm', 'prop'), function(lambda){
  d = ldply(c('cnst', 'prop', 'dich'), function(omega){
    d = df.random.agg
    d$omega = omega
    d
  })
  d$lambda = lambda
  d
})


df.cluster$procedure = 'Proposed method'
df.random_comb$procedure = 'Random choise method'
df.cluster.agg$procedure = 'Proposed method'
df.random.agg_comb$procedure = 'Random choise method'

df = union(df.cluster, df.random_comb)
df.agg = union(df.cluster.agg, df.random.agg_comb)
library(ggplot2)
df.agg = mutate(data.frame(df.agg), 
       lambda = revalue(lambda, c("coda" = "log",
                                  "coda.norm" = "norm",
                                  "demp.mod" = "degp")))
df.agg$lambda = factor(df.agg$lambda, levels=c('entr', 'demp', 'degp', 'log', 'norm', 'prop'))
df.agg = df.agg[df.agg$lambda != 'prop',]
df.agg$omega = factor(df.agg$omega, levels=c('cnst', 'prop', 'dich'))

ggplot(data=subset(df.agg, variable == 'AR' )) + 
  geom_line(aes(x=maxoverlap, y=median, col=lambda, linetype=procedure), size=0.8) +
  xlab(expression(bold(paste("Overlap (", omega, ")")))) + ylab('Adjusted Rand Index') +
  facet_grid(.~omega) + theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key = element_rect(colour = "white"),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold'))
ggsave('figure02/linesAR.pdf', width = 10, height = 5)

ggplot(data=subset(df.agg, variable == 'F' )) + 
  geom_line(aes(x=maxoverlap, y=median, col=lambda, linetype=procedure), size=0.8) +
  xlab(expression(bold(paste("Overlap (", omega, ")")))) + ylab('Fowlkes and Mallows Index') +
  facet_grid(.~omega) + theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key = element_rect(colour = "white"),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold'))
ggsave('figure02/linesF.pdf', width = 10, height = 5)

ggplot(data=subset(df.agg, variable == 'M' )) + 
  geom_line(aes(x=maxoverlap, y=median, col=lambda, linetype=procedure), size=0.8) +
  xlab(expression(bold(paste("Overlap (", omega, ")")))) + ylab('Merkin Index') +
  facet_grid(.~omega) + theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key = element_rect(colour = "white"),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold'))
ggsave('figure02/linesM.pdf', width = 10, height = 5)

ggplot(data=subset(df.agg, variable == 'agreement' )) + 
  geom_line(aes(x=maxoverlap, y=median, col=lambda, linetype=procedure), size=0.8) +
  xlab(expression(bold(paste("Overlap (", omega, ")")))) + ylab('Agreement proportion') +
  facet_grid(.~omega) + theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key = element_rect(colour = "white"),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold'))
ggsave('figure02/linesagreement.pdf', width = 10, height = 5)

library(scales)

ggplot(data=subset(df.agg, variable == 'agreement' )) + 
  geom_line(aes(x=maxoverlap, y=median, col=lambda, linetype=procedure), size=0.8) +
  xlab(expression(bold(paste("Overlap (", omega, ")")))) + ylab('Agreement proportion (logit)') +
  facet_grid(.~omega) + theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key = element_rect(colour = "white"),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold')) + coord_trans(y = "logit")
ggsave('figure02/linesagreementlogit.pdf', width = 10, height = 5)

ggplot(data=subset(df.agg, variable == 'varinf' )) + 
  geom_line(aes(x=maxoverlap, y=median, col=lambda, linetype=procedure), size=0.8) +
  xlab(expression(bold(paste("Overlap (", omega, ")")))) + ylab('Variation of information') +
  facet_grid(.~omega) + theme_bw() + theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key = element_rect(colour = "white"),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold'))
ggsave('figure02/linesvarinf.pdf', width = 10, height = 5)

# library(scales)
# every = 5
# ggplot(data=subset(df, variable == 'AR' & maxoverlap %% every == 0 ) %>% mutate('maxoverlap' = sprintf("%5.2f", maxoverlap/100))) + 
#   geom_boxplot(aes(x=maxoverlap, y=value, fill=procedure), outlier.size=0) +
#   facet_grid(lambda~omega) + theme_bw() + theme(
#     legend.position = 'top',
#     legend.title = element_blank(),
#     legend.key = element_rect(colour = "white"),
#     axis.text.x  = element_text(angle=90, hjust = 1, vjust=0.5) ) + 
#   xlab(expression(paste("Overlap (", omega, ")"))) + ylab('Adjusted Rand Index') +
#   scale_fill_grey(start = 1, end = 0.4)
# ggsave('boxplot.pdf', width = 6, height = 10)
# 
# gap = 1
# every = 6
# ggplot() +
# #  geom_vline(x=(1:50)[1:50 %% every == 0], linetype = 'dotted') + 
#   geom_point(data = subset(df.cluster, variable == 'R' & maxoverlap %% every == 0), 
#             aes(x=maxoverlap-gap, y=median ), size=2) +
#   geom_point(data = subset(df.random, variable == 'R' & maxoverlap %% every == 0), 
#             aes(x=maxoverlap+gap, y=median ), shape=15, size=2 ) +
#   geom_errorbar(data = subset(df.cluster, variable == 'R' & maxoverlap %% every == 0), 
#                 aes(x=maxoverlap-gap, y=median, ymin=first, ymax=third), width=0.9 ) +
#   geom_errorbar(data = subset(df.random, variable == 'R' & maxoverlap %% every == 0), 
#                 aes(x=maxoverlap+gap, y=median, ymin=first, ymax=third), width=0.9 ) + 
#   facet_grid(omega~lambda) + theme_bw()
# 
# # 
# # ggplot() +
# #   geom_line(data = subset(df.cluster, variable == 'AR'), 
# #             aes(x=maxoverlap, y=median ) ) +
# #   geom_line(data = subset(df.random, variable == 'AR'), 
# #             aes(x=maxoverlap, y=median ), linetype='dotted', size=0.75 ) +
# #   geom_errorbar(data = subset(df.cluster, variable == 'AR' & maxoverlap %% 5 == 0), 
# #                 aes(x=maxoverlap, y=median, ymin=first, ymax=third) ) +
# #   geom_errorbar(data = subset(df.random, variable == 'AR' & maxoverlap %% 5 == 0), 
# #                 aes(x=maxoverlap, y=median, ymin=first, ymax=third), linetype='dotted', size=0.75 ) + 
# #   facet_grid(omega~lambda) + theme_classic()
# # 
# # ggplot() + 
# #   geom_line(data = subset(df.cluster, variable == 'F'), aes(x=maxoverlap, y=value)) + 
# #   geom_line(data = subset(df.random, variable == 'F'), aes(x=maxoverlap, value), col='red') + 
# #   facet_grid(omega~lambda)
# # 
# # ggplot(data= subset(df.cluster, variable == 'M')) + geom_line(aes(x=maxoverlap, y=value)) + 
# #   facet_grid(omega~lambda)
# # 
# # ggplot() + 
# #   geom_line(data = subset(df.cluster, variable == 'agreement'), aes(x=maxoverlap, y=value)) + 
# #   geom_line(data = subset(df.random, variable == 'agreement'), aes(x=maxoverlap, value), col='red') + 
# #   facet_grid(omega~lambda)
# # 
# # ggplot(data= subset(df.cluster, variable == 'varinf')) + geom_line(aes(x=maxoverlap, y=value)) + 
# #   facet_grid(omega~lambda)
# #   
# #   
# # 
# # 
# # 
# # nsim = 10
# # mixt = 'spherical'
# # lambda = c('entr', 'demp', 'demp.mod', 'coda', 'prop')
# # omega = c('cnst', 'prop', 'dich')
# # 
# # sprintf("data/hp_sim-%02d-%d-%s-%s-%s-fig02.RData", maxoverlap, nsim, mixt, lambda, omega)
