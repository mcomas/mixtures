# setwd("/home/marc/research/subjects/mixtures/combining_mixtures")
require(plyr)
require(reshape2)
require(ggplot2)
require(gridExtra)

load(file='data/sim-01.RData')


sim.entropy$type = 'entropy'
sim.entropy.prop$type = 'entropy.prop'
sim.entropy.dichotomic$type = 'entropy.dichotomic'
sim.atchison.prop$type = 'atchison.prop'
sim.atchison.dichotomic$type = 'atchison.dichotomic'
d = rbind(sim.entropy, sim.entropy.prop, sim.entropy.dichotomic,
          sim.atchison.prop, sim.atchison.dichotomic)
d = reshape2::melt(d)
p1 <- ggplot(d, aes(y=value, x=type))+geom_boxplot()+facet_wrap(~variable, scales='free_y') + theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+xlab(NULL)+ylab('Obtained score')


d = rbind(sim.entropy, sim.entropy.prop, sim.entropy.dichotomic,
          sim.atchison.prop, sim.atchison.dichotomic)
d = reshape2::melt(d)
p2 <- ggplot(d, aes(y=value, x=type))+geom_boxplot()+facet_wrap(~variable, scales='free_y')+ theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+xlab(NULL)+ylab('Obtained score') 




pdf(file='tex/fig/experiment01.pdf', width=12, height=6)
grid.arrange(p1,p2,nrow=1)
dev.off()
