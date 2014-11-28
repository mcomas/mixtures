require(mclust)

data(Baudry_etal_2010_JCGS_examples)

m = Mclust(ex4.1)

Pi = m$parameters$pro
Mu = m$parameters$mean
S = m$parameters$variance$sigma


require(ggplot2)
devtools::load_all('../../packages/mixpack')

cm = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
cm$z = dmixnorm(cm, Pi = Pi, Mu = Mu, S = S)

( p <- ggplot(ex4.1, aes(x=X1, y=X2)) + geom_point() + theme_bw() )

ggsave(p, filename = 'figures/baudry_ex4_1.pdf')

( p.c6 <- p + stat_contour(data=cm, aes(x=X1, y=X2, z=z), col='blue')  )
ggsave(p.c6, filename = 'figures/baudry_ex4_1_contour6.pdf')

( p.c6.no.sample <- ggplot() + stat_contour(data=cm, aes(x=X1, y=X2, z=z), col='blue') + theme_bw() )
ggsave(p.c6.no.sample, filename = 'figures/baudry_ex4_1_contour6_no_sample.pdf')

CN = ldply(1:6, function(i){
  cn = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
  cn$z = dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_%02d',i)
  cn
})

( p.all <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=2) + theme_bw() + theme(legend.position="none") )
ggsave(p.all, filename = 'figures/baudry_ex4_1_all_distributions.pdf')

X1s = data.frame('X1' = 6.75, 'X2' = 1)
( p.c6.one <- ggplot(X1s, aes(x=X1, y=X2)) + geom_point(size=3) + stat_contour(data=cm, aes(x=X1, y=X2, z=z), col='blue') + theme_bw() )
#ggsave(p.c6.no.sample, filename = 'figures/baudry_ex4_1_contour6_no_sample.pdf')
( p.all.one <- ggplot(X1s, aes(x=X1, y=X2)) + geom_point(size=3) + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=2) + theme_bw() + theme(legend.position="none") )

( p.all.one <- ggplot(X1s, aes(x=X1, y=X2)) + geom_point(size=3) + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.all.one, filename = 'figures/baudry_ex4_1_all_distributions_one.pdf', width = 12, height = 3.5)

X1s.tau = ldply(1:6, function(i){
  cn = X1s
  cn$X1 = cn$X1 - 1
  cn$X2 = cn$X2 + 0.8
  cn$tau = sprintf('tau[%d] == %1.1E', i, Pi[i] * dmvnorm(X1s, mean = Mu[,i], sigma = S[,,i]) / dmixnorm(X1s, Pi = Pi, Mu = Mu, S = S) )
  cn$id = sprintf('n_%02d',i)
  cn
})

( p.all.one.tau <- ggplot(X1s, aes(x=X1, y=X2)) + geom_point(size=3) + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN, aes(x=X1, y=X2, z=z, col=id)) + 
    geom_text(data=X1s.tau,  aes(x=X1, y=X2, col=id, label=tau), size=4, parse=TRUE) +
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.all.one.tau, filename = 'figures/baudry_ex4_1_all_distributions_one_tau.pdf', width = 12, height = 3.5)


############################
######### PARTICIONS

partition = list(1, 2, 3:4, 5, 6)

CN5 = ldply(partition, function(part){
  cn = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
  cn$z = dmixnorm(cn, Pi = Pi, Mu = Mu, S = S, part = part) #dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_{%s}',paste(part, collapse=','))
  cn
})

( p.cn5 <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN5, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.cn5, filename = 'figures/baudry_ex4_1_all_distributions_5c.pdf', width = 12, height = 3.5)


partition = list(c(1,6), 2, 3:4, 5)

CN4 = ldply(partition, function(part){
  cn = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
  cn$z = dmixnorm(cn, Pi = Pi, Mu = Mu, S = S, part = part) #dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_{%s}',paste(part, collapse=','))
  cn
})

( p.cn4 <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN4, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.cn4, filename = 'figures/baudry_ex4_1_all_distributions_4c.pdf', width = 9.6, height = 3.5)

partition = list(c(1,3), c(2,6), 4, 5)
CN4b = ldply(partition, function(part){
  cn = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
  cn$z = dmixnorm(cn, Pi = Pi, Mu = Mu, S = S, part = part) #dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_{%s}',paste(part, collapse=','))
  cn
})

( p.cn4b <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN4b, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.cn4b, filename = 'figures/baudry_ex4_1_all_distributions_4c_b.pdf', width = 9.6, height = 3.5)


partition = list(c(1,6), 2, 3:5)
CN3 = ldply(partition, function(part){
  cn = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
  cn$z = dmixnorm(cn, Pi = Pi, Mu = Mu, S = S, part = part) #dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_{%s}',paste(part, collapse=','))
  cn
})

( p.cn3 <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN3, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.cn3, filename = 'figures/baudry_ex4_1_all_distributions_3c.pdf', width = 7.2, height = 3.5)

partition = list(c(1,6,2), 3:5)
CN2 = ldply(partition, function(part){
  cn = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
  cn$z = dmixnorm(cn, Pi = Pi, Mu = Mu, S = S, part = part) #dmvnorm(cn, mean = Mu[,i], sigma = S[,,i])
  cn$id = sprintf('n_{%s}',paste(part, collapse=','))
  cn
})

( p.cn2 <- ggplot() + 
    stat_contour(data=cm, aes(x=X1, y=X2, z=z), alpha=0.4) +
    stat_contour(data=CN2, aes(x=X1, y=X2, z=z, col=id)) + 
    facet_wrap(~id, nrow=1) + theme_bw() + theme(legend.position="none") )
ggsave(p.cn2, filename = 'figures/baudry_ex4_1_all_distributions_2c.pdf', width = 4.8, height = 3.5)
