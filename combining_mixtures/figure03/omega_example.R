library(MixSim)
library(ggplot2)


set.seed(6)
df = ldply(c(0.05, 0.25, 0.45), function(ome){
  ms = MixSim(MaxOmega=ome, K=3, p=2, PiLow=0.25, sph=TRUE)
  sim = simdataset(n=500, Pi=ms$Pi, Mu=ms$Mu, S=ms$S)
  
  df = data.frame(sim$X)
  df$k = as.character(sim$id)
  df$omega = ome
  df
})

ggplot(data=df, aes(x=X1, y=X2, col=k)) + geom_point(alpha=0.45) + facet_grid(.~omega) +
  theme_bw() + theme(
    #legend.position = 'top',
    legend.position = 'none',
#     legend.title = element_blank(),
#     legend.key = element_rect(colour = "white"),
#     legend.text = element_text(size = 13),
    axis.title.y = element_text(size=13, face='bold'),
    axis.title.x = element_text(size=13, face='bold'))
ggsave('figures/omega.pdf', width = 10, height = 4)