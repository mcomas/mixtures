library(plyr)

df = ldply(c('Entropy', 'DEMP', 'DEMP.mod', 'Log'), function(METH){
  df = ldply(c(90, 80, 70, 60, 50, 40, 30, 20, 10), function(PI){
    load(sprintf('data/data-fig01_%s-%03d-1000-001.RData', METH, PI))
    df$pi = PI
    df
  })
  df$meth = METH
  df
})

ggplot(data=df, aes(x=mu, y=ent, col=as.factor(pi))) + geom_line() + 
  facet_grid(meth~., scales='free')
