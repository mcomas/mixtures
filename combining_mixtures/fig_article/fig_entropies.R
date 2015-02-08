N = 1000

df.coord = data.frame('tau_a' = seq(0, 1, length.out = N))
df.coord$tau_b = 1 - df.coord$tau_a

df.coord = df.coord[apply(df.coord,1,prod) != 0,]

xlog = function(x) sum(x * log(x))
logR = function(x) log(x[2]/x[1])
dich = function(x) as.numeric(x[1] < x[2])


df = data.frame(
  df.coord,
  'ent' = apply(df.coord, 1, xlog),
  'log' = apply(df.coord, 1, logR),
  'dich' = apply(df.coord, 1, dich))

library(ggplot2)
library(reshape2)

d = melt(df, id.vars = c('tau_a', 'tau_b'))
ggplot(data=d) + 
  geom_point(aes(x=tau_a, y=value, col=variable), size=1) + 
  xlab(expression(tau[a])) + facet_grid('variable~.', scales ='free') + theme_bw()


