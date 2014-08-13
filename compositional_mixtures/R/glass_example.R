summary(fgl)

lcoda = c('Na', 'Mg', 'Al', 'Si', 'K', 'Ca', 'Ba', 'Fe')

apply(fgl[,lcoda] == 0, 2, sum)

llply(lcoda, function(v){ 
  eval(parse(text= sprintf("table('%s is zero  |' = fgl$%s == 0, fgl$type)", v, v)))
})

scoda = c('Na', 'Al', 'Si', 'Ca')

df = fgl[,scoda] / apply(fgl[,scoda], 1, sum)
PCbiplot(prcomp(clr(df)), cols=fgl$type)

with(fgl, boxplot(RI~type))
