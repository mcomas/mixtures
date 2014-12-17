dx = 1
dy = 1
#xlimits = seq(-30,130, dx)
#ylimits = seq(-30,130, dy)
xlimits = seq(-30,130, dx)
ylimits = seq(-30,130, dy)

cm = expand.grid(X1 = xlimits, X2 = ylimits)

a = factor( apply( sapply(1:6, function(i) dmixnorm_solution(cm, solution = mixt, part=i)), 1, which.max) )
names(a) = str_replace_all(sprintf("%s_%s", cm$X1, cm$X2), '-', 'm')


res = adply(cm, 1, function(v){
  data.frame(
    'o' = a[str_replace_all(sprintf("%s_%s", v[1], v[2]), '-', 'm')],
    'r' = a[str_replace_all(sprintf("%s_%s", v[1]+dx, v[2]), '-', 'm')],
    'u' = a[str_replace_all(sprintf("%s_%s", v[1], v[2]+dy), '-', 'm')],
    'l' = a[str_replace_all(sprintf("%s_%s", v[1]-dx, v[2]), '-', 'm')],
    'd' = a[str_replace_all(sprintf("%s_%s", v[1], v[2]-dy), '-', 'm')])
})

boundary = apply(res[,c('o', 'u', 'l', 'r', 'd')], 1, function(v) all(v == v[1]))
boundary = !is.na(boundary) & !boundary
df.boundary = cm[boundary,]

cm$id = a

ggplot() + 
  geom_point(data=cm, aes(x=X1, y=X2, col=id), size=2) + 
  geom_point(data=df.boundary, aes(x=X1, y=X2), size=2) + 
  geom_point(data=data.frame(M), aes(x=X1, y=X2), col = 'black', shape = 3, size=7) + 
  theme_bw() + theme(legend.position="none") 

