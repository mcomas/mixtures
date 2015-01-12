devtools::load_all('../../packages/mixpack')
library(reshape2)
library(ggtern)
# Confusion functions
l_confusion = list(
#  'cnst' = function(v_tau, a, b) 1,
  'prop' = function(v_tau, a, b) -v_tau[b],
  'entr' = function(v_tau, a, b) -xlog(v_tau[a] + v_tau[b]) + xlog(v_tau[a]) + xlog(v_tau[b]),
  'demp' = function(v_tau, a, b) if(which.max(v_tau) == b) 0 else 1,
  'coda' = function(v_tau, a, b) log(v_tau[a] / v_tau[b])

)

# Weighing functions
l_weight = list(
  'cnst' = function(v_tau, a) 1,
  'dich' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0,
  'prop' = function(v_tau, a) v_tau[a]
)

N = 200
seqs = seq(1/N, 1-1/N, length.out = N)
X = data.frame( expand.grid(seqs, seqs) )
names(X) = c('a', 'b')
X$other = 1 - X$a - X$b

valid = with(X, a > 0 & a < 1 & b > 0 & b < 1 & other > 0 & other < 1)

plots = list()
for(str_confusion in names(l_confusion) ){
  for(str_weight in names(l_weight) ){
    f_confusion = l_confusion[[str_confusion]]
    f_weight = l_weight[[str_weight]]
    
    f = function(v_tau, a, b) f_weight(v_tau, a) * f_confusion(v_tau, a, b)
    X$funct = NULL
    X$funct = apply(X, 1, f, a = 1, b = 2)

    cc <- contourLines(seqs, seqs, as.matrix(dcast(data = X[,c('a', 'b', 'funct')], a~b), nrow=N)[,-1])
    
    
    
    p = ggtern() +
      geom_point(data=X[valid,], aes(x=a, y=other, z=b, col=funct), alpha=0.3) + 
      scale_color_continuous(low="white",high="red")
    for(i in 1:length(cc)){
      icc = cc[[i]]
      df = data.frame('a' = icc$x, 'b' = icc$y)
      df$other = 1 - df$a - df$b
      df$funct = icc$level
      p  = p + geom_path(data = df, aes(x=a, y=other, z=b), size=0.3)
    }
    str_title = sprintf("%s-%s", str_confusion, str_weight)
    if(str_confusion == 'coda' & str_weight == 'dich')
      str_title = sprintf("%s-%s (Logratio)", str_confusion, str_weight)
    if(str_confusion == 'demp' & str_weight == 'prop')
      str_title = sprintf("%s-%s (Hennig)", str_confusion, str_weight)
    if(str_confusion == 'entr' & str_weight == 'cnst')
      str_title = sprintf("%s-%s (Baudry)", str_confusion, str_weight)
    plots[[sprintf("%s-%s", str_confusion, str_weight)]] = p + ggtitle(str_title)
  }
}



pdf(file='ternarys.pdf', width=15, height=20)
grid.newpage()
vp11 <- viewport(width = 1/3, height = 1/4, x = (1/3)/2, y = (1/4)/2)
vp21 <- viewport(width = 1/3, height = 1/4, x = 1/3+(1/3)/2, y = (1/4)/2)
vp31 <- viewport(width = 1/3, height = 1/4, x = 2/3+(1/3)/2, y = (1/4)/2)

vp12 <- viewport(width = 1/3, height = 1/4, x = (1/3)/2, y = 1/4+(1/4)/2)
vp22 <- viewport(width = 1/3, height = 1/4, x = 1/3+(1/3)/2, y = 1/4+(1/4)/2)
vp32 <- viewport(width = 1/3, height = 1/4, x = 2/3+(1/3)/2, y = 1/4+(1/4)/2)

vp13 <- viewport(width = 1/3, height = 1/4, x = (1/3)/2, y = 2/4+(1/4)/2)
vp23 <- viewport(width = 1/3, height = 1/4, x = 1/3+(1/3)/2, y = 2/4+(1/4)/2)
vp33 <- viewport(width = 1/3, height = 1/4, x = 2/3+(1/3)/2, y = 2/4+(1/4)/2)

vp14 <- viewport(width = 1/3, height = 1/4, x = (1/3)/2, y = 3/4+(1/4)/2)
vp24 <- viewport(width = 1/3, height = 1/4, x = 1/3+(1/3)/2, y = 3/4+(1/4)/2)
vp34 <- viewport(width = 1/3, height = 1/4, x = 2/3+(1/3)/2, y = 3/4+(1/4)/2)

# vp15 <- viewport(width = 1/3, height = 1/5, x = (1/3)/2, y = 4/5+(1/5)/2)
# vp25 <- viewport(width = 1/3, height = 1/5, x = 1/3+(1/3)/2, y = 4/5+(1/5)/2)
# vp35 <- viewport(width = 1/3, height = 1/5, x = 2/3+(1/3)/2, y = 4/5+(1/5)/2)

print(plots[[1]], vp = vp11)
print(plots[[2]], vp = vp21)
print(plots[[3]], vp = vp31)

print(plots[[4]], vp = vp12)
print(plots[[5]], vp = vp22)
print(plots[[6]], vp = vp32)

print(plots[[7]], vp = vp13)
print(plots[[8]], vp = vp23)
print(plots[[9]], vp = vp33)

print(plots[[10]], vp = vp14)
print(plots[[11]], vp = vp24)
print(plots[[12]], vp = vp34)

# print(plots[[13]], vp = vp15)
# print(plots[[14]], vp = vp25)
# print(plots[[15]], vp = vp35)

dev.off()
