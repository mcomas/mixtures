require(ggtern)
require(compositions)
require(grid)

S1 <- matrix(c(0.05,0.0,0.0,
               0.0,0.02,0.0,
               0.0,0.0,0.05), byrow=TRUE, nrow=3)

S2 <- matrix(c(0.05,0.01,0.0,
               0.01,0.02,0.0,
               0.0,0.0,0.05), byrow=TRUE, nrow=3)

S3 <- matrix(c(0.05,0.01,0.0,
               0.01,0.02,0.0,
               0.0,0.0,0.05), byrow=TRUE, nrow=3)

m1 <- c(1,1,3)
m2 <- c(5,3,1)
m3 <- c(1,3,1)

set.seed(1)
df2 = data.frame(rnorm.acomp(100, m1, S1))
df3 = data.frame(rnorm.acomp(100, m2, S2))
df1 = data.frame(rnorm.acomp(100, m3, S3))
df1$group = 'red'; df2$group = 'green'; df3$group = 'blue';

df = rbind(df1,df2,df3)
df$group = factor(df$group, levels=c('red', 'green', 'blue'))
names(df) = c('C', 'B', 'A', 'group')
df = df[sample(1:nrow(df), replace=F),]

p0 <- ggtern(data=df,aes(x=B,y=A,z=C, shape=group, col=group))+geom_point() + 
  scale_color_discrete(guide=FALSE) + scale_shape_discrete(guide=FALSE) + theme_bw() 

p1 <- ggplot(data=df,aes(x=B,y=A, shape=group, col=group))+ geom_point() + 
  scale_color_discrete(guide=FALSE) + scale_shape_discrete(guide=FALSE) + theme_bw()

dfc = df
dfc[,c('A', 'B')] = dfc[,c('A', 'B')] / apply(dfc[,c('A', 'B')], 1, sum)
p2a <- ggplot(data=dfc,aes(x=B,y=A, shape=group, col=group))+ geom_point() + 
  scale_color_discrete(guide=FALSE) + scale_shape_discrete(guide=FALSE) + theme_bw()
p2b <- ggplot(data=dfc,aes(x=A, col=group))+geom_histogram(binwidth = 0.02)+facet_grid(group~.) + ylab(NULL) + 
  scale_color_discrete(guide=FALSE) + scale_shape_discrete(guide=FALSE) + theme_bw() 
#ggplot(data=dfc,aes(x=B, col=group))+geom_histogram(binwidth = 0.02)+facet_grid(group~.)+theme_bw()

pdf(file='figures/subcomposition_coherenceA.pdf', width=10, height=6, pointsize=10)
grid.newpage()
vp0_ <- viewport(width = 0.5, height = 0.8, x = 0.26, y = 0.49)  # the larger map
vp1_ <- viewport(width = 0.4, height = 0.65, x = 0.75, y = 0.5)  # the inset in upper right
print(p0, vp = vp0_)
print(p1, vp = vp1_)
grid.text("Initial dataset", gp = gpar(fontsize = 25, fontface='bold' ), vp = viewport(width = 0.5, height = 0.2, x = 0.25, y = 0.9))
grid.text("Ernie's approach", gp = gpar(fontsize = 25, fontface='bold' ), vp = viewport(width = 0.5, height = 0.2, x = 0.75, y = 0.9))
dev.off()

pdf(file='figures/subcomposition_coherenceB.pdf', width=10, height=6, pointsize=10)
grid.newpage()
vp0_ <- viewport(width = 0.4, height = 0.65, x = 0.25, y = 0.5)  # the larger map
vp2_ <- viewport(width = 0.35, height = 0.65, x = 0.75, y = 0.5)  # the inset in upper right
print(p2b, vp = vp2_)
print(p2a, vp = vp0_)
grid.text("Bert's approach", gp = gpar(fontsize = 25, fontface='bold' ), vp = viewport(width = 0.5, height = 0.2, x = 0.5, y = 0.9))
dev.off()
