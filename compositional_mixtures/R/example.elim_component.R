set.seed(1)
library(ggtern)
library(Hmisc)
library(plyr)
library(compositions)
library(devtools)

N1 = 10
N2 = 10
SD = 0.25
## Random data generation
S1.comp = data.frame('a' = exp(rnorm(N1, mean = 0, sd=SD)), 
                     'b' = exp(rnorm(N1, mean =-1, sd=SD)),
                     'c' = exp(rnorm(N1, mean =-2, sd=SD)))
S2.comp = data.frame('a' = rep(exp(rnorm(N2/2, mean =-1, sd=SD)), 2), 
                     'b' = rep(exp(rnorm(N2/2, mean = 0, sd=SD)), 2),
                     'c' = exp(c(rnorm(N2/2, mean = -1.5, sd=SD), rnorm(N2/2, mean = 1, sd=SD))))
## Clousuring
S1 = round(S1.comp[,c('a','b','c')] / apply(S1.comp[,c('a','b','c')], 1, sum) *100, 2)
S2 = round(S2.comp[,c('a','b','c')] / apply(S2.comp[,c('a','b','c')], 1, sum) *100, 2)
## Labels
S1$site = 'S1'
S2$site = 'S2'
S1$condition = 'C1'
S2$condition = rep(c('C1', 'C2'), each=N2/2)
X = rbind(S1, S2)
X$interaction = interaction(X$condition, X$site)
## Es guarda el ternary
p1<-ggtern(X, aes(x=a, y=c, z=b, shape=interaction))+
  geom_point(color= '#444444', size=4) + 
  scale_shape_manual(values=c(16, 1,2), name='Condition & Site') +
  theme_classic()

ggsave(p1, file='figures/example_ternary.pdf', width = 5, height = 3.5)

X1<-X[,c('a', 'b', 'c', 'site', 'condition')]
sink(file='tex/example-coda3.tex')

cat(latexTabular(X1,
                 headings = sprintf("\\textbf{%s}", names(X1)),
                 hline = 1,
                 align = 'r r r | c c',
                 helvetica = F,
                 translate = F))
sink()

p2<-ggplot(X, aes(x=a, y=b, col=site, shape=interaction))+geom_point(color='#444444', size=4) +
  scale_shape_manual(values=c(16, 1,2), name='Condition & Site') + theme_classic() 
ggsave(p2, file='figures/example_2component.pdf', width = 5, height = 4)

load_all('~/research/packages/mixpack')

B1 = ldply(ilr_basis(D = 3))
H1 = ilr_coordinates(X[,c('a','b','c')])
X1 = cbind(H1, X[,c('site','condition', 'interaction')])

X.tab<-cbind(X[,c('a', 'b', 'c')], round(H1,3), X[, c('site', 'condition')])
sink(file='tex/example-coda3-ilr3.tex')

headings = sprintf("\\textbf{%s}", names(X.tab))
# headings[4] = "{\\boldmath$\\sqrt{1/2} \\log(a/b)$}"
# headings[5] = "{\\boldmath$\\sqrt{2/3} \\log(\\sqrt{ab}/c)$}"
headings[4] = "{\\boldmath$h_1$}"
headings[5] = "{\\boldmath$h_2$}"
cat(latexTabular(X.tab,
                 headings = headings,
                 hline = 1,
                 align = 'r r r | r r | c c',
                 helvetica = F,
                 translate = F))
sink()

vX = data.frame('v' = X1$coord.1[X1$site == 'S2' & X1$condition == 'C1'])

p3<-ggplot(X1, aes(x=coord.1, y=coord.2, shape=interaction))+geom_point(color='#444444', size=4)+
  scale_shape_manual(values=c(16, 1, 2), name='Condition & Site') + theme_classic() +
  xlab( expression(paste( sqrt(1 / 2),'  ', ln(paste(' ',  a / b, ' ') )) )) +
  ylab( expression(paste( sqrt(2 / 3),'  ', ln(paste(' ',  sqrt(paste(a,  b)) / c,' ') )) )) +
  geom_vline(data=vX, aes(xintercept=v), linetype=3, size=0.4)
ggsave(p3, file='figures/example_ilr3.pdf', width = 5, height = 4)

B1 = ldply(ilr_basis(D = 2))
H1 = ilr_coordinates(X[,c('a','b')])
X1 = cbind(H1, X[,c('site','condition')])

p4<-ggplot(data=X1, aes(x=coord.1, fill=site)) +
  geom_histogram(binwidth=0.15, alpha=1) + 
  geom_histogram(binwidth=0.15, alpha=0, col='black', show_guide=FALSE) + facet_grid(condition~.) + theme_bw() +
  geom_segment(aes(x=coord.1, xend=coord.1, y=-0.2, yend=0.2)) + 
  scale_colour_manual(values=c('#444444', 'black'), name="Site") + scale_fill_manual(values=c('#444444', 'white'), name="Site") + ylab('Count') + 
  xlab( expression(paste( sqrt(1 / 2),'  ', ln(paste(' ',  a / b, ' ') )) ))

ggsave(p4, file='figures/example_ilr2.pdf', width = 6.5, height = 5)

# B1 = t( matrix(clr(ldply( ilr_basis(D = 3) )), ncol=3 ) )
# 
# X_B1 = data.frame( ilr(X[,c('a', 'b', 'c')], V = B1) )
# names(X_B1) = c('coord 1', 'coord 2')
