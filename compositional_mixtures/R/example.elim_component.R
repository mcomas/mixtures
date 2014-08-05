set.seed(1)
library(ggtern)
N1 = 10
N2 = 10
SD = 0.25
## Random data generation
S1.comp = data.frame('a' = exp(rnorm(N1, mean = 0, sd=SD)), 
                     'b' = exp(rnorm(N1, mean =-1, sd=SD)),
                     'c' = exp(rnorm(N1, mean =-2, sd=SD)))
S2.comp = data.frame('a' = exp(rnorm(N2, mean =-1, sd=SD)), 
                     'b' = exp(rnorm(N2, mean = 0, sd=SD)),
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

## Es guarda el ternary
p1<-ggtern(X, aes(x=a, y=b, z=c, col=site, shape=condition))+geom_point(size=3)+theme_classic() +
  scale_shape_discrete(name='Condition') + scale_color_grey(name="Site")
ggsave(p1, file='figures/example_ternary.pdf', width = 5, height = 3.5)

X1<-X[,c('a', 'b', 'c', 'site', 'condition')]
sink(file='tex/example-coda3.tex')

cat(Hmisc::latexTabular(X1, 
                        headings = sprintf("\\textbf{%s}", names(X1)),
                        hline = 1, 
                        align = 'r r r | c c', 
                        helvetica = F,
                        translate = F))
sink()

p2<-ggplot(X, aes(x=a, y=b, col=site, shape=condition))+geom_point(size=3)+theme_classic() +
  scale_colour_discrete(name="Site") + scale_shape_discrete(name='Condition')
ggsave(p2, file='figures/example_2component.pdf', width = 5, height = 4)


