library(mvtnorm)
library(plyr)
library(abind)
library(mclust)
library(Rmixmod)
library(grid)
library(ggplot2)
library(ggtern)
library(gridExtra)

#source('R/coda-functions/ggplot_ternary.R')
load('data/selected-glass-data.RData')
devtools::load_all('../../packages/mixpack')
load(file='data/component_elimination.RData')


library(Hmisc)

P1  = round(mm1@bestResult@parameters@proportions, 2)
P2  = round(mm2@bestResult@parameters@proportions, 2)
P3  = round(mm3@bestResult@parameters@proportions, 2)

M1  = round(mm1@bestResult@parameters@mean, 2)
M2  = round(mm2@bestResult@parameters@mean, 2)
M3  = round(mm3@bestResult@parameters@mean, 2)


R1 = llply(llply(mm@bestResult@parameters@variance, cov2cor), round, 2)
V1 = llply(llply(mm@bestResult@parameters@variance, function(v) matrix(diag(v), ncol=1)), round, 3)

#V1 = llply(mm1@bestResult@parameters@variance, round, 2)
V2 = llply(mm2@bestResult@parameters@variance, round, 2)
V3 = llply(mm3@bestResult@parameters@variance, round, 2)

library(stringr)

sink('tex/pars1_component_elimination.tex')
cat('\\[\n')
cat('\\begin{array}{c@{\\hskip 0.1in}c@{\\hskip 0.1in}c }\n')
#cat('\\hline ')

cat(sprintf("\\pi_1 = %s, & \\mu_1 = \\left(%s\\right), & \\Sigma_1 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right), \\\\ & &\\\\ \n",
            paste(P1[1], collapse = ','),
            paste(M1[1,], collapse = ','),
            str_replace_all(latexTabular(R1[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V1[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

cat(sprintf("\\pi_2 = %s, & \\mu_2 = \\left(%s\\right), & \\Sigma_2 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right), \\\\ & &\\\\ \n",
            paste(P1[2], collapse = ','),
            paste(M1[2,], collapse = ','),
            str_replace_all(latexTabular(R1[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V1[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

cat(sprintf("\\pi_3 = %s, & \\mu_3 = \\left(%s\\right) and & \\Sigma_3 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right). \\\\ & &\\\\ \n",
            paste(P1[3], collapse = ','),
            paste(M1[3,], collapse = ','),
            str_replace_all(latexTabular(R1[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V1[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

# cat(sprintf("\\pi_1 = %s, & \\pi_2 = %s, & \\pi_3 = %s, \\\\ & &\\\\ \n",
#             paste(P1[1], collapse = ','),
#             paste(P1[2], collapse = ','),
#             paste(P1[3], collapse = ',')))
# 
# cat(sprintf("\\mu_1 = \\left(%s\\right), & \\mu_2 = \\left(%s\\right), & \\mu_3 = \\left(%s\\right), \\\\ & &\\\\ \n",
#             paste(M1[1,], collapse = ','),
#             paste(M1[2,], collapse = ','),
#             paste(M1[3,], collapse = ',')))
# 
# cat(sprintf("\\Sigma_1 = \\left(\n%s\\right), & \\Sigma_2 = \\left(\n%s\\right) \\text{ and }& \\Sigma_3 = \\left(\n%s\\right). \\\\ & &\\\\ \n",
#             str_replace_all(latexTabular(V1[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(V1[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(V1[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))
#cat('\\hline')
cat('\\end{array}\n')
cat('\\]\n')
sink()

sink('tex/pars2_component_elimination.tex')
cat('\\[\n')
cat('\\begin{array}{|l@{ }l@{ }l |}\n')
cat('\\hline ')
cat(sprintf("\\mu_1 = \\left(%s\\right) & \\mu_2 = \\left(%s\\right) & mu_3 = \\left(%s\\right) \\\\ & &\\\\ \n",
            paste(M2[1,], collapse = ','),
            paste(M2[2,], collapse = ','),
            paste(M2[3,], collapse = ',')))

cat(sprintf("\\Sigma_1 = \\left(\n%s\\right) & \\Sigma_2 = \\left(\n%s\\right) & \\Sigma_3 = \\left(\n%s\\right) \\\\ & &\\\\ \n",  
            str_replace_all(latexTabular(V2[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V2[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V2[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))
cat('\\hline')
cat('\\end{array}\n')
cat('\\]\n')
sink()

sink('tex/pars3_component_elimination.tex')
cat('\\[\n')
cat('\\begin{array}{|l@{ }l@{ }l |}\n')
cat('\\hline ')
cat(sprintf("\\mu_1 = \\left(%s\\right) & \\mu_2 = \\left(%s\\right) & \\mu_3 = \\left(%s\\right) \\\\ & &\\\\ \n", 
            paste(M3[1,], collapse = ','),
            paste(M3[2,], collapse = ','),
            paste(M3[3,], collapse = ',')))

cat(sprintf("\\Sigma_1 = \\left(\n%s\\right) & \\Sigma_2 = \\left(\n%s\\right) & \\Sigma_3 = \\left(\n%s\\right) \\\\ & &\\\\ \n", 
            str_replace_all(latexTabular(V3[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V3[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V3[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))
cat('\\hline')
cat('\\end{array}\n')
cat('\\]\n')
sink()