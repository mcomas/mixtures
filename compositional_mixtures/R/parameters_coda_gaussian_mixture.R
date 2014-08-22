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
load(file='data/coda_gaussian_mixture.RData')


library(Hmisc)

P  = round(mm@bestResult@parameters@proportions, 2)

M  = round(mm@bestResult@parameters@mean, 2)

R = llply(llply(mm@bestResult@parameters@variance, cov2cor), round, 2)

V = llply(llply(mm@bestResult@parameters@variance, function(v) matrix(diag(v), ncol=1)), round, 3)

library(stringr)

sink('tex/pars_coda_gaussian_mixture.tex')
cat('\\[\n')
cat('\\begin{array}{c@{\\hskip 0.1in}c@{\\hskip 0.1in}c }\n')

cat(sprintf("\\pi_1 = %s, & \\mu_1 = \\left(%s\\right), & \\Sigma_1 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right), \\\\ & &\\\\ \n",
            paste(P[1], collapse = ','),
            paste(M[1,], collapse = ','),
            str_replace_all(latexTabular(R[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

cat(sprintf("\\pi_2 = %s, & \\mu_2 = \\left(%s\\right), & \\Sigma_2 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right), \\\\ & &\\\\ \n",
            paste(P[2], collapse = ','),
            paste(M[2,], collapse = ','),
            str_replace_all(latexTabular(R[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

cat(sprintf("\\pi_3 = %s, & \\mu_3 = \\left(%s\\right) and & \\Sigma_3 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right). \\\\ & &\\\\ \n",
            paste(P[3], collapse = ','),
            paste(M[3,], collapse = ','),
            str_replace_all(latexTabular(R[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
            str_replace_all(latexTabular(V[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

# cat(sprintf("\\pi_1 = %s, & \\pi_2 = %s, & \\pi_3 = %s, \\\\ & &\\\\ \n",
#             paste(P[1], collapse = ','),
#             paste(P[2], collapse = ','),
#             paste(P[3], collapse = ',')))
# 
# cat(sprintf("\\mu_1 = \\left(%s\\right), & \\mu_2 = \\left(%s\\right), & \\mu_3 = \\left(%s\\right), \\\\ & &\\\\ \n",
#             paste(M[1,], collapse = ','),
#             paste(M[2,], collapse = ','),
#             paste(M[3,], collapse = ',')))
# 
# cat(sprintf("\\Sigma_1 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right), & \\Sigma_2 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right) \\text{ and }& \\Sigma_3 = \\left(\n%s\\right) \\cdot \\left(\n%s\\right). \\\\ & &\\\\ \n",
#             str_replace_all(latexTabular(R[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(V[[1]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(R[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(V[[2]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(R[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array'),
#             str_replace_all(latexTabular(V[[3]], headings = NULL, helvetica = FALSE), 'tabular', 'array')))

#cat('\\hline')
cat('\\end{array}\n')
cat('\\]\n')
sink()