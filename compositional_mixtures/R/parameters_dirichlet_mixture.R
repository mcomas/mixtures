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
load('data/dirichlet_mixture_parameters.RData')

sink('tex/pars_dirichlet_mixture.tex')
cat('\\[\n')
cat('\\begin{array}{l@{\\hskip 0.1in}l }\n')
#cat('\\hline ')

cat(sprintf("\\hat{\\pi}_1 = %3.2f, & \\hat{\\m\\alpha}_1 = \\left(%s\\right), \\\\ & \\\\ \n",
            pi[1], paste(round(alpha[[1]],1), collapse = ', ')))

cat(sprintf("\\hat{\\pi}_2 = %3.2f, & \\hat{\\m\\alpha}_2 = \\left(%s\\right), \\\\ & \\\\ \n",
            pi[2], paste(round(alpha[[2]],1), collapse = ', ')))

cat(sprintf("\\hat{\\pi}_3 = %3.2f \\text{ and }& \\hat{\\m\\alpha}_3 = \\left(%s\\right). \\\\ & \\\\ \n",
            pi[3], paste(round(alpha[[3]],1), collapse = ', ')))


# cat(sprintf("\\pi_1 = %3.2f, & \\pi_2 = %3.2f, & \\pi_3 = %3.2f, \\\\ & &\\\\ \n",
#             pi[1], pi[2], pi[3]))
# 
# cat(sprintf("\\alpha_1 = \\left(%s\\right), & \\alpha_2 = \\left(%s\\right), & \\alpha_3 = \\left(%s\\right). \\\\ & &\\\\ \n",
#             paste(round(alpha[[1]],1), collapse = ', '),
#             paste(round(alpha[[2]],1), collapse = ', '),
#             paste(round(alpha[[3]],1), collapse = ', ')))

#cat('\\hline')
cat('\\end{array}\n')
cat('\\]\n')
sink()
