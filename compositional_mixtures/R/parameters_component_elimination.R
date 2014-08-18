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

M1  = round(mm1@bestResult@parameters@mean, 2)
M2  = round(mm2@bestResult@parameters@mean, 2)
M3  = round(mm3@bestResult@parameters@mean, 2)

V1 = llply(mm1@bestResult@parameters@variance, round, 2)
V2 = llply(mm2@bestResult@parameters@variance, round, 2)
V3 = llply(mm3@bestResult@parameters@variance, round, 2)

library(stringr)

sink('tex/pars1_component_elimination.tex')
cat(sprintf("\\[\n\\mu_1 = \\left(%s\\right), \\;\n\\Sigma_1 = \\left(\n%s\\right)\\]", 
            paste(M1[1,], collapse = ','),
            str_replace_all(latexTabular(V1[[1]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))

cat(sprintf("\\[\n\\mu_2 = \\left(%s\\right), \\;\n\\Sigma_2 = \\left(\n%s\\right)\\]", 
            paste(M1[2,], collapse = ','),
            str_replace_all(latexTabular(V1[[2]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))

cat(sprintf("\\[\n\\mu_3 = \\left(%s\\right), \\;\n\\Sigma_3 = \\left(\n%s\\right) \\]",
            paste(M1[3,], collapse = ','),
            str_replace_all(latexTabular(V1[[3]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))
sink()

sink('tex/pars2_component_elimination.tex')
cat(sprintf("\\[\n\\mu_1 = \\left(%s\\right), \\;\n\\Sigma_1 = \\left(\n%s\\right)\\]", 
            paste(M2[1,], collapse = ','),
            str_replace_all(latexTabular(V2[[1]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))

cat(sprintf("\\[\n\\mu_2 = \\left(%s\\right), \\;\n\\Sigma_2 = \\left(\n%s\\right)\\]", 
            paste(M2[2,], collapse = ','),
            str_replace_all(latexTabular(V2[[2]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))

cat(sprintf("\\[\n\\mu_3 = \\left(%s\\right), \\;\n\\Sigma_3 = \\left(\n%s\\right) \\]",
            paste(M2[3,], collapse = ','),
            str_replace_all(latexTabular(V2[[3]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))
sink()

sink('tex/pars3_component_elimination.tex')
cat(sprintf("\\[\n\\mu_1 = \\left(%s\\right), \\;\n\\Sigma_1 = \\left(\n%s\\right)\\]", 
            paste(M3[1,], collapse = ','),
            str_replace_all(latexTabular(V3[[1]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))

cat(sprintf("\\[\n\\mu_2 = \\left(%s\\right), \\;\n\\Sigma_2 = \\left(\n%s\\right)\\]", 
            paste(M3[2,], collapse = ','),
            str_replace_all(latexTabular(V3[[2]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))

cat(sprintf("\\[\n\\mu_3 = \\left(%s\\right), \\;\n\\Sigma_3 = \\left(\n%s\\right) \\]",
            paste(M3[3,], collapse = ','),
            str_replace_all(latexTabular(V3[[3]], headings = NULL, helvetica = FALSE),
                            'tabular', 'array')))
sink()