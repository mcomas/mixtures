library(mclust)
library(stringr)
library(Hmisc)
data(Baudry_etal_2010_JCGS_examples)

m = Mclust(ex4.1)

Pi = m$parameters$pro
Mu = m$parameters$mean
S = m$parameters$variance$sigma


library(ggplot2)
devtools::load_all('../../packages/mixpack')

cm = expand.grid(X1 = seq(-2.5,12.5, 0.05), X2 = seq(-2.5,10, 0.05))
cm$z = dmixnorm(cm, Pi = Pi, Mu = Mu, S = S)

( p <- ggplot(ex4.1, aes(x=X1, y=X2)) + geom_point() + theme_bw() )


p_parameters = function(pi_i, mu_i, sigma_i){
  cat(sprintf("\\hat{\\pi}_1 = %s, & \\hat{\\m\\mu}_1 = \\left(%s\\right), & \\hat{\\m\\Sigma}_1 = \\left(\n%s\\right), \\\\ & &\\\\ \n",
              paste(pi_i, collapse = ','),
              paste(mu_i, collapse = ','),
              str_replace_all(latexTabular(sigma_i, headings = NULL, helvetica = FALSE), 'tabular', 'array')))
}

p_parameters(pi_i = round(Pi[1], 2), mu_i = round(Mu[,1], 2), sigma_i = round(S[,,1], 2))
p_parameters(pi_i = round(Pi[2], 2), mu_i = round(Mu[,2], 2), sigma_i = round(S[,,2], 2))
p_parameters(pi_i = round(Pi[3], 2), mu_i = round(Mu[,3], 2), sigma_i = round(S[,,3], 2))
p_parameters(pi_i = round(Pi[4], 2), mu_i = round(Mu[,4], 2), sigma_i = round(S[,,4], 2))
p_parameters(pi_i = round(Pi[5], 2), mu_i = round(Mu[,5], 2), sigma_i = round(S[,,5], 2))
p_parameters(pi_i = round(Pi[6], 2), mu_i = round(Mu[,6], 2), sigma_i = round(S[,,6], 2))

