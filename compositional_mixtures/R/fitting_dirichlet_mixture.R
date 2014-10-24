load('data/selected-glass-data.RData')
library(plyr)
require(compositions)

alea = function(){
  cc = sample(1:G, nrow(X), replace=TRUE)
  while(length(table(cc)) < G) cc = sample(1:G, nrow(X), replace=TRUE)
  alpha_0 = llply(split(X, cc), function(d){
    fitDirichlet(d)$alpha
  })
  tau_0 = t(apply(X, 1, function(x){
    laply(1:G, function(i) gtools::ddirichlet(x, alpha_0[[i]]) )
  }))
  tau_0 = tau_0/apply(tau_0, 1, sum)
  
  tau = tau_0
  
  as.numeric( apply(t(apply(tau, 1, function(p) rmultinom(1, size=1, prob = p) )), 1, which.max) )
}
set.seed(2)
G = 3
maxlike = 0
steps = 100
repeat{
  steps = steps -1
  if(steps < 0)
    break
  c.next = alea()
  while(length(table(c.next)) < G){
    c.next = alea()
  }
  iter = 0
  maxIter = 100
  repeat{
    c.previous = c.next
    D = split(X, c.previous)
    alpha = llply(D, function(d){
      fitDirichlet(d)$alpha
    })
    post = t(apply(X, 1, function(x){
      laply(1:G, function(i) gtools::ddirichlet(x, alpha[[i]]) )
    }))
    c.next = apply(post, 1, which.max)
    if( length(table(c.next)) < G ) 
      break
    if( sum(c.next != c.previous) == 0 | iter > maxIter)
      break
    iter = iter + 1
  }
  
  pi = apply(post / apply(post, 1, sum), 2, mean)
  like_t = sum(log(post %*% pi))
  
  if(like_t > maxlike){
    print(like_t)
    c.final = c.next
    max_alpha = alpha
    print(paste("iteration", 1000-steps))
    print(ldply(alpha))
    maxlike = like_t
    plot(acomp(X), col=c.next, pch=15, cex=0.7, center=TRUE)
  }
}

partition = as.numeric(c.final)
save(pi, alpha, partition, file='data/dirichlet_mixture_parameters.RData')
