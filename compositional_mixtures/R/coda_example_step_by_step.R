

basis = function(D) lapply((D-1):1, function(i) (v <- exp(c(rep(1/sqrt(i*(i+1)), i), -sqrt(i/(i+1)), rep(0, D-i-1)))) / sum(v) )
inner_product = function(x, y, D=length(x)){
  i = rep(1:D, D)
  j = rep(1:D, each=D)
  sum( ifelse(i<j, log(x[i]/x[j]) * log(y[i]/y[j]), 0 ) ) / D
}

D = 4
B = basis(D)

## Checking that B is a basis
inner_product(B[[1]], B[[1]])
inner_product(B[[2]], B[[2]])
inner_product(B[[1]], B[[2]])

