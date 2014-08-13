library(MASS)
data(fgl)

selected = fgl$type %in% c("Con", "Head", "Veh")
X = fgl[selected,c('Ca', 'Si', 'Al')]
X = X / apply(X, 1, sum)
y = factor(fgl$type[selected])

save(X, y, file='data/selected-glass-data.RData')
