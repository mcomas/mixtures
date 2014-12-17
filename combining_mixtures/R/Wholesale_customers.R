data <- read.csv("~/Research/mixtures/combining_mixtures/data/Wholesale_customers_data.csv")

devtools::load_all('../../packages/mixpack')
   
X = data[,3:8]

ggbiplot(clr_coordinates(X),  obs.col = factor(data$Channel), transparency = 1, size = 2)

