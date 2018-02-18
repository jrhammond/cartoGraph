
##### Load in models

load('./Models/m2RR2.Rdata')
load('./Models/m3RR2.Rdata')
load('./Models/m4RR2.Rdata')
load('./Models/m6RR2.Rdata')


###############################################################################
##
## Generating model results table
##
###############################################################################

texreg(list(m2,m3,m4,m6))

stargazer(
  m2, m3, m4, m6
  , digits = 2
  , selection.equation = T
  # , covariate.labels = c(
  #   'Logged trade', 'IGO overlap', 'Religious dist.', 'Joint democracy', 'Relative capacity'
  # )
  , model.numbers = F
  , dep.var.labels = 'Zero-inflation stage'
  , header = F
)


stargazer(
  m2, m3, m4, m6
  , digits = 2
  , selection.equation = F
  # , covariate.labels = c(
  #   'Logged trade', 'IGO overlap', 'Religious dist.', 'Joint democracy', 'Relative capacity'
  # )
  , model.numbers = F
  , dep.var.labels = 'Number of fatal battles'
  , header = F
)

###############################################################################
##
## Testing out-of-sample performance
##
###############################################################################

##### Predict zeroes
observed_zeros <- abs(test_data$eventflag - 1)

## Model 2: no centrality 
predicted_zeros_m2 <- predict(m2, newdata = test_data, type = 'zero')

## Model 3: degree centrality
predicted_zeros_m3 <- predict(m3, newdata = test_data, type = 'zero')

## Model 2: betweenness centrality
predicted_zeros_m4 <- predict(m4, newdata = test_data, type = 'zero')

## Model 6: betweenness + degree
predicted_zeros_m6 <- predict(m6, newdata = test_data, type = 'zero')

auc(observed_zeros, predicted_zeros_m2)
auc(observed_zeros, predicted_zeros_m3)
auc(observed_zeros, predicted_zeros_m4)
auc(observed_zeros, predicted_zeros_m6)


##### Predict count
observed_test <- test_data$numevents

## Model 2: no centrality 
predicted_m2 <- predict(m2, newdata = test_data, type = 'count')
brier_m2 <- mean(-2*dpois(observed_test,lambda=predicted_m2) + sapply(predicted_m2, function(x){ sum(dpois(1:10,lambda=x)^2) }))

## Model 3: betweenness
predicted_m3 <- predict(m3, newdata = test_data, type = 'count')
brier_m3 <- mean(-2*dpois(observed_test,lambda=predicted_m3) + sapply(predicted_m3, function(x){ sum(dpois(1:1000,lambda=x)^2) }))

## Model 4: degree
predicted_m4 <- predict(m4, newdata = test_data, type = 'count')
brier_m4 <- mean(-2*dpois(observed_test,lambda=predicted_m4) + sapply(predicted_m4, function(x){ sum(dpois(1:1000,lambda=x)^2) }))

## Model 6: betweenness + degree
predicted_m6 <- predict(m6, newdata = test_data, type = 'count')
brier_m6 <- mean(-2*dpois(observed_test,lambda=predicted_m6) + sapply(predicted_m6, function(x){ sum(dpois(1:10,lambda=x)^2) }))

brier_m2; brier_m6

mean(abs(observed_test - predicted_m2))
mean(abs(observed_test - predicted_m6))

mean((observed_test - predicted_m2)^2)
mean((observed_test - predicted_m3)^2)
mean((observed_test - predicted_m4)^2)
mean((observed_test - predicted_m6)^2)

##### Predict overall estimate
observed_test <- test_data$numevents

## Model 2: no centrality 
predicted_m2 <- predict(m2, newdata = test_data, type = 'response')
brier_m2 <- mean(-2*dpois(observed_test,lambda=predicted_m2) + sapply(predicted_m2, function(x){ sum(dpois(1:10,lambda=x)^2) }))

## Model 3: betweenness
predicted_m3 <- predict(m3, newdata = test_data, type = 'response')
brier_m3 <- mean(-2*dpois(observed_test,lambda=predicted_m3) + sapply(predicted_m3, function(x){ sum(dpois(1:10,lambda=x)^2) }))

## Model 4: degree
predicted_m4 <- predict(m4, newdata = test_data, type = 'response')
brier_m4 <- mean(-2*dpois(observed_test,lambda=predicted_m4) + sapply(predicted_m4, function(x){ sum(dpois(1:10,lambda=x)^2) }))

## Model 6: betweenness + degree
predicted_m6 <- predict(m6, newdata = test_data, type = 'response')
brier_m6 <- mean(-2*dpois(observed_test,lambda=predicted_m6) + sapply(predicted_m6, function(x){ sum(dpois(1:10,lambda=x)^2) }))

brier_m2; brier_m3; brier_m4; brier_m6

mean(abs(observed_test - predicted_m2))
mean(abs(observed_test - predicted_m6))

mean((observed_test - predicted_m2)^2)
mean((observed_test - predicted_m6)^2)










## Code from Wikipedia identifying optimal intercept/slope for power-law curve

pwrdist <- function(u,...) {
  # u is vector of event counts, e.g. how many
  # crimes was a given perpetrator charged for by the police
  fx <- table(u)
  i <- as.numeric(names(fx))
  y <- rep(0,max(i))
  y[i] <- fx
  m0 <- glm(y~log(1:max(i)),family=quasipoisson())
  print(summary(m0))
  sub <- paste("s=",round(m0$coef[2],2),"lambda=",sum(u),"/",length(u))
  plot(i,fx,log="xy",xlab="x",sub=sub,ylab="counts",...)
  grid()
  lines(1:max(i),(fitted(m0)),type="b")
  return(m0)
}


files_list <- list.files('./Data/Networks')

#############################
##
##  Mapping Networks
##
#############################

#####
## Loading country networks
##  Example case: Burundi - it's small and easy to grok

load(paste0('./Data/Networks/', files_list[2]))

net_deg <- degree.distribution(out_network)
table(igraph::degree(out_network))

deg_data <- data.table(
  ties = 0:(length(net_deg)-1)
  , freq = net_deg
  , count = 0L
)

deg_data[as.integer(names(table(igraph::degree(out_network))))+1, count := as.integer(table(igraph::degree(out_network)))]

plot(deg_data$freq ~ deg_data$ties, log = 'xy', ylim = c(0.0001, 1))
lines(1/deg_data$ties ~ deg_data$ties, col = 'blue')
lines(1/deg_data$ties^2 ~ deg_data$ties, col = 'red')
lines(1/deg_data$ties^.5 ~ deg_data$ties, col = 'green')


pwrdist(deg_data[count > 0, count])

require(ggplot2)
ggplot(data=deg_data, aes(x=ties, y=freq)) +
  geom_point() + 
  geom_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10()




library(lattice)
library(latticeExtra)
xyplot(deg_data$freq ~ deg_data$ties, type = c('p', 'g'),
       scales = list(x = list(log = 10), y = list(log = 10)),
       xscale.components=xscale.components.log10ticks,
       yscale.components=yscale.components.log10ticks)


lines(log(net_deg, base = 2) ~ log(1:length(net_deg), base = 2))
lines(log(1:length(net_deg), base = 2) ~ log(1:length(net_deg), base = 2))



#############################
##
##  Making figures
##
#############################

#####
## Sample country network
##  Example case: Burundi - it's small and easy to grok

load('./Data/Networks/BI_1-90_network.Rdata')
roads <- readOGR('./Data/Roads', 'BI_1-90_roads')
load('./Data/PopulatedPlaces/BI_1-90_towndata.Rdata')
roads <- spTransform(roads, CRS(proj4string(townsobj)))

plot(roads, lwd = 0.75)
points(townsobj, pch = 16, col = 'grey20', cex = 0.65)
points(townsobj, pch = 16, col = 'grey60', cex = 0.5)


plot(out_network, vertex.size = 1, vertex.label = NA, vertex.color = 'grey', edge.color = 'grey', edge.width = .5)
plot(out_network, vertex.size = 1
     , vertex.label = NA
     , layout = layout.kamada.kawai
     , vertex.color = 'grey'
     , edge.color = 'grey'
     , edge.width = .5)


#####
## Correlation plot

## Generate correlations

corr_data <- cor(out_table[
  , list(
    numevents, between, degree, as.integer(isolate), capital, ppp2000
    , logpop2000, cowgroup, forest, mountain, log_bdist, log_capdist, log_vdist
  )
  ]
)

rownames(corr_data) <- c(
    'No. events', 'Betweenness', 'Degree', 'Isolate', 'Capital', 'Econ. activity'
    , 'Population (log)', 'Ethnic territory', '% Forest', '% Mountain', 'Border dist. (log)'
    , 'Capital dist. (log)', 'Violence dist. (log)'
  )
colnames(corr_data) <- rownames(corr_data)

## Plot correlations
dev.off()
pdf(file = './Plots/corrPlot.pdf', width = 12, height = 12)
corrplot(
  corr_data
  , method = 'number'
  , col = 'black'
)
dev.off()
