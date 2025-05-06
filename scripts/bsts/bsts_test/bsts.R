# Install the bsts package
install.packages("bsts")

# Set workind directory
setwd("~/Documents/Princeton/Spring 2025/COS513/final_project")

# Load package
library(bsts)

# Read in data
data <- read.csv("ercot_test_dataset.csv")
colnames(data)[5] <- "temp" # change from T to temp so it doesn't get confused for boolean True
train <- data[1:16, ]
test <- data[17:20, ]

# Plot net load over time
plot(train$year, train$LOAD, type="l",
     xlab="year",
     ylab="Net energy load")

# Define model
## Local linear trend only
ss <- AddLocalLinearTrend(list(),train$LOAD)
model1 <- bsts(train$LOAD,
               state.specification = ss,
               family = "gaussian",
               niter = 10000)

## Local linear trend with predictor variables
### Just temperature
model2 <- bsts(LOAD ~ temp,
               state.specification = ss,
               family = "gaussian",
               niter = 10000,
               data = train)

### Temperature & population
model3 <- bsts(LOAD ~ temp + population,
               state.specification = ss,
               family = "gaussian",
               niter = 10000,
               data = train)

# Using local level instead of local linear trend...
## Local level trend with temperature predictor
ss <- AddLocalLevel(list(), train$LOAD)
model1 <- bsts(LOAD ~ temp,
               state.specification = ss,
               family = "gaussian",
               niter = 5000,
               data = train)

## Local level trend with temperature + population predictors
summary(lm(LOAD ~ temp + population, train))
model2 <- bsts(LOAD ~ temp + population,
               state.specification = ss,
               family = "gaussian",
               niter = 1000,
               data = train,
               expected.model.size = 2) # passed to SpikeSlabPrior, expected number of significant regressors

## Local level trend with population predictor
model3 <- bsts(LOAD ~ population,
               state.specification = ss,
               family = "gaussian",
               niter = 5000,
               data = train)

# Local level trend with predictors and time varying coefficients
# Static regression uses a spike and slab prior which leads to sparse inclusion of predictors (many of the sampled coefficients are zero)
# Trying a dynamic regression component in place of static regression since there are only two predictors (want the sampled coefficients to be mostly non-zero)
ss <- AddLocalLevel(list(), train$LOAD)
ss <- AddDynamicRegression(ss, train$LOAD ~ train$temp + train$population)
model4 <- bsts(train$LOAD,
               state.specification = ss,
               niter = 5000)

ss <- AddLocalLevel(list(), train$LOAD)
ss <- AddDynamicRegression(ss, train$LOAD ~ train$temp)
model5 <- bsts(train$LOAD,
               state.specification = ss,
               niter = 5000)

ss <- AddLocalLevel(list(), train$LOAD)
ss <- AddDynamicRegression(ss, train$LOAD ~ train$population)
model6 <- bsts(train$LOAD,
               state.specification = ss,
               niter = 5000)


# Check 'trace' for stationarity
plot(1:length(model1$log.likelihood), model1$log.likelihood, "l")
plot(1:length(model2$log.likelihood), model2$log.likelihood, "l")
plot(1:length(model3$log.likelihood), model3$log.likelihood, "l")

# Plot
plot(model1)
plot(model1, "components")

plot(model2)
plot(model2, "components")
plot(model2, "coef")

plot(model3)
plot(model3, "components")
plot(model3, "coef")

plot(model4)
plot(model4, "components") # population + temp

plot(model5, "components", burn=100) # temp only

plot(model6, "components") # population only

# Predict and compare to true values
SuggestBurn(0.1, model1)
pred1 <- predict(model1, horizon=4, newdata=test, burn=54)
plot(pred1, plot.original = 16) # forecasted values
lines(data$LOAD, col="red", lty="dotted") # true values

SuggestBurn(0.1, model2)
pred2 <- predict(model2, horizon=4, newdata=test, burn=66)
plot(pred2, plot.original = 16) # forecasted values
lines(data$LOAD, col="red", lty="dotted") # true values

pred3 <- predict(model3, horizon=4, newdata=test)
plot(pred3, plot.original = 16) # forecasted values
lines(data$LOAD, col="red", lty="dotted") # true values

pred4 <- predict(model4, newdata=cbind(test$temp, test$population))
plot(pred4, plot.original = 16) # forecasted values
lines(data$LOAD, col="red", lty="dotted") # true values

pred5 <- predict(model5, newdata=test$temp)
plot(pred5, plot.original = 16) # forecasted values
lines(data$LOAD, col="red", lty="dotted") # true values
