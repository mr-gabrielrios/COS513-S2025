################################################################################
# Load libraries/data

setwd("~/Documents/Princeton/Spring 2025/COS513/final_project")

library(bsts)
library(ggplot2)

data <- read.csv("ALL_US_monthly_dataset.csv")
data$date <- as.Date(data$time)

################################################################################
# EDA

# Plot net energy load over time
ggplot(data=data, aes(x=date,y=LOAD)) +
  geom_line() +
  xlab("Date") + 
  ylab("Net energy load") + # add units! 
  ggtitle("Net energy load") +
  theme_bw()

# Plot temperature over time
ggplot(data=data, aes(x=date,y=T)) +
  geom_line() +
  xlab("Date") + 
  ylab("Temperature (Kelvin)") + 
  ggtitle("Temperature") +
  theme_bw()

# Plot net energy load against temperature
ggplot(data=data, aes(x=T,y=LOAD)) +
  geom_point() +
  xlab("Temperature (Kelvin)") + 
  ylab("Net energy load") + # add units! 
  ggtitle("Load vs. Temperature") +
  theme_bw()

################################################################################
# Split data into a train, val, and test set (by date)

train <- data[data$date < as.Date("2003-01-01"),]
val <- data[(data$date >= as.Date("2003-01-01") & data$date < as.Date("2006-06-01")),]
test <- data[data$date >= as.Date("2006-06-01"),]

################################################################################
# Define + fit all the models that I want to try/compare

NITER <- 10000
set.seed(123)

# USING DEFAULT PRIORS

# Local level only
ss1 <- AddLocalLevel(list(), train$LOAD)
model1 <- bsts(train$LOAD,
               state.specification = ss1,
               niter = NITER)

# Local level with seasons
ss2 <- AddLocalLevel(list(), train$LOAD)
ss2 <- AddSeasonal(ss2, train$LOAD, nseasons=4, season.duration=3) # assume 4 seasons
model2 <- bsts(train$LOAD,
               state.specification = ss2,
               niter = NITER)

# Local level with seasons and regression on temperature
ss3 <- AddLocalLevel(list(), train$LOAD)
ss3 <- AddSeasonal(ss3, train$LOAD, nseasons=4, season.duration=3) # assume 4 seasons
ss3 <- AddDynamicRegression(ss3, LOAD ~ T, data=train)
model3 <- bsts(train$LOAD,
               state.specification = ss3,
               niter = NITER)

# Local linear trend only
ss4 <- AddLocalLinearTrend(list(), train$LOAD)
model4 <- bsts(train$LOAD,
               state.specification = ss4,
               niter = NITER)

# Local linear trend with seasons
ss5 <- AddLocalLinearTrend(list(), train$LOAD)
ss5 <- AddSeasonal(ss5, train$LOAD, nseasons=4, season.duration=3) # assume 4 seasons
model5 <- bsts(train$LOAD,
               state.specification = ss5,
               niter = NITER)

# Local linear trend with seasons and regression on temperature
ss6 <- AddLocalLinearTrend(list(), train$LOAD)
ss6 <- AddSeasonal(ss6, train$LOAD, nseasons=4, season.duration=3) # assume 4 seasons
ss6 <- AddDynamicRegression(ss6, LOAD ~ T, data=train)
model6 <- bsts(train$LOAD,
               state.specification = ss6,
               niter = NITER)

# Try a semi-local linear trend 
ss7 <- AddSemilocalLinearTrend(list(), train$LOAD)
model7 <- bsts(train$LOAD,
               state.specification = ss7,
               niter=NITER)

# Try including population as a predictor - local linear trend
ss8 <- AddLocalLinearTrend(list(), train$LOAD)
ss8 <- AddSeasonal(ss8, train$LOAD, nseasons=4, season.duration=3) # assume 4 seasons
ss8 <- AddDynamicRegression(ss8, LOAD ~ T + population, data=train)
model8 <- bsts(train$LOAD,
               state.specification = ss8,
               niter = NITER)

# Try including population as a predictor - local level trend
ss9 <- AddLocalLevel(list(), train$LOAD)
ss9 <- AddSeasonal(ss9, train$LOAD, nseasons=4, season.duration=3) # assume 4 seasons
ss9 <- AddDynamicRegression(ss9, LOAD ~ T + population, data=train)
model9 <- bsts(train$LOAD,
               state.specification = ss9,
               niter = NITER)

################################################################################
# Plot the models (visually examine fit/smell check)

lapply(list(model1,model2,model3,model4,model5,model6,model7,model8,model9),
       plot)

# Plot each component separately
lapply(list(model1,model2,model3,model4,model5,model6,model7,model8,model9),
       plot,
       "comp")

################################################################################
# Calculate MSE on the validation set

# Define a function to compute MSE on the validation set
validation_mse <- function(model, newdata){
  burnin <- SuggestBurn(0.2, model)
  if(!is.null(model$dynamic.regression.coefficients)){
    pred <- predict(model,
                    newdata=newdata, 
                    burn=burnin)
  } else{
    pred <- predict(model,
                    horizon=nrow(newdata),
                    burn=burnin)
  }
  
  # plot the prediction!
  plot(pred, plot.original=nrow(train)) 
  lines(c(train$LOAD, newdata$LOAD), col="red", lty="dotted")
  
  # calculate, print, and return the mse
  mse <- mean((pred$mean - newdata$LOAD)^2)
  print(paste("The MSE on the validation set is", format(mse, scientific=TRUE)))
  mse
}

# Calculate validation set MSE for each fitted model
mse.val <- lapply(list(model1,model2,model3,model4,model5,model6,model7,model8,model9),
                  validation_mse,
                  newdata=val)

################################################################################
# Predict on the test set with the best fitting model + the most complex model 
# (model 8 & model 9)

test_mse <- function(model, newdata.val, newdata.test){
  burnin <- SuggestBurn(0.2, model)
  if(!is.null(model$dynamic.regression.coefficients)){
    pred <- predict(model,
                    newdata=rbind(newdata.val, newdata.test), 
                    burn=burnin)
  } else{
    pred <- predict(model,
                    horizon=nrow(newdata.val)+nrow(newdata.test),
                    burn=burnin)
  }
  
  # plot the prediction!
  plot(pred, plot.original=nrow(train),ylim=c(2e6, 6e6),
       xlab="Timepoint", ylab="Net energy load",
       main="Projection") 
  lines(data$LOAD, col="red", lty="dotted")
  
  # calculate, print, and return the mse
  # calculate ONLY for the test portion of the prediction
  mse <- mean((pred$mean[-c(1:nrow(newdata.val))] - newdata.test$LOAD)^2)
  print(paste("The MSE on the test set is", format(mse, scientific=TRUE)))
  mse
}

# Calculate test set MSE for the two models
mse.test <- lapply(list(model8,model9),
                  test_mse,
                  newdata.val=val, newdata.test=test)

# Use the coda R package to check for convergence of the MCMC analysis?

# Make diagonstic plots
AcfDist(residuals(model8))
AcfDist(residuals(model9))

################################################################################
# GARBAGE CODE...troubleshooting

# Tutorial data/code
data(iclaims)
ss <- AddLocalLinearTrend(list(), initial.claims$iclaimsNSA)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52)
model <- bsts(initial.claims$iclaimsNSA,
               state.specification = ss,
               niter = 1000)
plot(model, "comp")

# Troubleshooting seasonal trends...temperature should have a seasonal trend
ss.tmp <- AddLocalLinearTrend(list(), train$T)
ss.tmp <- AddSeasonal(ss.tmp, train$T, nseasons=4,
                      season.duration = 3)
model.tmp <- bsts(train$T,
                  state.specification = ss.tmp,
                  niter = 10000)
plot(model.tmp)
plot(model.tmp,"comp")
plot(1:length(model.tmp$log.likelihood), model.tmp$log.likelihood, "l")
# Diagnostic plots for model fit
resid <- residuals(model.tmp)
AcfDist(resid)
qqdist(resid)
# Predictions
pred.tmp <- predict(model.tmp, horizon=24, burn=SuggestBurn(0.2, model.tmp))


