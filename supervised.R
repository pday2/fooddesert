library("tidyverse")
library("MASS") # stepAIC
library("dplyr")
library("caret")
library("randomForest")
library("lmtest") # Breush-Pagan
library("skedastic") # White test for heteroskedasticity
library("car") # boxCox, Box-Tidwell
library("xgboost")

if (Sys.info()[1] == "Windows") {
  setwd("C:/Users/peter/My Tresors/Documentsacer/KULeuven/CollAnaBigData/fooddesert-Work")
} else {
  setwd("/home/muddy/Tresors/Documentsacer/KULeuven/CollAnaBigData/fooddesert-Work")   
}

grocery <- read.csv('./grocery/grocery.csv')
health <- read.csv('./health/health.csv')
race <- read.csv('./race/race.csv')
income <- read.csv('./income/income_cleaned.csv')
edu <- read.csv('./education/edu.csv')

# aland10 - ZCTA land area, square miles

data <- inner_join(grocery, health, by='zcta')
data <- inner_join(data, race, by='zcta')
data <- inner_join(data, income, by='zcta')
data <- inner_join(data, edu, by='zcta')
# found an na in grocery_per1k - imputed value with median
sum(is.na(data))
which(is.na(data), arr.ind=TRUE)
data$grocery_per1k[is.na(data$grocery_per1k)] <- median(data$grocery_per1k, na.rm = T) 
sum(is.na(data))
# remove zcta, year, Year and geolocation (lat&long) for supervised learning
dat <- data[-c(1,2,8,9)]
# initial linear regressions suggested that some of the age
# ranges are not linearly associated with obesity
dat <- dat[-c(21,22,23,24,25,26,27,28,29,30,31,32,33)]
# and mixed race seems to have problems... multicollinearity?
dat <- dat[-19]
# Removing population stats due to multicollinearity
dat <- dat[-c(1,6,12)]

summary(data$grocery_persqmile)

high <- data %>% filter(grocery_persqmile>50) 
high$zcta

# Train/Validation/Test split
set.seed(33)
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.6,0.4))
train  <- dat[sample, ]
testvalid   <- dat[!sample, ]
validtest <- sample(c(TRUE, FALSE), nrow(testvalid), replace=TRUE, prob=c(0.5,0.5))
valid <- testvalid[validtest, ]
test <- testvalid[!validtest, ]

hist(train$OBESITY)
#pairs(train[c(4,5,6,7,9,11,13,16,22)])
cor(train[c(4,5,6,7,9,11,13,16,22)])

plot(train$grocery_persqmile, train$OBESITY)
abline(lm(train$OBESITY~train$grocery_persqmile))

summary(data$OBESITY)
summary(data$grocery_persqmile)
summary(data$grocery_count)
histogram(data$grocery_count)
data %>% filter(grocery_count>50)
# zcta = 10*** = NYC
# Lawrence, Massachusetts 01841
# Boston, Massachusetts 02128
# Hartford, Connecticut 06106
# NYC and maybe NJ?

### TRANSFORMATION?????????????????????????????????????
#train$OBESITY <- sqrt(train$OBESITY)

fit.full <- lm(OBESITY ~ ., data=train)
fit.intr <- lm(OBESITY ~ 1, data=train)

stepAIC(fit.full, direction = "backward")
# Backward step selection removes:
#   white, DEPRESSION

# Based on F-value
dropterm(fit.full, test = "F")
fit1 <- update(fit.full, ~ . - white)
dropterm(fit1, sorted = TRUE, test = "F")

fit2 <- update(fit1, ~ . - DEPRESSION)
dropterm(fit2, sorted = TRUE, test = "F")

# grocery_per_1k not signif
fit3 <- update(fit2, ~ . - grocery_per1k)
dropterm(fit3, sorted = TRUE, test = "F")


summary(fit3)
boxplot(train$OBESITY)
histogram(train$OBESITY)
# maybe skews left

#### Diagnostics
pred = predict(fit3)
sres = stdres(fit3)
student = rstandard(fit3)
plot(pred, sres, pch=16); abline(h=0)
plot(pred, student, pch=16); abline(h=0)
plot(train$OBESITY, sres, pch=16); abline(h=0)
# residual values increase with obesity, though variance doesn't change much
plot(sres, xlab = "Index", ylab = "Standardized residual", ylim = c(-4.0,4.0))
abline(h = -3.0, lty = 2)
abline(h = 3.0, lty = 2)

boxplot(sres, pch=16)
# that's a lot of potential outliers?

qqnorm(student, pch=16); qqline(student)
# May not be normal at the ends, middle section looks good

# Shapiro-Wilk test for normality
# H0: model errors are normally distributed
# must have 5000 or fewer observations
shapiro.test(sres)


# Breusch-Pagan - test for Homoskedasticity
# H0: residuals are homoskedastic
bptest(fit3, studentize=F)
# BP = 840.07, df = 20, p-value < 2.2e-16
# Reject H0 in favor of H1 - residuals are heteroskedastic

boxcox(fit3, lambda=seq(-2, 2, 0.1), plotit = T)
# lambda ~ 1 => no transformation

# Multicollinearity?
multico = solve(cor(train[, -5]))
summary(multico)
# No values above 10! 
# Multicollinearity fixed by removing population variables

############## WEIGHTED LEAST SQUARES #####################
# Linear Models Chapter 6 slide 21
datafit3 <- train[-c(3,6,10)]
# Fit using OLS
fit <- lm(OBESITY~., data = datafit3)
# Calculate residuals
resid <- residuals(fit)
# regress sq or abs resids on regressors from OLS
# abs(resid) estimates sigma
# resid^2 estimates sigma^2
stdev <- lm(abs(resid)~., data = datafit3[-4])
summary(stdev)
# Calculate weights from estimated Std Dev w=1/s^2
weights <- 1/stdev$fitted^2
# Fit regression with weights
wls <- lm(OBESITY~., data = datafit3, weights=weights)
summary(wls)

# Breusch-Pagan test - p<0.001, still heteroskedastic
bptest(wls, studentize=F)
wstudent = rstandard(wls)
qqnorm(wstudent, pch=16); qqline(wstudent)

# These will still show heteroskedasticity
plot(datafit3$OBESITY,resid(wls))
# With weights, heteroskedasticity should now be gone
plot(datafit3$OBESITY,resid(wls)*sqrt(weights),ylab="Weighted residuals")

# Fitted/Predicted vs residuals
wpred = predict(wls)
wsres = stdres(wls)
wstudent = rstandard(wls)
plot(wpred, wsres, pch=16); abline(h=0)
plot(wpred, wstudent, pch=16); abline(h=0)

skedastic::white_lm(fit3)
skedastic::white_lm(wls)

plot(train$OBESITY~log(train$grocery_persqmile))

####################### XGBOOST #######################

# Obesity is variable 5
X_train <- train[-5]
y_train <- train[5]
X_valid <- valid[-5]
y_valid <- valid[5]
X_test <- test[-5]
y_test <- test[5]

# XGBoost is an ensemble method, similar to Random Forest
# Except not random, tree N+1 is based on the loss from tree N
# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
# XGB Params: https://xgboost.readthedocs.io/en/latest/parameter.html?highlight=objective
xgbtrain = xgb.DMatrix(as.matrix(sapply(X_train, as.numeric)), label=y_train$OBESITY)
xgbvalid = xgb.DMatrix(as.matrix(sapply(X_valid, as.numeric)), label=y_valid$OBESITY)
watchlist <- list(train=xgbtrain, test=xgbvalid)

# depth = 4 and nrounds=10 yielded minimum test-rmse
xgb.fit <- xgb.train(data=xgbtrain, max.depth = 4, eta = 1, nthread = 2, nrounds = 10, 
                   watchlist=watchlist, objective = "reg:squarederror")
# linear boosting - not as good
lin.fit <- xgb.train(data=xgbtrain, booster = "gblinear", nthread = 2, nrounds=10, 
                 watchlist=watchlist, objective = "reg:squarederror")
summary(xgb.fit)

# Plot of variable importance
importance_matrix <- xgb.importance(model = xgb.fit)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
# Gain is the improvement in accuracy brought by a feature to the branches it is on
# Cover measures the relative quantity of observations concerned by a feature
# Frequency is a simpler way to measure the Gain. It just counts the number of
#   times a feature is used in all generated trees
#   !You should not use it (unless you know why you want to use it)!

# View model trees
#xgb.dump(xgb.fit, with_stats = TRUE)
# with depth=5 the tree ends up having 608 nodes/leaves

# For example tree with depth = 2... plotting a 5 level tree is not helpful
# With this many variables, its still not visible
xgb.fit2 <- xgb.train(data=xgbtrain, max.depth = 5, eta = 1, nthread = 2, nrounds = 10, 
                     watchlist=watchlist, objective = "reg:squarederror")
xgb.plot.tree(model = xgb.fit2, trees=0) # just from 0th root node







# Random Forest
# Reeeheeeheeealy Slow!!!
#rf = train(x = X_train, y = y_train$OBESITY, method = 'rf')
#y_pred = predict(rf, newdata = X_test)
















sum(is.na(train$OBESITY))
#Including cross-validation
myclassifier_cv = train(x = X_train, y = y_train$OBESITY, method = "lm", 
                        trControl = trainControl(method='cv', number = 10))
# ERROR...
# Error in na.fail.default(list(OBESITY = c(25.8, 22.1, 20.6, 27, 27.4,  : 
#   missing values in object
myclassifier_cv
