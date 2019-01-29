#' This file contains the code used to clean, analyze, and model the data.

# Packages
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
suppressPackageStartupMessages(library(glmnet))
library(leaps)
set.seed(628)

# Open dataset
dat <- suppressMessages(read_csv('data/BodyFat.csv'))

# Root MSE calculator
root.mse <- function(y, yhat) return(sqrt(mean((y - yhat) ^ 2)))

# Siri's equation for calculating body fat percentage from density
siri <- function(density) return(495 / density - 450)

# Examine the difference between BODYFAT and values calculated with Siri's equation
plot(p <- ggplot(dat, aes(DENSITY, BODYFAT)) + geom_point() +
       stat_function(fun = siri, n = 101, color = 'blue'))

############
# Outliers #
############

# Drop observation 42
dat <- dat[!(dat$IDNO %in% c(42, 182)), ]
dat <- subset(dat, select = -c(DENSITY, IDNO))
plot(lm(BODYFAT ~ ., dat), which = 4)

######################
# Stepwise selection #
######################

lm.max <- lm(BODYFAT ~ ., data = dat)
lm.min <- lm(BODYFAT ~ 1, data = dat)
scope <- list(lower=lm.min, upper=lm.max)

# AIC
lm1 <- step(object = lm.max, scope = scope, direction = 'both',
            trace = 0, k = 2) # K=2->AIC, K=log(n)->BIC
lm2 <- step(object = lm.min, scope = scope, direction = 'both',
            trace = 0, k = 2) # K=2->AIC, K=log(n)->BIC

# BIC
lm3 <- step(object = lm.max, scope = scope, direction = 'both',
            trace = 0, k = log(dim(dat)[1]))
lm4 <- step(object = lm.min, scope = scope, direction = 'both',
            trace = 0, k = log(dim(dat)[1]))

# Found models
summary(lm1) # AIC starting with full model
summary(lm2) # AIC starting with empty model
summary(lm3) # BIC starting with full model
summary(lm4) # BIC starting with empty model

#########################
# Best Subset Selection #
#########################

bss <- leaps(x = subset(dat, select = -BODYFAT), y=dat$BODYFAT)

###################
# Lasso selection #
###################

#
# USING LASSO TO SELECT FROM ALL FEATURES
#

# Feature matrix
lasso.dat <- as.matrix(subset(dat, select = -BODYFAT))

# Cross-validated selection of lambda
cv.lasso1 <- cv.glmnet(x = lasso.dat, y = dat$BODYFAT, 
                      type.measure = 'mse', alpha = 1)

# Results at each lambda value
cv.lasso1$glmnet.fit

# Lasso regression using a specific lambda value chosen from above
m1 <- glmnet(lasso.dat, dat$BODYFAT, lambda = 0.314500, alpha = 1)

# Standard error of the above model `m1`
print(root.mse(dat$BODYFAT, predict(m1, lasso.dat)))

#
# USING LASSO TO FIND BEST LINEAR MODEL FOR AGE, HEIGHT, ABDOMEN, WRIST
#

# Feature matrix with 4 features: AGE, HEIGHT, ABDOMEN, WRIST
lasso.dat.sparse.1 <- as.matrix(
  subset(dat, select = c(AGE, HEIGHT, ABDOMEN, WRIST)))

# CV Lasso using these 4 features
cv.lasso2 <- cv.glmnet(lasso.dat.sparse.1, dat$BODYFAT, type.measure = 'mse',
                       alpha = 1)

# Lasso using the best lambda from the previous CV
m2 <- glmnet(lasso.dat.sparse.1, dat$BODYFAT, lambda = cv.lasso2$lambda.min,
             alpha = 1)

# Standard error of the above model `m2`
print(root.mse(dat$BODYFAT, predict(m2, lasso.dat.sparse.1)))

#
# USING LASSO TO FIND BEST LINEAR MODEL FOR WEIGHT, ABDOMEN, FOREARM, WRIST
#

# Feature matrix with 4 features: WEIGHT, ABDOMEN, FOREARM, WRIST
lasso.dat.sparse.2 <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN, FOREARM, WRIST)))

# CV Lasso using these 4 features
cv.lasso2 <- cv.glmnet(lasso.dat.sparse.2, dat$BODYFAT, type.measure = 'mse',
                       alpha = 1)

# Lasso using the best lambda from the previous CV
m2 <- glmnet(lasso.dat.sparse.2, dat$BODYFAT, lambda = cv.lasso2$lambda.min,
             alpha = 1)

# Standard error of the above model `m2`
print(root.mse(dat$BODYFAT, predict(m2, lasso.dat.sparse.2)))

#########################
# Best Subset Selection #
#########################

# Using the 6 identified features above, perform best subset selection

# Feature matrix containing WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT
dat.6vars <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT)))

# Best subset selection
best.subset <- regsubsets(x = dat.6vars, y = dat$BODYFAT, nvmax = 4, method = 'exhaustive')
summary(best.subset)

#' Selection Algorithm: exhaustive
#'          WEIGHT ABDOMEN FOREARM WRIST AGE HEIGHT
#' 1  ( 1 ) " "    "X"     " "     " "   " " " "   
#' 2  ( 1 ) "X"    "X"     " "     " "   " " " "   
#' 3  ( 1 ) "X"    "X"     " "     "X"   " " " "   
#' 4  ( 1 ) "X"    "X"     "X"     "X"   " " " "

# CV LASSO WITH BEST 3 FEATURES

# Feature matrix with WEIGHT, ABDOMEN, WRIST
lasso.dat.sparse.3 <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN, WRIST)))

# CV Lasso using these 3 features
cv.lasso3 <- cv.glmnet(lasso.dat.sparse.3, dat$BODYFAT, type.measure = 'mse',
                       alpha = 1)

# Lasso using the best lambda from the previous CV
m3 <- glmnet(lasso.dat.sparse.3, dat$BODYFAT, lambda = cv.lasso3$lambda.min,
             alpha = 1)

# Standard error of the above model `m2`
print(root.mse(dat$BODYFAT, predict(m3, lasso.dat.sparse.3)))

# CV LASSO WITH BEST 2 FEATURES

# Feature matrix with WEIGHT, ABDOMEN
lasso.dat.sparse.4 <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN)))

# CV Lasso using these 3 features
cv.lasso4 <- cv.glmnet(lasso.dat.sparse.4, dat$BODYFAT, type.measure = 'mse',
                       alpha = 1)

# Lasso using the best lambda from the previous CV
m4 <- glmnet(lasso.dat.sparse.4, dat$BODYFAT, lambda = cv.lasso4$lambda.min,
             alpha = 1)

# Standard error of the above model `m2`
print(root.mse(dat$BODYFAT, predict(m4, lasso.dat.sparse.4)))

###########
# RESULTS #
###########

#' Stepwise variable selection using AIC resulted in 7-8 variables being chosen.
#' 
#' Stepwise variable selection using BIC resulted in 4 variables being chosen:
#' WEIGHT, ABDOMEN, FOREARM, WRIST
#' 
#' Using Lasso CV to find the highest lambda which restricts the model to 4
#' features results in the following variables being chosen:
#' AGE, HEIGHT, ABDOMEN, WRIST
#' 
#' Now, I run CV Lasso for both of the 4 variable sets.
#' 
#' CV Lasso for WEIGHT, ABDOMEN, FOREARM, WRIST results in 3.971803 rootMSE
#' CV Lasso for AGE, HEIGHT, ABDOMEN, WRIST results in 4.057284 rootMSE
#' 
#' Given the 6 variables above, I performed best subset selection.
#' This resulted in:
#' - The best 3 variables are WEIGHT, ABDOMEN, WRIST
#' - The best 2 variables are WEIGHT, ABDOMEN
#' - The best 1 variable is ABDOMEN
#' 
#' CV Lasso on WEIGHT, ABDOMEN, WRIST results in 4.025157 rootMSE
#' CV Lasso on WEIGHT, ABDOMEN results in 4.085882
#' 
#' OLS on WEIGHT, ABDOMEN, WRIST results in 4.057 rootMSE
#' OLS on WEIGHT, ABDOMEN results in 4.11 rootMSE
#' OLS on ABDOMEN results in 4.503 rootMSE
