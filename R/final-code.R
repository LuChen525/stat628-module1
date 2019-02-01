#' This file contains the code used to clean, analyze, and model the data.
#' 
#' The working directory should be the project root folder `stat628-module1`.

# Packages
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
suppressPackageStartupMessages(library(glmnet))
library(leaps)
set.seed(628)

# Open dataset
dat <- suppressMessages(read_csv("data/BodyFat.csv"))

# Root MSE calculator
root.mse <- function(y, yhat) return(sqrt(mean((y - yhat) ^ 2)))

# Siri's equation for calculating body fat percentage from density
siri <- function(density) return(495 / density - 450)

############
# Outliers #
############

# Examine the difference between BODYFAT and values calculated with Siri's eq.
fig1 <- ggplot(dat, aes(DENSITY, BODYFAT)) + geom_point() +
  stat_function(fun = siri, n = 101, color = "blue") +
  annotate("text", 1.065, 5, label = as.numeric(which(
    dat$DENSITY < 1.07 & dat$DENSITY > 1.06 & dat$BODYFAT > 5 & dat$BODYFAT < 10))) +
  annotate("text", 1.099, 15.4, label = as.numeric(which(
    dat$DENSITY < 1.11 & dat$DENSITY > 1.09 & dat$BODYFAT > 15 & dat$BODYFAT < 20))) +
  annotate("text", 1.108, 2, label = as.numeric(which(
    dat$DENSITY > 1.1 & dat$BODYFAT < 5))) +
  annotate("text", 1.067, 19.8, label = as.numeric(which(
    dat$DENSITY < 1.07 & dat$DENSITY > 1.06 & dat$BODYFAT > 18 & dat$BODYFAT < 20))) +
  labs(title = "Figure 1") + theme(plot.title = element_text(hjust = 0.5))
plot(fig1)

# Look at abnomal points to see if they should be deleted
dat[c(48,96,182,76),]

# Drop observation 182 which has zero bodyfat
dat <- dat[dat$IDNO != 182,]

# Look at Cook's distance
dat <- subset(dat, select = -c(DENSITY, IDNO))
plot(lm(BODYFAT ~ ., dat), which = 4, main = "Figure 2")

# Look at abnomal points to see if they should be deleted
dat[c(39,42,86),]

# Drop observation 42
dat <- dat[-42,]
plot(lm(BODYFAT ~ ., dat), which = 4, main="Figure 3")

#############################
# Linear relationship check #
#############################

layout(matrix(seq(1,15),nrow=3))
plot(dat$AGE, dat$BODYFAT)
plot(dat$WEIGHT, dat$BODYFAT)
plot(dat$HEIGHT, dat$BODYFAT)
plot(dat$ADIPOSITY, dat$BODYFAT)
plot(dat$NECK, dat$BODYFAT)
plot(dat$CHEST, dat$BODYFAT)
plot(dat$ABDOMEN, dat$BODYFAT)
plot(dat$HIP, dat$BODYFAT)
plot(dat$THIGH, dat$BODYFAT)
plot(dat$KNEE, dat$BODYFAT)
plot(dat$ANKLE, dat$BODYFAT)
plot(dat$BICEPS, dat$BODYFAT)
plot(dat$FOREARM, dat$BODYFAT)
plot(dat$WRIST, dat$BODYFAT)
layout(1)

######################
# Stepwise selection #
######################

lm.max <- lm(BODYFAT ~ ., data = dat)
lm.min <- lm(BODYFAT ~ 1, data = dat)
scope <- list(lower=lm.min, upper=lm.max)

# AIC
lm1 <- step(object = lm.max, scope = scope, direction = "both",
            trace = 0, k = 2) # K=2->AIC, K=log(n)->BIC
lm2 <- step(object = lm.min, scope = scope, direction = "both",
            trace = 0, k = 2) # K=2->AIC, K=log(n)->BIC

# BIC
lm3 <- step(object = lm.max, scope = scope, direction = "both",
            trace = 0, k = log(dim(dat)[1]))
lm4 <- step(object = lm.min, scope = scope, direction = "both",
            trace = 0, k = log(dim(dat)[1]))

# Found models
cat("AIC stepwise starting with full model results in:")
summary(lm1)$call
cat("\n\nAIC stepwise starting with empty model results in:")
summary(lm2)$call
cat("\n\nBIC stepwise starting with full model results in:")
summary(lm3)$call
cat("\n\nBIC stepwise starting with empty model results in:")
summary(lm4)$call

###################
# Lasso selection #
###################

# Feature matrix
lasso.dat <- as.matrix(subset(dat, select = -BODYFAT))

# Cross-validated selection of lambda
cv.lasso1 <- cv.glmnet(x = lasso.dat, y = dat$BODYFAT, 
                       type.measure = 'mse', alpha = 1)

# Best lambda for each model size
lasso.results <- data.frame(p = cv.lasso1$glmnet.fit$df,
                            explained = cv.lasso1$glmnet.fit$dev.ratio,
                            lambda = cv.lasso1$glmnet.fit$lambda)
print(lasso.results[c(18, 21, 25, 33, 34, 41, 43, 45),])

# Lasso regression using a specific lambda value chosen from above
m1 <- glmnet(lasso.dat, dat$BODYFAT, lambda = 0.314500, alpha = 1)

# Standard error of the above model `m1`
cat("\nRoot MSE for lambda=0.3145: ",
    root.mse(dat$BODYFAT, predict(m1, lasso.dat))) # = 4.132473

#########################
# Best Subset Selection #
#########################

# Using the 6 identified features above, perform best subset selection

# Feature matrix containing WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT
dat.6vars <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT)))

# Best subset selection
best.subset <- regsubsets(x = dat.6vars, y = dat$BODYFAT, nvmax = 4,
                          method = 'exhaustive')
summary(best.subset)$outmat

#' Results of best subset selection for p=1,2,3,4 variables:
#'          WEIGHT ABDOMEN FOREARM WRIST AGE HEIGHT
#' 1  ( 1 ) " "    "X"     " "     " "   " " " "   
#' 2  ( 1 ) "X"    "X"     " "     " "   " " " "   
#' 3  ( 1 ) "X"    "X"     " "     "X"   " " " "   
#' 4  ( 1 ) "X"    "X"     "X"     "X"   " " " "

# Fit an OLS regression for each of these four models
summary(lm(BODYFAT ~ ABDOMEN, dat))$sigma
summary(lm(BODYFAT ~ ABDOMEN + WEIGHT, dat))$sigma
summary(lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST, dat))$sigma
summary(lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, dat))$sigma

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
#' Given the 6 variables above, I performed best subset selection.
#' This resulted in:
#' - The best 4 variables are WEIGHT, ABDOMEN, WRIST, FOREARM
#' - The best 3 variables are WEIGHT, ABDOMEN, WRIST
#' - The best 2 variables are WEIGHT, ABDOMEN
#' - The best 1 variable is ABDOMEN
#' 
#' OLS on WEIGHT, ABDOMEN, WRIST results in 4.057 rootMSE
#' OLS on WEIGHT, ABDOMEN results in 4.11 rootMSE
#' OLS on ABDOMEN results in 4.503 rootMSE
#' 
#' So WEIGHT, ABDOMEN provide the best balance between accuracy and simplicity.

#################
# Model Fitting #
#################

# Final linear model:
bodyfat.model <- lm(dat$BODYFAT ~ dat$ABDOMEN + dat$WEIGHT)
summary(bodyfat.model)

#################
# Rule of Thumb #
#################

# Exact model predictions
bodyfat.model.yhat <- predict(bodyfat.model, dat)

# Simplified rule of thumb predictions
rot.yhat <- 0.91 * dat$ABDOMEN - 0.14 * dat$WEIGHT - 41

# Exact vs. rule of thumb accuracy
root.mse(dat$BODYFAT, bodyfat.model.yhat)
root.mse(dat$BODYFAT, rot.yhat)

###################################
# Model evaluation and diagnostic #
###################################

# Model diagnostic plots
layout(matrix(c(1,2,3,4), nrow = 2))
plot(bodyfat.model)

# Seem that 39th point is an outlier, so we decided to evaluate the robustness
# of our model:
rodata <- dat[-39,]
without <- lm(rodata$BODYFAT ~ rodata$ABDOMEN + rodata$WEIGHT)
summary(without)
summary(bodyfat.model)
plot(without)
plot(bodyfat.model)

#' Trade-off between robustness and precison:
#' - Removing 39th point does make our final model more accurate and this can be 
#'   measured from the residual standard error.
#' - About 0.056 improved. However, we want our model to be robust which means
#'   it can be used to different kinds of men. 
#' - It seems that the coefficients do not change too much with and without 39th
#'   point, which means our model is robust. 
#' So we regard this as our final model and keep the 39th point for robustness.
