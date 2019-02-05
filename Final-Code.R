#' This file contains the code used to clean, analyze, and model the data.
#' 
#' The working directory should be the project root folder `stat628-module1`.

# Packages
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
suppressPackageStartupMessages(library(glmnet))
library(leaps)
suppressPackageStartupMessages(library(car))
set.seed(628)

# Open dataset
dat <- suppressMessages(read_csv("data/BodyFat.csv"))
summary(dat[2:17])

# Root MSE calculator
root.mse <- function(y, yhat) {
  return(sqrt(mean((y - yhat) ^ 2)))
}

# Siri's equation for calculating body fat percentage from density
siri <- function(density) {
  return(495 / density - 450)
}

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
dat[c(48, 76, 96, 182),]

# Replace observation 182's bodyfat with 14.72%
dat[182, 2] <- 14.72

# Look at Cook's distance
dat <- subset(dat, select = -c(DENSITY, IDNO))
plot(lm(BODYFAT ~ ., dat), which = 4, main = "Figure 2")
abline(h = 4 / (dim(dat)[1] - 14 - 1), col = "red")

# Look at abnomal points to see if they should be deleted
dat[c(39, 42, 86),]

# Replace observation 42's height with 69.43 inches
dat$HEIGHT[42] <- 69.43
plot(lm(BODYFAT ~ ., dat), which = 4, main = "Figure 3")
abline(h = 4 / (dim(dat)[1] - 14 - 1), col = "red")

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
cat("Results of Lasso regression at the best Lambda value for each model size:\n")
print(lasso.results[c(18, 21, 24, 33, 34, 42, 46, 49),])

# Lasso regression using a specific lambda value chosen from above
m1 <- glmnet(lasso.dat, dat$BODYFAT, lambda = 0.31424241, alpha = 1)

# Standard error of the above model `m1`
cat("\nRoot MSE for lambda=0.314: ",
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

# OLS regressions for p=1,2,3,4 features
cat("p = 1  -->  sigma = ",
    summary(lm(BODYFAT ~ ABDOMEN, dat))$sigma,
    "\np = 2  -->  sigma = ",
    summary(lm(BODYFAT ~ ABDOMEN + WEIGHT, dat))$sigma,
    "\np = 3  -->  sigma = ",
    summary(lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST, dat))$sigma,
    "\np = 4  -->  sigma = ",
    summary(lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, dat))$sigma,
    sep = "")

#################
# Model Fitting #
#################

# Final linear model:
bodyfat.model <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = dat)
round(as.data.frame(coef(bodyfat.model), 1))
summary(bodyfat.model)

round(confint(bodyfat.model), 2)

#################
# Rule of Thumb #
#################

# Exact model predictions
bodyfat.model.yhat <- predict(bodyfat.model, dat)

# Simplified rule of thumb predictions
rot.yhat <- 0.91 * dat$ABDOMEN - 0.14 * dat$WEIGHT - 40

# Exact vs. rule of thumb accuracy
cat('Standard error of exact model:   ',
    root.mse(dat$BODYFAT, bodyfat.model.yhat))
cat('\nStandard error of rule of thumb: ',
    root.mse(dat$BODYFAT, rot.yhat))

round(predict(bodyfat.model, newdata = data.frame(ABDOMEN = 84, WEIGHT = 173),
              interval = "predict"), 2)

####################################
# Model Evaluation and Diagnostics #
####################################

# Model diagnostic plots
layout(matrix(c(1, 2, 3, 4), nrow = 2))
plot(bodyfat.model)
layout(1)
plot(predict(bodyfat.model), rstandard(bodyfat.model), pch = 23, bg = "red",
     cex = 1.2, xlab = "Predicted Body Fat %", ylab = "Standardized Residuals",
     main = "Standardized Residual Plot")
abline(a = 0, b = 0, col = "black", lwd = 3)

# Multicolinearity
cat("VIF values for assessing multicollinearity:\n")
vif(bodyfat.model)

# Outlier or influential points 
pii = hatvalues(bodyfat.model)
cooki = cooks.distance(bodyfat.model)
n = dim(dat)[1]
plot(1:n, cooki, type = "p", pch = 23, bg = "red", cex=1.2,
     xlab = "Index (Each Observation)", ylab = "Cooki or pii",
     main = "Influence Values (Pii and Cooki)")
points(1:n, pii, type = "p", pch = 23, bg = "green", cex = 1.2)
legend("topright", legend = c("pii", "cooki"), pch = c(23,23),
       col = c("green", "red"))

# The 39th point is an outlier, so we decided to evaluate our model's robustness
rodata <- dat[-39,]
without <- lm(rodata$BODYFAT ~ rodata$ABDOMEN + rodata$WEIGHT)
round(as.data.frame(coef(bodyfat.model)), 2)
round(as.data.frame(coef(without)), 2)
layout(matrix(c(1, 2, 3, 4), nrow = 2))
plot(without)
