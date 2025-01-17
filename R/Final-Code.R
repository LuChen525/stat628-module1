#' This file contains the code used to clean, analyze, and model the data.
#' 
#' The working directory should be the project root folder `stat628-module1`.

# Packages
options(warn = -1)
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
suppressPackageStartupMessages(library(glmnet))
library(leaps)
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(reshape2))
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
options(repr.plot.width=3, repr.plot.height=2)
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

# Look at boxplot
data<- read.csv("data/BodyFat.csv", header=T,check.names=F)
data_m <- melt(data,id.vars="IDNO")
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot(aes(fill=factor(variable))) + 
  #  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none") + 
  labs(title = "Figure 2") +
  theme(plot.title = element_text(hjust = 0.5))
p

# Look at abnomal points to see if they should be deleted
dat[c(39, 42),]

# Replace observation 42's height with 69.43 inches
dat$HEIGHT[42] <- 69.43

# Remove IDNO and DENSITY to do regression
dat <- dat[, c(-1, -3)]

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
scope <- list(lower = lm.min, upper = lm.max)

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
cat("Results of Lasso regression at best Lambda value for each model size:\n")
print(lasso.results[c(18, 21, 24, 33, 34, 42, 46),])

# Lasso regression using a specific lambda value chosen from above
m1 <- glmnet(lasso.dat, dat$BODYFAT, lambda = 0.31424241, alpha = 1)

# Standard error of the above model `m1`
cat("\nRoot MSE for lambda=0.314: ",
    root.mse(dat$BODYFAT, predict(m1, lasso.dat))) # = 4.149806

#########################
# Best Subset Selection #
#########################

# Using the 6 identified features above, perform best subset selection

# Feature matrix containing WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT
dat.6vars <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT)))

# Best subset selection
best.subset <- regsubsets(x = dat.6vars, y = dat$BODYFAT, nvmax = 4,
                          method = "exhaustive")
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

#########################
# Consider Interactions #
#########################

# Create a matrix including all second-order interactions of these 12 variables:
# WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT, and their respective inverses
dat.6.vars <- as.matrix(
  subset(dat, select = c(WEIGHT, ABDOMEN, FOREARM, WRIST, AGE, HEIGHT)))
dat.6.vars.inverse <- 1 / dat.6.vars
colnames(dat.6.vars.inverse) <- paste(colnames(dat.6.vars.inverse), "_INVERSE",
                                      sep = "")
dat.interactions <- model.matrix(~ . ^ 2, data = as.data.frame(
  cbind(dat.6.vars, dat.6.vars.inverse)))

# Cross-validated selection of lambda
cv.lasso.int <- cv.glmnet(x = dat.interactions, y = dat$BODYFAT,
                          type.measure = 'mse', alpha = 1)

# Best lambda for each model size
lasso.results.int <- data.frame(p = cv.lasso.int$glmnet.fit$df,
                                explained = cv.lasso.int$glmnet.fit$dev.ratio,
                                lambda = cv.lasso.int$glmnet.fit$lambda)
cat("Results of Lasso regression at best Lambda value for each model size:\n")
print(lasso.results.int[c(2, 3, 12, 15, 20, 22),])

# Lasso regression using a specific lambda value chosen from above
m.int <- glmnet(dat.interactions, dat$BODYFAT, lambda = 1.0870402, alpha = 1)
coef(m.int)

# Standard error of the above model `m.int`
cat("\nRoot MSE for lambda=1.087: ",
    root.mse(dat$BODYFAT, predict(m.int, dat.interactions))) # = 4.17236

###########################################
# Best Subset Selection with Interactions #
###########################################

# Using the 5 identified features above, perform best subset selection
vars.chosen <- c("ABDOMEN_INVERSE",
                 "ABDOMEN:WRIST_INVERSE",
                 "ABDOMEN:HEIGHT_INVERSE",
                 "WRIST:ABDOMEN_INVERSE",
                 "HEIGHT:ABDOMEN_INVERSE")
dat.5.vars <- dat.interactions[, vars.chosen]

# Best subset selection
best.subset <- regsubsets(x = dat.5.vars, y = dat$BODYFAT, nvmax = 4,
                          method = 'exhaustive')

# OLS regression for p=1 interaction
dat.5.vars <- as.data.frame(dat.5.vars)
cat("p = 1  -->  sigma = ",
    summary(lm(dat$BODYFAT ~ `HEIGHT:ABDOMEN_INVERSE`, dat.5.vars))$sigma,
    sep = "")

# This is not a better accuracy than our simple 2-variable linear model

#################
# Model Fitting #
#################

# Final linear model:
bodyfat.model <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = dat)
round(as.data.frame(coef(bodyfat.model)), 2)

# Interpretation
summary(bodyfat.model)

# Confidence intervals
round(confint(bodyfat.model), 2)

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
text(45, y = 0.15, labels = "39")
points(1:n, pii, type = "p", pch = 23, bg = "green", cex = 1.2)
text(45, y = 0.45, labels = "39")
legend("right", legend = c("pii", "cooki"), pch = c(23,23),
       col = c("green", "red"))

####################
# Robustness Tests #
####################

# The 39th point is an outlier, so we decided to evaluate our model's robustness
rodata <- dat[-39,]
without <- lm(rodata$BODYFAT ~ rodata$ABDOMEN + rodata$WEIGHT)

# Use bootstrp sample to fit model and see changes of coefficients
bootdat <- sample(dat, size = dim(dat)[1], replace = T)

# Conbined results
data.frame(with = round(coef(bodyfat.model), 2), without=round(coef(without), 2),
           bootstrap = round(coef(lm(BODYFAT ~ ABDOMEN + WEIGHT,
                                     data = bootdat)), 2))

################################
# Conclusion and example usage #
################################

# Exact model predictions
bodyfat.model.yhat <- predict(bodyfat.model, dat)

# Simplified rule of thumb predictions
rot.yhat <- 0.91 * dat$ABDOMEN - 0.14 * dat$WEIGHT - 40.72

# Exact vs. rule of thumb accuracy
cat('Standard error of exact model:   ',
    root.mse(dat$BODYFAT, bodyfat.model.yhat))
cat('\nStandard error of rule of thumb: ',
    root.mse(dat$BODYFAT, rot.yhat))

# Example usage
round(predict(bodyfat.model, newdata = data.frame(ABDOMEN = 84, WEIGHT = 173),
              interval = "predict"), 2)
