#Student Name & Number

# Step 0 - Objective and Contex -------------------------------------------
#MOdel: 
# r˜t = β0 + β1 × Mrk tminRF + β2 × SMB + β3 × HML + β4 × RMW + β5 × CMA + β6 × Mom + ϵ


# Step 1 - Data Collection and Curation  ----------------------------------

#Stock: TSLA

#Done in excel and saved as HOA1 TSLA.scv

#Open the file dialog box to choose the CSV file
HOA1-TSLA <- file.choose()

# Step 2 - Getting Modelling Started In R Studio --------------------------

#Import data in R as a data frame
data.full <- read.csv(HOA1_TSLA)


# Step 3 - Data Exploration: Getting to Know Your DaTA --------------------

summary(data.full)

var(data.full)

cor(data.full[,2:9])

pairs(data.full[,2:9], main="TSLA Trial Group Scatter Plots")

#Question 
#What do you observe?

# Step 4 - Training Set and Test Set --------------------------------------

#Training set contains the first 80% of data.full (first 96 months)
data.train<-data.full[1:96,]

#Test set contains the last 20% of data.full (last 24 months)
data.test<-data.full[97:120,]

# Step 5 - Implementing a Multiple Linear Regression ----------------------

fit <- lm(excret ~ Mkt.RF + SMB + HML + RMW + CMA + Mom, data = data.train)
summary(fit)

# Step 6 - Hypothesis Testing ---------------------------------------------

# Clearly identify:

# Estimated coefficients
coefficients <- summary(fit)$coefficients[,1]
print(coefficients)

# Standard error (se) of estimates for each coefficients
se <- summary(fit)$coefficients[,2]
print(se)

# R-squared
r_squared <- summary(fit)$r.squared
print(r_squared)

# Adjusted R-squared
adj_r_squared <- summary(fit)$adj.r.squared
print(adj_r_squared)

# F-Statistics
f_statistic <- summary(fit)$fstatistic
print(f_statistic)

# Two-tailed test at 5% significance (95% confidence level)
summary(fit)$coef[, "Pr(>|t|)"]

# Question
# What do you conclude for each of these tests? (reject if p-value > 0.05)


# Step 7 - Factor Selection -----------------------------------------------

library(leaps)

# Best Subset
best_subset <- regsubsets(excret ~ Mkt.RF + SMB + HML + RMW + CMA + Mom, data = data.train, nvmax = 6)
bs_summary <- summary(best_subset)
coef(best_subset, 2)
# Comment: For instance, we see that the R2 statistic increases from 32 %, when only one variable is included in the model, to almost 55 %, when all variables are included. As expected, the R2 statistic increases monotonically as morevariables are included"""

# Forward Stepwise
forward_stepwise <- regsubsets(excret ~ Mkt.RF + SMB + HML + RMW + CMA + Mom, data = data.train, nvmax = 6, method = "forward")
fs_summary <- summary(forward_stepwise)
coef(forward_stepwise, 2)
# Comment: For instance, we see that the R2 statistic increases from 32 %, when only one variable is included in the model, to almost 55 %, when all variables are included. As expected, the R2 statistic increases monotonically as morevariables are included"""

# Backward Stepwise
Backward_stepwise <- regsubsets(excret ~ Mkt.RF + SMB + HML + RMW + CMA + Mom, data = data.train, nvmax = 6, method = "backward")
bas_summary <- summary(Backward_stepwise)
coef(Backward_stepwise, 2)
# Comment: For instance, we see that the R2 statistic increases from 32 %, when only one variable is included in the model, to almost 55 %, when all variables are included. As expected, the R2 statistic increases monotonically as morevariables are included"""

# Question
# What do you observe? Answer the following question: 
# 1. What is the optimal model based on best subset selection? consider: adj.r-sqr>BIC>(2) (highest strength, lowest complexity)
# 2. #What is the optimal model based on best subset selection? # consider: adj.r-sqr>BIC>(2) (highest strength, lowest complexity)
# 3. What is the optimal model based on best subset selection? consider: adj.r-sqr>BIC>(2) (highest strength, lowest complexity)
# 4 Is the optimal model the same for all three linear model selection approach? if not, which model is the best?


# Step 8 - Regularization -------------------------------------------------

library(glmnet)
grid <- 10^seq(10, -2, length=100)

# 8.1 Ridge Regression
ridge <- glmnet(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")], data.train[, c("excret")], alpha=0, lambda=grid)

# Plot
library(RColorBrewer)
n_pred <- 6
line_colors <- brewer.pal(8, "Dark2")
label_colors <- line_colors

# Ridge Regression: Plot 
plot(ridge, xvar="lambda", col=line_colors, label=TRUE, label.col=label_colors)
legend("topright", legend=colnames(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")]), col=line_colors, lty=1)

# Ridge Regression: Cross Validation
cv.ridge <- cv.glmnet(as.matrix(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")]), as.matrix(data.train[, c("excret")]), alpha=0, lambda=grid, nfolds=10, type.measure="mse")
plot(cv.ridge)

# Extract the lambda value that gives the minimum cross-validation error
opt_lambda_r <- cv.ridge$lambda.min

# Fit the final Ridge regression model on the full training set
ridge.opt <- glmnet(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")], data.train[, c("excret")], alpha=0, lambda=opt_lambda_r)

coef(ridge.opt,id=opt_lambda_r)

# 8.2 Lasso Regression
lasso <- glmnet(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")], data.train[, c("excret")], alpha=1, lambda=grid)

# Plot
library(RColorBrewer)
n_pred <- 6
line_colors <- brewer.pal(8, "Dark2")
label_colors <- line_colors

# Lasso Regression: Plot
plot(lasso, xvar="lambda", col=line_colors, label=TRUE, label.col=label_colors)
legend("topright", legend=colnames(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")]), col=line_colors, lty=1)

# Lasso Regression: Cross Validation
cv.lasso <- cv.glmnet(as.matrix(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")]), as.matrix(data.train[, c("excret")]), alpha=1, lambda=grid, nfolds=10, type.measure="mse")
plot(cv.lasso)

# Extract the lambda value that gives the minimum cross-validation error
opt_lambda_l <- cv.lasso$lambda.min

# Fit the final Lasso regression model on the full training set
lasso.opt <- glmnet(data.train[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")], data.train[, c("excret")], alpha=1, lambda=opt_lambda_l)

coef(lasso.opt,id=opt_lambda_l)

#Question: 
# What do you observe? Answer the following question: 
# 1. Compare the coefficients of ridge, Lasso and multiple linear regression; 
# 2. Did one (or both) of the regularization set any coefficient exactly to 0?


# Step 9 - Mean Squared Error ---------------------------------------------

# 9.1 MSE of Training Set

# 9.1.1 MSE Multiple Linear Regression
mlr.pred_tr <- predict(fit, newdata=data.train)
mse_mlr_tr <- mean((data.train$excret - mlr.pred_tr)^2)
mse_mlr_tr

# 9.1.2 MSE Factor Selection
# The three factor selection methods give the same model. Best subset is used for the MSE calculation.
coef.opt <- coef(best_subset, 2)
predictors <- names(coef.opt)[-1] # exclude the intercept term
best_subset.opt <- glmnet(data.train[, predictors], data.train$excret, alpha = 0, lambda = 0)
bs.pred_tr <- predict(best_subset.opt, as.matrix(data.train[, predictors]))
mse_bs_tr <- mean((data.train$excret - bs.pred_tr)^2)
mse_bs_tr

# 9.1.3 MSE Ridge Regression
ridge.pred_tr <- predict(ridge.opt, newx = as.matrix(data.train[,4:9]), s = opt_lambda_r)
mse_ridge_tr <- mean((data.train$excret - ridge.pred_tr)^2)
mse_ridge_tr

# 9.1.4 MSE Lasso Regression
lasso.pred_tr <- predict(lasso.opt, newx = as.matrix(data.train[,4:9]), s = opt_lambda_l)
mse_lasso_tr <- mean((data.train$excret - lasso.pred_tr)^2)
mse_lasso_tr

# 9.2 MSE of Test Set

# 9.2.1 MSE Multiple Linear Regression
mlr.pred_te <- predict(fit, newdata = data.test[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom")])
mse_mlr_te <- mean((data.train$excret - mlr.pred_te)^2)
mse_mlr_te

# 9.2.2 MSE Factor Selection
# The three factor selection methods give the same model. Best subset is used for the MSE calculation.
best_subset.opt <- glmnet(data.test[, predictors], data.test$excret, alpha = 0, lambda = 0)
bs.pred_te <- predict(best_subset.opt, as.matrix(data.test[, predictors]))
mse_bs_te <- mean((data.test$excret - bs.pred_te)^2)
mse_bs_te

# 9.2.3 MSE Ridge Regression
ridge.pred_te <- predict(ridge.opt, newx = as.matrix(data.test[,4:9]), s = opt_lambda_r)
mse_ridge_te <- mean((data.test$excret - ridge.pred_te)^2)
mse_ridge_te

# 9.2.4 MSE Lasso Regression
lasso.pred_te <- predict(lasso.opt, newx = as.matrix(data.test[,4:9]), s = opt_lambda_l)
mse_lasso_te <- mean((data.test$excret - lasso.pred_te)^2)
mse_lasso_te

# Question: 
# 1. Compare the training set MSE for these models. 
# the test set MSE for the multiple linear regression, the best model that you got after performing 
# a factor selection, ridge regression, and LASSO;
# 2. Compare the test set MSE for these models.


# Step 10 - Conclusion ----------------------------------------------------

#Which model would you recommend, if any?

#Justify your answer

