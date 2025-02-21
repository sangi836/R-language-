install.packages("glmnet")
# Load necessary libraries
library(ISLR)
library(glmnet)
library(caret)

# Load the College dataset
data("College")

# Convert 'Private' to a numerical binary variable (0 = No, 1 = Yes)
College$Private <- ifelse(College$Private == "Yes", 1, 0)

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets (70% train, 30% test)
trainIndex <- createDataPartition(College$Grad.Rate, p = 0.7, list = FALSE)
train_data <- College[trainIndex, ]
test_data <- College[-trainIndex, ]

# Prepare matrices for glmnet (glmnet requires matrix inputs)
x_train <- model.matrix(Grad.Rate ~ ., train_data)[, -1]  # Remove intercept column
y_train <- train_data$Grad.Rate
x_test <- model.matrix(Grad.Rate ~ ., test_data)[, -1]
y_test <- test_data$Grad.Rate

# ------------------------
# RIDGE REGRESSION
# ------------------------

# Perform cross-validation to find optimal lambda
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, standardize = TRUE)

# Extract optimal lambda values
lambda_min_ridge <- cv_ridge$lambda.min
lambda_1se_ridge <- cv_ridge$lambda.1se

# Plot the cross-validation results
plot(cv_ridge)
title("Ridge Regression Cross-Validation", line = 2.5)

# Fit Ridge Regression model with optimal lambda
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda_min_ridge)

# Extract coefficients
ridge_coefs <- coef(ridge_model)
print("Ridge Regression Coefficients:")
print(ridge_coefs)

# Predict on training and test sets
ridge_train_pred <- predict(ridge_model, s = lambda_min_ridge, newx = x_train)
ridge_test_pred <- predict(ridge_model, s = lambda_min_ridge, newx = x_test)

# Calculate RMSE for Ridge Regression
ridge_train_rmse <- sqrt(mean((y_train - ridge_train_pred)^2))
ridge_test_rmse <- sqrt(mean((y_test - ridge_test_pred)^2))

print(paste("Ridge Regression Train RMSE:", round(ridge_train_rmse, 2)))
print(paste("Ridge Regression Test RMSE:", round(ridge_test_rmse, 2)))

# ------------------------
# LASSO REGRESSION
# ------------------------

# Perform cross-validation for LASSO
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, standardize = TRUE)

# Extract optimal lambda values
lambda_min_lasso <- cv_lasso$lambda.min
lambda_1se_lasso <- cv_lasso$lambda.1se

# Plot the cross-validation results
plot(cv_lasso)
title("LASSO Regression Cross-Validation", line = 2.5)

# Fit LASSO Regression model with optimal lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min_lasso)

# Extract coefficients
lasso_coefs <- coef(lasso_model)
print("LASSO Regression Coefficients:")
print(lasso_coefs)

# Identify coefficients that are reduced to zero
zero_coefs <- rownames(lasso_coefs)[which(lasso_coefs == 0)]
print("Coefficients Shrunk to Zero in LASSO:")
print(zero_coefs)

# Predict on training and test sets
lasso_train_pred <- predict(lasso_model, s = lambda_min_lasso, newx = x_train)
lasso_test_pred <- predict(lasso_model, s = lambda_min_lasso, newx = x_test)

# Calculate RMSE for LASSO Regression
lasso_train_rmse <- sqrt(mean((y_train - lasso_train_pred)^2))
lasso_test_rmse <- sqrt(mean((y_test - lasso_test_pred)^2))

print(paste("LASSO Regression Train RMSE:", round(lasso_train_rmse, 2)))
print(paste("LASSO Regression Test RMSE:", round(lasso_test_rmse, 2)))

# ------------------------
# COMPARISON
# ------------------------

print(paste("Ridge Test RMSE:", round(ridge_test_rmse, 2)))
print(paste("LASSO Test RMSE:", round(lasso_test_rmse, 2)))

if (ridge_test_rmse < lasso_test_rmse) {
  print("Ridge performed better than LASSO.")
} else {
  print("LASSO performed better than Ridge.")
}
