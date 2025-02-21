# Load necessary libraries
library(tidyverse)
library(corrplot)

# Load the Ames Housing dataset
data <- read.csv("C:/Users/abhin/Desktop/Intermediate Analytics/module -1/AmesHousing.csv")


# 1. Perform Exploratory Data Analysis (EDA)
summary(data) # Descriptive statistics
str(data)    # Structure of the dataset

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values[missing_values > 0]

# 2. Impute missing values (replace with mean for numeric variables)
data <- data %>% mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# 3. Create a correlation matrix for numeric variables
numeric_data <- data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7, main = "Correlation Matrix")

# 4. Identify variables with highest, lowest, and approximately 0.5 correlation with SalePrice
sale_price_cor <- cor_matrix["SalePrice", ]

# Variables with highest and lowest correlation
highest_cor <- names(sort(sale_price_cor, decreasing = TRUE))[2]
lowest_cor <- names(sort(sale_price_cor, decreasing = FALSE))[1]

# Variable with correlation closest to 0.5
closest_to_0_5 <- names(sort(abs(sale_price_cor - 0.5)))[1]

# 5. Scatter plots for SalePrice vs identified variables
par(mfrow = c(1, 3))
plot(numeric_data[[highest_cor]], numeric_data$SalePrice, main = paste("Highest Correlation:", highest_cor), xlab = highest_cor, ylab = "SalePrice")
plot(numeric_data[[lowest_cor]], numeric_data$SalePrice, main = paste("Lowest Correlation:", lowest_cor), xlab = lowest_cor, ylab = "SalePrice")
plot(numeric_data[[closest_to_0_5]], numeric_data$SalePrice, main = paste("Correlation ~0.5:", closest_to_0_5), xlab = closest_to_0_5, ylab = "SalePrice")

# 6. Fit a regression model using at least 3 continuous variables
model <- lm(SalePrice ~ Lot.Area + Overall.Qual + Year.Built, data = numeric_data)
summary(model)

# Report the model equation
cat("Model Equation: SalePrice =", coef(model)[1], "+", coef(model)[2], "* Lot.Area +", coef(model)[3], "* Overall.Qual +", coef(model)[4], "* Year.Built\n")

# 7. Plot the regression model diagnostics
par(mfrow = c(2, 2))
plot(model)

# 8. Check for multicollinearity
library(car)
install.packages("car", lib = "your_custom_library_path")
vif_values <- vif(model)
print(vif_values)

# 9. Check for outliers
outliers <- cooks.distance(model) > (4 / nrow(data))
which(outliers)

# 10. Correct issues if any and refit the model
# For simplicity, excluding identified outliers and refitting
corrected_data <- numeric_data[!outliers, ]
model_corrected <- lm(SalePrice ~ Lot.Area + Overall.Qual + Year.Built, data = corrected_data)
summary(model_corrected)

# 11. Use all subsets regression to find the best model
library(leaps)
install.packages("leaps")
subset_model <- regsubsets(SalePrice ~ ., data = numeric_data, nvmax = 5)
subset_summary <- summary(subset_model)

# Best model based on adjusted R-squared
best_model_index <- which.max(subset_summary$adjr2)
best_model <- subset_summary$which[best_model_index, ]
best_model



