# Load necessary libraries
library(ISLR) # For the College dataset
library(caret) # For train-test split and confusion matrix
library(pROC) # For ROC and AUC calculations

# Load the dataset
data(College)

# 1. Exploratory Data Analysis
# Convert Private to a binary variable for logistic regression
College$Private <- ifelse(College$Private == "Yes", 1, 0)

# Summary statistics
summary(College)

# Visualize the data
boxplot(College$Outstate ~ College$Private, main = "Outstate Tuition by Private/Public", 
        xlab = "Private (1: Yes, 0: No)", ylab = "Outstate Tuition")

hist(College$Apps, main = "Histogram of Applications", xlab = "Applications")

# 2. Split the data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(College$Private, p = 0.7, list = FALSE)
trainData <- College[trainIndex, ]
testData <- College[-trainIndex, ]

# 3. Fit the logistic regression model
logit_model <- glm(Private ~ Outstate + F.Undergrad, data = trainData, family = binomial)

# Summarize the model
summary(logit_model)

# 4. Confusion matrix for the training set
trainPred <- ifelse(predict(logit_model, trainData, type = "response") > 0.5, 1, 0)
confMatrixTrain <- confusionMatrix(as.factor(trainPred), as.factor(trainData$Private))

# Print confusion matrix
confMatrixTrain

# Interpret confusion matrix (manual interpretation in the report)

# 5. Report metrics for training set
accuracy_train <- confMatrixTrain$overall["Accuracy"]
precision_train <- confMatrixTrain$byClass["Pos Pred Value"]
recall_train <- confMatrixTrain$byClass["Sensitivity"]
specificity_train <- confMatrixTrain$byClass["Specificity"]

# Print metrics
cat("Training Metrics:\n")
cat("Accuracy:", accuracy_train, "\n")
cat("Precision:", precision_train, "\n")
cat("Recall (Sensitivity):", recall_train, "\n")
cat("Specificity:", specificity_train, "\n")

# 6. Confusion matrix for the test set
testPred <- ifelse(predict(logit_model, testData, type = "response") > 0.5, 1, 0)
confMatrixTest <- confusionMatrix(as.factor(testPred), as.factor(testData$Private))

# Print confusion matrix
confMatrixTest

# 7. Plot and interpret the ROC curve
roc_curve <- roc(testData$Private, predict(logit_model, testData, type = "response"))
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# 8. Calculate and interpret the AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")
