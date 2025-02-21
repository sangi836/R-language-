#SECTION 11-1 

#1 State the hypotheses and identify the claim.

# Load necessary libraries
library(dplyr)

# Load the dataset
baseball_data <- read.csv(file.choose())

# Hypothesis 1: Higher OBP increases the likelihood of making the playoffs
# Perform a logistic regression
model <- glm(Playoffs ~ OBP, data = baseball_data, family = binomial)
summary(model)

# Hypothesis 2: Wins differ between AL and NL leagues
# Perform a t-test
t_test_results <- t.test(W ~ League, data = baseball_data)
t_test_results

# 2 Find the critical value.
# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())

# Define significance level
alpha <- 0.05

# Example 1: Critical value for a t-test (two-tailed)
# Hypothesis: Difference in Wins (W) between AL and NL leagues
t_critical <- qt(1 - alpha/2, df = nrow(data) - 2)  # Two-tailed test
t_critical

# Example 2: Critical value for a chi-square test
# Hypothesis: Independence of League and Playoffs
chi_critical <- qchisq(1 - alpha, df = 1)  # df = degrees of freedom
chi_critical

# Example 3: Critical value for a z-test (one-tailed)

# Hypothesis: Checking proportion of Playoffs
z_critical <- qnorm(1 - alpha)  # One-tailed test
z_critical

# 3 Compute the test value.
# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())


# Example 1: T-test - Test value for difference in means (e.g., Wins by League)
t_test <- t.test(W ~ League, data = data)  # Assuming W (wins) and League are columns
test_value_t <- t_test$statistic
test_value_t

# Example 2: Z-test - Test value for proportions (e.g., Playoffs participation)
# Assuming "Playoffs" is binary (0 = No, 1 = Yes)
prop_table <- prop.table(table(data$Playoffs))
p_hat <- prop_table[2]  # Proportion of teams that made the playoffs
p0 <- 0.5  # Hypothetical proportion
n <- nrow(data)
z_test_value <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n)
z_test_value

# Example 3: Chi-square test - Independence test (e.g., League and Playoffs)
chi_test <- chisq.test(table(data$League, data$Playoffs))
test_value_chi <- chi_test$statistic
test_value_chi

# 4 Make the decision.

# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())

# Example: T-test to compare means of Wins (W) by League
t_test <- t.test(W ~ League, data = data)  # Assuming W (wins) and League exist in your data

# Extract p-value and test statistic
test_statistic <- t_test$statistic
p_value <- t_test$p.value
alpha <- 0.05  # Significance level

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. There is a significant difference."
} else {
  decision <- "Fail to reject the null hypothesis. No significant difference found."
}

# Print results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Decision:", decision, "\n")

# Adjustments for Different Tests:

# Chi-square Test:

chi_test <- chisq.test(table(data$League, data$Playoffs))  # Replace columns appropriately
p_value <- chi_test$p.value

# Z-test:

z_test_value <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n)
critical_value <- qnorm(1 - alpha)
decision <- ifelse(abs(z_test_value) > critical_value, "Reject H0", "Fail to reject H0")

# 5 Summarize the results.

# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())

# Descriptive Statistics Summary
# Summarize numeric columns
summary_stats <- data %>%
  summarise(
    Total_Teams = n(),
    Average_Wins = mean(W, na.rm = TRUE),
    Average_RS = mean(RS, na.rm = TRUE),  # Runs Scored
    Average_RA = mean(RA, na.rm = TRUE),  # Runs Allowed
    Average_OBP = mean(OBP, na.rm = TRUE),  # On-base Percentage
    Average_SLG = mean(SLG, na.rm = TRUE),  # Slugging Percentage
    Playoff_Proportion = mean(Playoffs, na.rm = TRUE)  # Proportion of playoff teams
  )

# Print summary statistics
print("Descriptive Statistics:")
print(summary_stats)

# Inferential Statistics Summary (Example: T-test for Wins by League)
t_test <- t.test(W ~ League, data = data)  # Wins grouped by League
t_test_results <- list(
  Test_Statistic = t_test$statistic,
  P_Value = t_test$p.value,
  Decision = ifelse(t_test$p.value < 0.05, "Significant difference in Wins by League", "No significant difference")
)

# Print inferential results
print("Inferential Statistics:")
print(t_test_results)

# Additional Summaries: Frequency tables (e.g., Playoffs by League)
frequency_table <- table(data$League, data$Playoffs)
print("Frequency Table (Playoffs by League):")
print(frequency_table)

# Visualization (Optional)
# Bar plot for playoff proportions by league
barplot(prop.table(frequency_table, 1), beside = TRUE, col = c("blue", "red"),
        legend = rownames(frequency_table), main = "Playoff Proportions by League",
        xlab = "Playoff Participation", ylab = "Proportion")

# 6 blood type 

# Observed frequencies from the sample
observed <- c(12, 8, 24, 6)

# Expected proportions from the general population
expected_proportions <- c(0.20, 0.28, 0.36, 0.16)

# Total sample size
n <- 50

# Calculate expected frequencies
expected <- expected_proportions * n

# Perform the chi-square goodness-of-fit test
chi_test <- chisq.test(x = observed, p = expected_proportions)

# Print results
print("Chi-Square Test Results:")
print(chi_test)

# Decision based on the significance level (alpha = 0.10)
alpha <- 0.10
if (chi_test$p.value < alpha) {
  decision <- "Reject the null hypothesis. The distribution differs from the general population."
} else {
  decision <- "Fail to reject the null hypothesis. The distribution is the same as the general population."
}

# Print the decision
cat("Decision:", decision, "\n")

# 8 On-time performance by Airlines 

# Observed frequencies from the sample
observed <- c(125, 40, 10, 25)  # On time, Weather delay, National delay, Aircraft arriving late

# Expected proportions from government statistics
expected_proportions <- c(0.708, 0.12, 0.082, 0.09)  # On time, Weather delay, National delay, Late

# Total number of flights in the sample
n <- sum(observed)

# Calculate expected frequencies
expected <- expected_proportions * n

# Perform the chi-square goodness-of-fit test
chi_test <- chisq.test(x = observed, p = expected_proportions)

# Print results
print("Chi-Square Test Results:")
print(chi_test)

# Decision based on the significance level (alpha = 0.05)
alpha <- 0.05
if (chi_test$p.value < alpha) {
  decision <- "Reject the null hypothesis. The results differ significantly from the government's statistics."
} else {
  decision <- "Fail to reject the null hypothesis. The results do not differ significantly from the government's statistics."
}

# Print the decision
cat("Decision:", decision, "\n")

# SECTION 11-2 

# 1 State the hypotheses and identify the claim.

# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())
# Define the null and alternative hypotheses
cat("Null Hypothesis (H0): The mean number of wins is the same for AL and NL leagues.\n")
cat("Alternative Hypothesis (H1): The mean number of wins differs for AL and NL leagues.\n")

# Summarize data for wins by league
summary_by_league <- data %>%
  group_by(League) %>%
  summarise(
    Mean_Wins = mean(W, na.rm = TRUE),
    SD_Wins = sd(W, na.rm = TRUE),
    Count = n()
  )
print("Summary Statistics by League:")
print(summary_by_league)

# Perform a two-sample t-test
t_test <- t.test(W ~ League, data = data)

# Print test results
print("T-Test Results:")
print(t_test)

# Identify the claim
cat("Claim: The mean number of wins differs for AL and NL leagues.\n")

# 2 Find the critical value.

# T-Test Critical Value

# Define the significance level (alpha)
alpha <- 0.05  # Adjust as needed (e.g., 0.01, 0.10)

# Degrees of freedom (adjust based on your test)
# Assuming you have a dataset with two groups (AL and NL) comparing means
df <- 100  # Replace with the correct degrees of freedom

# Calculate the critical t-value (two-tailed)
t_critical <- qt(1 - alpha / 2, df)
cat("Critical t-value (two-tailed):", t_critical, "\n")

# Z-Test Critical Value

# Define the significance level (alpha)
alpha <- 0.05

# Calculate the critical z-value (two-tailed)
z_critical <- qnorm(1 - alpha / 2)
cat("Critical z-value (two-tailed):", z_critical, "\n")

# Chi-Square Test Critical Value

# Define the significance level (alpha)
alpha <- 0.05

# Degrees of freedom (adjust based on the test)
df <- 3  # Replace with the correct degrees of freedom

# Calculate the critical chi-square value
chi_critical <- qchisq(1 - alpha, df)
cat("Critical chi-square value:", chi_critical, "\n")

# F-Test Critical Value 

# Define the significance level (alpha)
alpha <- 0.05

# Degrees of freedom for numerator and denominator
df1 <- 5  # Replace with the numerator degrees of freedom
df2 <- 20  # Replace with the denominator degrees of freedom

# Calculate the critical F-value
f_critical <- qf(1 - alpha, df1, df2)
cat("Critical F-value:", f_critical, "\n")

# 3 Compute the test value.

# T-Test (Two-Sample)

# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())
# Load necessary library
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())
# Perform a two-sample t-test
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with appropriate column names

# Extract the test statistic
test_value_t <- t_test$statistic
cat("T-Test Value:", test_value_t, "\n")


# Perform a two-sample t-test
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with appropriate column names

# Extract the test statistic
test_value_t <- t_test$statistic
cat("T-Test Value:", test_value_t, "\n")

# Chi-Square Test

# Observed frequencies
observed <- c(12, 8, 24, 6)  # Replace with your observed counts

# Expected proportions (if applicable)
expected_proportions <- c(0.20, 0.28, 0.36, 0.16)  # Replace with appropriate proportions

# Calculate expected frequencies
n <- sum(observed)  # Total sample size
expected <- expected_proportions * n

# Perform chi-square test
chi_test <- chisq.test(x = observed, p = expected_proportions)

# Extract the test statistic
test_value_chi <- chi_test$statistic
cat("Chi-Square Test Value:", test_value_chi, "\n")

# Z-Test (Proportion) 

# Define observed and population proportions
p_hat <- 0.60  # Sample proportion
p0 <- 0.50  # Population proportion
n <- 100  # Sample size

# Compute the z-test statistic
z_test_value <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n)
cat("Z-Test Value:", z_test_value, "\n")

# 4 Make the decision. 

# T-Test

# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())

# Perform a two-sample t-test (replace column names with actual ones)
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with relevant columns

# Extract p-value and test statistic
p_value <- t_test$p.value
test_statistic <- t_test$statistic
alpha <- 0.05  # Significance level

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. There is a significant difference."
} else {
  decision <- "Fail to reject the null hypothesis. No significant difference found."
}

# Print results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Decision:", decision, "\n")

# Chi-Square Test 

# Observed frequencies
observed <- c(12, 8, 24, 6)  # Replace with your observed data

# Expected proportions
expected_proportions <- c(0.20, 0.28, 0.36, 0.16)  # Replace with proportions

# Total sample size
n <- sum(observed)

# Expected frequencies
expected <- expected_proportions * n

# Perform chi-square test
chi_test <- chisq.test(x = observed, p = expected_proportions)

# Extract p-value and test statistic
p_value <- chi_test$p.value
test_statistic <- chi_test$statistic
alpha <- 0.05  # Significance level

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. The observed data differs significantly from the expected data."
} else {
  decision <- "Fail to reject the null hypothesis. The observed data does not differ significantly from the expected data."
}

# Print results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Decision:", decision, "\n")

# Z-Test

# Define observed and population proportions
p_hat <- 0.60  # Sample proportion
p0 <- 0.50  # Population proportion
n <- 100  # Sample size

# Compute z-test statistic
z_test_value <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n)
alpha <- 0.05  # Significance level
z_critical <- qnorm(1 - alpha / 2)  # Two-tailed critical value

# Make the decision
if (abs(z_test_value) > z_critical) {
  decision <- "Reject the null hypothesis. The sample proportion differs significantly from the population proportion."
} else {
  decision <- "Fail to reject the null hypothesis. The sample proportion does not differ significantly from the population proportion."
}

# Print results
cat("Z-Test Statistic:", z_test_value, "\n")
cat("Critical Value:", z_critical, "\n")
cat("Decision:", decision, "\n")

# 5 Summarize the results.

# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv(file.choose())

# 1. Descriptive Statistics Summary
# Summarize key numeric variables (e.g., Wins, Runs Scored, On-base Percentage)
descriptive_summary <- data %>%
  summarise(
    Total_Teams = n(),
    Mean_Wins = mean(W, na.rm = TRUE),
    SD_Wins = sd(W, na.rm = TRUE),
    Mean_RS = mean(RS, na.rm = TRUE),  # Runs Scored
    SD_RS = sd(RS, na.rm = TRUE),  # Runs Scored
    Mean_OBP = mean(OBP, na.rm = TRUE),  # On-base Percentage
    SD_OBP = sd(OBP, na.rm = TRUE)  # On-base Percentage
  )

print("Descriptive Statistics Summary:")
print(descriptive_summary)

# 2. Grouped Summary (e.g., by League)
grouped_summary <- data %>%
  group_by(League) %>%
  summarise(
    Mean_Wins = mean(W, na.rm = TRUE),
    SD_Wins = sd(W, na.rm = TRUE),
    Count = n()
  )

print("Grouped Summary by League:")
print(grouped_summary)

# 3. Inferential Statistics
# Example: T-test for Wins by League
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with appropriate column names

# Extract test results
test_statistic <- t_test$statistic
p_value <- t_test$p.value
decision <- ifelse(p_value < 0.05, "Reject the null hypothesis", "Fail to reject the null hypothesis")

print("Inferential Statistics (T-Test Results):")
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Decision:", decision, "\n")

# 4. Frequency Analysis (e.g., Playoff Participation by League)
playoff_frequency <- table(data$League, data$Playoffs)  # Replace with appropriate column names
print("Frequency Table (Playoffs by League):")
print(playoff_frequency)

# 5. Visualization (Optional)
# Bar plot for playoff proportions by league
barplot(prop.table(playoff_frequency, 1), beside = TRUE, col = c("blue", "red"),
        legend = rownames(playoff_frequency), main = "Playoff Proportions by League",
        xlab = "Playoff Participation", ylab = "Proportion")

# 8 ETHNICITY AND MOVIE ADMINSTRATION 

# Create a contingency table for the data
movie_data <- matrix(c(724, 335, 174, 107,  # Data for 2013
                       370, 292, 152, 140), # Data for 2014
                     nrow = 2, byrow = TRUE)  # Rows: Years, Columns: Ethnicities

# Add row and column names
rownames(movie_data) <- c("2013", "2014")
colnames(movie_data) <- c("Caucasian", "Hispanic", "African_American", "Other")

# Print the table
cat("Contingency Table:\n")
print(movie_data)

# Perform the chi-square test
chi_test <- chisq.test(movie_data)

# Print the test results
cat("Chi-Square Test Results:\n")
print(chi_test)

# Interpret the decision
alpha <- 0.05  # Significance level
if (chi_test$p.value < alpha) {
  decision <- "Reject the null hypothesis. Movie attendance is dependent on ethnicity."
} else {
  decision <- "Fail to reject the null hypothesis. Movie attendance is not dependent on ethnicity."
}

# Print the decision
cat("Decision:", decision, "\n")

# 10 WOMEN IN MILITARY 

# Create a contingency table for the data
military_data <- matrix(c(10791, 62491,  # Army: Officers, Enlisted
                          7816, 42750,   # Navy: Officers, Enlisted
                          932, 9525,     # Marine Corps: Officers, Enlisted
                          11819, 54344), # Air Force: Officers, Enlisted
                        nrow = 4, byrow = TRUE)  # Rows: Branches, Columns: Rank

# Add row and column names
rownames(military_data) <- c("Army", "Navy", "Marine Corps", "Air Force")
colnames(military_data) <- c("Officers", "Enlisted")

# Print the contingency table
cat("Contingency Table:\n")
print(military_data)

# Perform the chi-square test
chi_test <- chisq.test(military_data)

# Print the test results
cat("\nChi-Square Test Results:\n")
print(chi_test)

# Decision based on the significance level (alpha = 0.05)
alpha <- 0.05
if (chi_test$p.value < alpha) {
  decision <- "Reject the null hypothesis. There is a relationship between rank and branch of the Armed Forces."
} else {
  decision <- "Fail to reject the null hypothesis. There is no relationship between rank and branch of the Armed Forces."
}

# Print the decision
cat("\nDecision:", decision, "\n")

# 12. Compare models and choose the preferred one
cat("Preferred model is the one with the highest adjusted R-squared\n")

# 12-1 

# 1 State the hypotheses and identify the claim.

# Load necessary library
library(dplyr)

# Load the dataset
baseball_data <- read.csv(file.choose())

# State the hypotheses and claim
cat("Null Hypothesis (H0): The mean number of wins is the same for AL and NL leagues.\n")
cat("Alternative Hypothesis (H1): The mean number of wins differs for AL and NL leagues.\n")
cat("Claim: The mean number of wins differs for AL and NL leagues.\n\n")

# Summarize the data to check league-wise statistics
league_summary <- data %>%
  group_by(League) %>%
  summarise(
    Mean_Wins = mean(W, na.rm = TRUE),
    SD_Wins = sd(W, na.rm = TRUE),
    Count = n()
  )
cat("Summary Statistics by League:\n")
print(league_summary)

# Perform a two-sample t-test
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with appropriate column names

# Print the t-test results
cat("\nT-Test Results:\n")
print(t_test)

# Identify and print the decision
alpha <- 0.05  # Significance level
if (t_test$p.value < alpha) {
  decision <- "Reject the null hypothesis. There is sufficient evidence to support the claim."
} else {
  decision <- "Fail to reject the null hypothesis. There is insufficient evidence to support the claim."
}

cat("\nDecision:", decision, "\n")

# 12-2 
# 2 Find the critical value.

# Critical Value for a T-Test

# Define the significance level (alpha)
alpha <- 0.05  # Adjust as needed

# Degrees of freedom (n - 1 or for two-sample t-test, n1 + n2 - 2)
df <- 100  # Replace with the actual degrees of freedom based on your data

# Calculate the critical t-value for a two-tailed test
t_critical <- qt(1 - alpha / 2, df)
cat("Critical t-value (two-tailed):", t_critical, "\n")

# 2 Critical Value for a Z-Test

# Define the significance level (alpha)
alpha <- 0.05  # Adjust as needed

# Calculate the critical z-value for a two-tailed test
z_critical <- qnorm(1 - alpha / 2)
cat("Critical z-value (two-tailed):", z_critical, "\n")

# 3  Critical Value for a Chi-Square Test

# Define the significance level (alpha)
alpha <- 0.05  # Adjust as needed

# Degrees of freedom (df = (rows - 1) * (columns - 1) for a contingency table)
df <- 3  # Replace with the actual degrees of freedom

# Calculate the critical chi-square value
chi_critical <- qchisq(1 - alpha, df)
cat("Critical chi-square value:", chi_critical, "\n")

# 4 Critical Value for an F-Test 

# Define the significance level (alpha)
alpha <- 0.05  # Adjust as needed

# Degrees of freedom for numerator and denominator
df1 <- 4  # Replace with numerator degrees of freedom
df2 <- 10  # Replace with denominator degrees of freedom

# Calculate the critical F-value
f_critical <- qf(1 - alpha, df1, df2)
cat("Critical F-value:", f_critical, "\n")

# 3 Compute the test value.

# T-Test 

# Load necessary library
library(dplyr)

# Load the dataset
baseball_data <- read.csv(file.choose())


# Perform a two-sample t-test (replace column names accordingly)
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with relevant columns

# Extract the test value
test_value_t <- t_test$statistic
cat("T-Test Value:", test_value_t, "\n")

# 2 Chi-Square Test 

# Create a contingency table (replace with your actual data)
contingency_table <- table(data$League, data$Playoffs)  # Replace "League" and "Playoffs" with relevant columns

# Perform the chi-square test
chi_test <- chisq.test(contingency_table)

# Extract the test value
test_value_chi <- chi_test$statistic
cat("Chi-Square Test Value:", test_value_chi, "\n")

# 3  Z-Test

# Define observed and population proportions
p_hat <- 0.60  # Sample proportion
p0 <- 0.50  # Population proportion
n <- 100  # Sample size

# Compute the z-test value
z_test_value <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n)
cat("Z-Test Value:", z_test_value, "\n")

# 4  F-Test

# Perform an F-test
group1 <- data$OBP[data$League == "AL"]  # Replace "OBP" and "League" with relevant columns and conditions
group2 <- data$OBP[data$League == "NL"]

# Compute the F-test value
f_test_value <- var(group1, na.rm = TRUE) / var(group2, na.rm = TRUE)
cat("F-Test Value:", f_test_value, "\n")

# 4 Make the decision. 

# 1 T-Test 

# Load necessary library
library(dplyr)

# Load the dataset
baseball_data <- read.csv(file.choose())

# Perform a two-sample t-test
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with relevant column names

# Extract the p-value
p_value <- t_test$p.value

# Define the significance level
alpha <- 0.05

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. There is a significant difference."
} else {
  decision <- "Fail to reject the null hypothesis. No significant difference was found."
}

# Print results
cat("T-Test Results:\n")
print(t_test)
cat("\nDecision:", decision, "\n")

# 2 Chi-Square Test 
# Create a contingency table (replace with actual columns)
contingency_table <- table(data$League, data$Playoffs)  # Replace "League" and "Playoffs" with relevant column names

# Perform the chi-square test
chi_test <- chisq.test(contingency_table)

# Extract the p-value
p_value <- chi_test$p.value

# Define the significance level
alpha <- 0.05

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. The variables are dependent."
} else {
  decision <- "Fail to reject the null hypothesis. The variables are independent."
}

# Print results
cat("Chi-Square Test Results:\n")
print(chi_test)
cat("\nDecision:", decision, "\n")

# 3  Z-Test 
# Define observed and population proportions
p_hat <- 0.60  # Sample proportion
p0 <- 0.50  # Population proportion
n <- 100  # Sample size

# Compute the z-test statistic
z_test_value <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n)

# Define the significance level
alpha <- 0.05

# Calculate the critical z-value for a two-tailed test
z_critical <- qnorm(1 - alpha / 2)

# Make the decision
if (abs(z_test_value) > z_critical) {
  decision <- "Reject the null hypothesis. The sample proportion differs significantly."
} else {
  decision <- "Fail to reject the null hypothesis. No significant difference was found."
}

# Print results
cat("Z-Test Results:\n")
cat("Z-Test Statistic:", z_test_value, "\n")
cat("Critical Value:", z_critical, "\n")
cat("\nDecision:", decision, "\n")

# 5 Summarize the results.

# Load necessary library
library(dplyr)

# Load the dataset

baseball_data <- read.csv(file.choose())

# 1. Descriptive Statistics Summary
# Summarize key numeric variables (e.g., Wins, Runs Scored, On-base Percentage)
descriptive_summary <- data %>%
  summarise(
    Total_Teams = n(),
    Mean_Wins = mean(W, na.rm = TRUE),
    SD_Wins = sd(W, na.rm = TRUE),
    Mean_RS = mean(RS, na.rm = TRUE),  # Runs Scored
    SD_RS = sd(RS, na.rm = TRUE),  # Runs Scored
    Mean_OBP = mean(OBP, na.rm = TRUE),  # On-base Percentage
    SD_OBP = sd(OBP, na.rm = TRUE)  # On-base Percentage
  )

cat("Descriptive Statistics Summary:\n")
print(descriptive_summary)

# 2. Grouped Summary (e.g., by League)
grouped_summary <- data %>%
  group_by(League) %>%
  summarise(
    Mean_Wins = mean(W, na.rm = TRUE),
    SD_Wins = sd(W, na.rm = TRUE),
    Count = n()
  )

cat("\nGrouped Summary by League:\n")
print(grouped_summary)

# 3. Inferential Statistics
# Example: T-test for Wins by League
t_test <- t.test(W ~ League, data = data)  # Replace "W" and "League" with relevant column names

# Extract test results
cat("\nInferential Statistics (T-Test Results):\n")
print(t_test)

# 4. Frequency Analysis (e.g., Playoff Participation by League)
playoff_frequency <- table(data$League, data$Playoffs)  # Replace with relevant column names
cat("\nFrequency Table (Playoffs by League):\n")
print(playoff_frequency)

# 5. Visualization (Optional)
# Bar plot for playoff proportions by league
barplot(prop.table(playoff_frequency, 1), beside = TRUE, col = c("blue", "red"),
        legend = rownames(playoff_frequency), main = "Playoff Proportions by League",
        xlab = "Playoff Participation", ylab = "Proportion")

# 8 SODIUM CONTENTS OF FOOD 

# Create the data frame with the given data
sodium_data <- data.frame(
  Sodium = c(270, 130, 230, 180, 80, 70, 200,  # Condiments
             260, 220, 290, 290, 200, 320, 140,  # Cereals
             100, 180, 250, 250, 300, 360, 160),  # Desserts
  Food_Type = rep(c("Condiments", "Cereals", "Desserts"), each = 7)  # Food categories
)

# Perform ANOVA
anova_result <- aov(Sodium ~ Food_Type, data = sodium_data)

# Summary of the ANOVA
anova_summary <- summary(anova_result)

# Print ANOVA results
cat("ANOVA Results:\n")
print(anova_summary)

# Decision based on p-value
alpha <- 0.05  # Significance level
p_value <- anova_summary[[1]]["Pr(>F)"][1]  # Extract p-value

p_value <- anova_summary[[1]]$`Pr(>F)`[1]

} else {
  decision <- "Fail to reject the null hypothesis. There is no significant difference in mean sodium levels among the food types."}

cat("\nDecision:", decision, "\n")

# SECTION 12-2 
 # 10 SALE FOR LEADING COMPANIES 

# Create the data frame with the given data
sales_data <- data.frame(
  Sales = c(578, 320, 264, 249, 237,  # Cereal
            311, 106, 109, 125, 173,  # Chocolate Candy
            261, 185, 302, 689),      # Coffee
  Category = rep(c("Cereal", "Chocolate Candy", "Coffee"), 
                 times = c(5, 5, 4))  # Categories with respective frequencies
)

# Perform ANOVA
anova_result <- aov(Sales ~ Category, data = sales_data)

# Summary of the ANOVA
anova_summary <- summary(anova_result)

# Print ANOVA results
cat("ANOVA Results:\n")
print(anova_summary)

# Extract p-value
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

# Define the significance level
alpha <- 0.01  # Given in the problem

# Decision based on p-value
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. There is a significant difference in the means of sales among the categories."
} else {
  decision <- "Fail to reject the null hypothesis. There is no significant difference in the means of sales among the categories."
}

# Print the decision
cat("\nDecision:", decision, "\n")

# section 12-2 
# 12 

# Create the data frame with the given data
expenditures_data <- data.frame(
  Expenditures = c(4946, 5953, 6202, 7243, 6113,  # Eastern third
                   6149, 7451, 6000, 6479,        # Middle third
                   5282, 8605, 6528, 6911),       # Western third
  Region = rep(c("Eastern", "Middle", "Western"), 
               times = c(5, 4, 4))  # Regions with respective frequencies
)

# Perform ANOVA
anova_result <- aov(Expenditures ~ Region, data = expenditures_data)

# Summary of the ANOVA
anova_summary <- summary(anova_result)

# Print ANOVA results
cat("ANOVA Results:\n")
print(anova_summary)

# Extract p-value
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

# Define the significance level
alpha <- 0.05  # Given in the problem

# Decision based on p-value
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. There is a significant difference in the means of expenditures among the regions."
} else {
  decision <- "Fail to reject the null hypothesis. There is no significant difference in the means of expenditures among the regions."
}

# Print the decision
cat("\nDecision:", decision, "\n")

# SECTION 12-3 

# 1 State the hypotheses.

# ANOVA 
# State the hypotheses
cat("Null Hypothesis (H0): The mean expenditures are equal across all regions.\n")
cat("Alternative Hypothesis (H1): At least one region has a different mean expenditure.\n")

# T-Test 

# State the hypotheses
cat("Null Hypothesis (H0): The mean number of wins is the same for AL and NL leagues.\n")
cat("Alternative Hypothesis (H1): The mean number of wins differs for AL and NL leagues.\n")

#  Chi-Square Test 

# State the hypotheses
cat("Null Hypothesis (H0): The two variables (e.g., League and Playoffs) are independent.\n")
cat("Alternative Hypothesis (H1): The two variables are not independent.\n")

# 2 Find the critical value for each F test. 

# Define the significance level (alpha)
alpha <- 0.05  # Adjust as needed

# Degrees of freedom for numerator (df1) and denominator (df2)
df1 <- 2  # Replace with actual numerator degrees of freedom
df2 <- 15  # Replace with actual denominator degrees of freedom

# Calculate the critical F-value
f_critical <- qf(1 - alpha, df1, df2)

# Print the critical value
cat("Critical F-value at alpha =", alpha, "is:", f_critical, "\n")

# 3 Compute the summary table and find the test value.

# Perform ANOVA with the actual column names
anova_result <- aov(Sales ~ Category, data = data)

# Compute the summary table
anova_summary <- summary(anova_result)

# Print the summary table
cat("ANOVA Summary Table:\n")
print(anova_summary)

# Extract the F-test value (test statistic)
f_value <- anova_summary[[1]]$`F value`[1]

# Print the test value
cat("\nF-Test Value:", f_value, "\n")

# 4 Make the decision.

# Load necessary library
library(dplyr)

# Load the dataset
baseball_data <- read.csv(file.choose())

# Inspect the dataset to find correct column names
str(data)
head(data)

# Replace "Revenue" and "Region" with the actual column names from your dataset
anova_result <- aov(Revenue ~ Region, data = data)  # Replace placeholders with actual column names

# Get the ANOVA summary
anova_summary <- summary(anova_result)

# Extract the p-value
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

# Define the significance level
alpha <- 0.05

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis. There is sufficient evidence to support the alternative hypothesis."
} else {
  decision <- "Fail to reject the null hypothesis. There is insufficient evidence to support the alternative hypothesis."
}

# Print results
cat("P-Value:", p_value, "\n")
cat("Decision:", decision, "\n")

# 5  Summarize the results. (Draw a graph of the cell means if necessary.) 

# Load the dataset
baseball_data <- read.csv(file.choose())
# Check the structure of the dataset
str(data)

# View the first few rows
head(data)
dependent_var <- "Sales"
grouping_var <- "Category"
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
baseball_data <- read.csv(file.choose())
# Inspect the dataset
str(data)
head(data)

# Replace with correct column names
dependent_var <- "Sales"  # Replace with your numerical column name
grouping_var <- "Category"  # Replace with your categorical column name

# Compute descriptive statistics
descriptive_stats <- data %>%
  group_by(.data[[grouping_var]]) %>%
  summarise(
    Mean = mean(.data[[dependent_var]], na.rm = TRUE),
    SD = sd(.data[[dependent_var]], na.rm = TRUE),
    N = n()
  )

# Check if descriptive_stats is created
cat("Descriptive Statistics:\n")
print(descriptive_stats)

# Perform ANOVA
anova_result <- aov(as.formula(paste(dependent_var, grouping_var, sep = " ~ ")), data = data)
anova_summary <- summary(anova_result)

cat("\nANOVA Summary:\n")
print(anova_summary)

# Extract p-value
p_value <- anova_summary[[1]]$`Pr(>F)`[1]
cat("\nP-Value:", p_value, "\n")

# Plot cell means
ggplot(descriptive_stats, aes(x = .data[[grouping_var]], y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Cell Means with Error Bars",
       x = grouping_var, y = paste("Mean", dependent_var)) +
  theme_minimal()

# 12-3 10 INCREASE THE PLANT GROWTH 

# Create the dataset
growth_data <- data.frame(
  Growth = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6),
  Light = rep(c("Grow-light 1", "Grow-light 2"), each = 6),
  Food = rep(c("Plant food A", "Plant food B"), times = 6)
)

# Check the structure of the dataset
str(growth_data)
head(growth_data)

# Perform two-way ANOVA
anova_result <- aov(Growth ~ Light * Food, data = growth_data)

# Summary of the ANOVA
anova_summary <- summary(anova_result)

cat("\nTwo-Way ANOVA Summary:\n")
print(anova_summary)

# Check interaction effect
interaction_p_value <- anova_summary[[1]]$`Pr(>F)`[3]
if (interaction_p_value < 0.05) {
  interaction_decision <- "Reject the null hypothesis. There is a significant interaction between Light and Food."
} else {
  interaction_decision <- "Fail to reject the null hypothesis. There is no significant interaction between Light and Food."
}

cat("\nInteraction Decision:", interaction_decision, "\n")

# Check main effects
light_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
food_p_value <- anova_summary[[1]]$`Pr(>F)`[2]

light_decision <- ifelse(light_p_value < 0.05,
                         "Reject the null hypothesis. There is a significant difference in mean growth with respect to Light.",
                         "Fail to reject the null hypothesis. No significant difference in mean growth with respect to Light.")

food_decision <- ifelse(food_p_value < 0.05,
                        "Reject the null hypothesis. There is a significant difference in mean growth with respect to Food.",
                        "Fail to reject the null hypothesis. No significant difference in mean growth with respect to Food.")

cat("\nLight Decision:", light_decision, "\n")
cat("\nFood Decision:", food_decision, "\n")

# Optional: Visualize the interaction effect
library(ggplot2)
ggplot(growth_data, aes(x = Light, y = Growth, color = Food)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = Food), linetype = "dashed") +
  labs(title = "Interaction Plot: Growth by Light and Food",
       x = "Grow-light",
       y = "Mean Growth (inches)") +
  theme_minimal()












