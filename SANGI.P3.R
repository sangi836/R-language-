rm(list = ls())  # clears global environment
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
options(scipen = 100) # disables scientific notion for entire R session
options(digits=2) #reduces the output of decimal places in integers
cat("\014")  # clears console

library(pacman)
library(tidyverse)
p_load(tidyverse)

filename <- file.choose()
data <- readRDS(filename)


names(data)
#Reduce my table to feature only relevant columns
#Add "subject_age" for Module 2
data <- data[, c("date", "location", "subject_age", 
                 "subject_race", "subject_sex", "type", 
                 "reason_for_stop", "search_conducted", 
                 "contraband_found", "outcome")]

#change column as factor:
data$reason_for_stop <- factor(data$reason_for_stop)

###Examples of other ways to change column types. these already are these types but it's good to know the functions###
data$date <- as.Date(data$date)
data$subject_race <- factor(data$subject_race)
data$subject_sex <- factor(data$subject_sex)
data$type <- factor(data$type)
data$search_conducted <- as.logical(data$search_conducted)
data$contraband_found <- as.logical(data$contraband_found)
data$outcome <- factor(data$outcome)
###################################################################

# Add new column with day of week
data <- data |> mutate(dayofweek = wday(date, label = TRUE))

# Frequency table of stops by race
table(data$subject_race)
race <- data |> group_by(subject_race) |> summarize(counts = n())

# Cross-tabulation of race and search conducted
table(data$subject_race, data$search_conducted)

#remove Other, Unknown and NA from table
data <- data |> filter(subject_race != 'unknown', 
                       subject_race != 'other', subject_race != 'NA')

# More advanced cross-tabulation
install.packages('gmodels')
library(gmodels)
CrossTable(data$subject_race, data$search_conducted, 
           prop.chisq = FALSE)
##

library(ggplot2) #library for visualizations
# Histogram of stops by day of week
ggplot(data, aes(x = dayofweek)) +
  geom_bar( fill = "red", color = "black") +
  labs(title = "Distribution of Traffic Stops", 
       x = "Day of Week", y = "Frequency")

# Bar plot of stops by race
ggplot(data, aes(x = subject_race, fill = subject_race)) +
  geom_bar() +
  labs(title = "Traffic Stops by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

install.packages("tidyverse")
library(tidyverse) #need to run "summarise". If doesn't work try p_load(tidyverse)
# Calculate the proportion of searches for each race
search_proportions <- data |>
  group_by(subject_race) |>
  summarise(search_rate = mean(search_conducted, na.rm = TRUE))

#install package formattable to use percent function below
install.packages('formattable')
library("formattable") 
# Create the plot
ggplot(search_proportions, aes(x = subject_race, y = search_rate, 
                               fill = subject_race)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=percent(search_rate)), vjust=-0.3, size=3.5)+
  labs(title = "Search Rates by Race", 
       x = "Race", 
       y = "Proportion of Stops Resulting in Search") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

####More interactive style plots#####
install.packages('plotly')
library(plotly)
p <- ggplot(data, aes(x = subject_race, fill = reason_for_stop)) +
  geom_bar(position = "dodge") +
  labs(title = "Reasons for Stop by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p)
########

# Faceted histogram using ggplot2 to compare across multiple classes
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "red", color = "black") +
  facet_wrap(~ subject_race) +
  labs(title = "Distribution of Stop Times by Race",
       x = "Time of Day",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



############MOD 2###################
# Im going to add "subject_age" to the data df and re-run the script.
# This includes the same clean up script we ran including removing NAs

# Load necessary libraries

library(dplyr)
library(ggplot2)
library(psych)

# Clean up the age column
age <- data |> group_by(subject_age) |> summarize(counts = n())

data <- data |> filter(subject_age != 'NA') #remove NAs

data <- data |> filter(subject_age > 15) #remove age less than legal driving age (16 in MA)

# 1. Descriptive Statistics
## For the entire sample
overall_stats <- t(psych::describe(data$subject_age))

## By group ('subject_race')
group_stats <- data %>%
  group_by(subject_race) %>%
  summarise(
    mean_age = mean(subject_age, na.rm = TRUE),
    sd_age = sd(subject_age, na.rm = TRUE),
    min_age = min(subject_age, na.rm = TRUE),
    max_age = max(subject_age, na.rm = TRUE),
    N = n()
  )

# Print statistics in a three-line table format
print(knitr::kable(group_stats, format = "pipe"))

# 2. Visualizations
## Scatter plot of age vs. outcome (using jitter to handle categorical outcome)
png("scatter_plot.png", width = 800, height = 600)
ggplot(data, aes(x = subject_age, y = outcome)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Age vs. Outcome",
       x = "Age", y = "Outcome")
dev.off()

#Review the outcome frequency chart
outcome <- data |> group_by(outcome)|> summarize(counts = n())
#Reduce table to new outcome table with only the two columns needed for the chart
data_outcome <- data[, c("outcome", "subject_age")] |> 
  filter(outcome != 'NA')
#Re-run the scatter plot above but change the "data" to "data_outcome"

## Jitter plot for search conducted by race
png("jitter_plot.png", width = 800, height = 600)
ggplot(data, aes(x = subject_race, y = search_conducted)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Search Conducted by Race",
       x = "Race", y = "Search Conducted")
dev.off()

## Boxplot of age by race to detect outliers
png("boxplot.png", width = 800, height = 600)
boxplot(subject_age ~ subject_race, data = data,
        main = "Age Distribution by Race",
        xlab = "Race", ylab = "Age")
dev.off()

#Need to convert subject_race to character so we can remove from boxplot chart
data_age <- data[, c("subject_age", "subject_race")]
data_age$subject_race <- as.character(data_age$subject_race)
data_age <- data_age |> filter(subject_race != 'other', subject_race != 'unknown')
table(data_age$subject_race)


####Did not review in class but will discuss in Module 3. Feel free to run when you are reading about
#######this in the next ALEKS exam. 
# 3. Central Limit Theorem Demonstration
sample_means <- replicate(1000, mean(sample(data$subject_age[!is.na(data$subject_age)], 30, replace = TRUE)))
hist(sample_means, breaks = 30, main = "Distribution of Sample Means",
     xlab = "Sample Mean Age")

# 4. Confidence Interval Calculation for Age
ci_mean_age <- t.test(data$subject_age)$conf.int
ci_mean_age

####^
# Lower Bound (36.45628):
# This is the lower limit of the confidence interval. 
# It means that, based on the sample data, we are 95% confident that the true 
#  population mean age is at least 36.45628 years.
# Upper Bound (36.48579):
# This is the upper limit of the confidence interval. 
# It means that we are 95% confident that the true population mean age is no 
#  greater than 36.48579 years.
# Confidence Level (0.95):
# The confidence level of 0.95 (or 95%) indicates that if we were to take many 
#  random samples from the population and calculate a confidence interval for each 
#  sample, approximately 95% of those intervals would contain the true population mean.
# Interpretation:
# The confidence interval suggests that the true mean age of individuals stopped 
#  by law enforcement in this dataset is likely between 36.46 years and 36.49 years.
# Since the interval is very narrow (only about 0.03 years wide), this indicates a 
#  high level of precision in estimating the mean age, 
#  which is expected given the large sample size.
####

# Confidence Interval for Proportion of Searches Conducted
ci_prop_search <- prop.test(sum(data$search_conducted == TRUE), nrow(data), correct=FALSE)$conf.int
ci_prop_search

####^
# CI Interpretation:
# The confidence interval suggests that the true proportion of traffic stops where 
#  a search was conducted is likely between 1.67% and 1.70%.
# Since this interval is very narrow, it indicates high precision in estimating the 
#  proportion, which is expected given a large sample size.
# The fact that searches are conducted in only about 1.67%-1.70% of traffic stops 
#  suggests that searches are relatively rare events in this dataset.
####

# 5. Sample Size Determination for Proportion of Searches Conducted
margin_of_error <- 0.05
z_score <- qnorm(0.975)
p <- mean(data$search_conducted == TRUE, na.rm=TRUE)
n <- (z_score^2 * p * (1-p)) / margin_of_error^2

print(paste("Minimum sample size needed:", ceiling(n)))
#Summary:^
#You need at least 26 observations to accurately estimate the proportion of searches 
# conducted during traffic stops with the given level of confidence and precision. 
#This ensures that your study results are statistically valid and reliable when making 
# inferences about the population from which your data is drawn.

#############MOD 3###################
# Load necessary libraries
library(dplyr)

# Load the dataset (assuming it's already cleaned and available as 'data')
# data <- read.csv("ma_statewide_2020_04_01.csv.gz")

# Conduct a one-sample t-test for mean age
## Null Hypothesis (H₀): The mean age of individuals stopped is equal to 35 years.
## Alternative Hypothesis (H₁): The mean age of individuals stopped is not equal to 35 years.

# Perform one-sample t-test
t_test_age <- t.test(data$subject_age, mu = 35, alternative = "two.sided")
?t.test
#t.test(data$subject_age, mu = 35, alternative = "greater") tests if mean age is greater than 35
#t.test(data$subject_age, mu = 35, alternative = "less") tests if mean age is less than 35

# Print the results of the t-test
print(t_test_age)

# Interpretation:
cat("The one-sample t-test compares the sample mean age to a hypothesized population mean of 35 years. 
The null hypothesis assumes that the true mean age is equal to 35 years. 
If the p-value is less than our significance level (usually 0.05), we reject the null hypothesis and 
conclude that the mean age is significantly different from 35 years.\n")

# Check the p-value
if(t_test_age$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis. 
      There is evidence to suggest that the mean age of individuals stopped is significantly different from 35 years.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis. 
      There is no significant evidence to suggest that the mean age differs from 35 years.\n")
}

"Hypotheses
Null Hypothesis (H₀): There is no association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are not more likely to be searched than White drivers.

Alternative Hypothesis (H₁): There is an association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are more likely to be searched than White drivers."


# Create a contingency table for the observed frequencies
# Use frequency from race table:
#   race <- data |> group_by(subject_race) |> summarize(counts = n())
#     Black counts (Searched 8323 Not Searched 333282) | White counts (Searched 34008 Not Searched 2376651)
observed <- matrix(c(8323, 333282, 34008, 2376651), nrow = 2, byrow = TRUE,
                   dimnames = list(Race = c("Black", "White"),
                                   Search_Conducted = c("Yes", "No")))

# Print the observed contingency table
print(observed)

# Perform the Chi-square test for independence
chi_test <- chisq.test(observed)

# Print the results of the Chi-square test
print(chi_test)

# Interpretation based on p-value
if(chi_test$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
}
