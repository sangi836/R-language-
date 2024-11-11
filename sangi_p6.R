#question 1
# Given values
n <- 7   # number of games
k <- 5   # number of wins
p <- 0.65 # probability of winning a game

# Calculate the probability of exactly 5 wins
probability <- dbinom(k, n, p)

# Output the result
probability
#question 2
# Load the dplyr library for tibble (if you don't have it, you need to install it with install.packages("dplyr"))
library(dplyr)

# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game

# Create a data frame with possible number of wins (0 to 7) and the corresponding probabilities
outcomes <- tibble(
  wins = 0:n,                             # Possible wins (0 to 7)
  probability = dbinom(0:n, n, p)         # Binomial probability for each outcome
)

# View the resulting data frame
outcomes
#question 3
# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game

# Calculate the probability of winning fewer than 5 games (i.e., 0, 1, 2, 3, or 4 wins)
prob3_result <- pbinom(4, n, p)

# Output the result
prob3_result
#question 4
# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game

# Calculate the probability of winning between 3 and 5 games (inclusive)
prob4_result <- sum(dbinom(3:5, n, p))

# Output the result
prob4_result
#question 5
# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game

# Calculate the probability of winning more than 4 games
prob5_result <- 1 - pbinom(4, n, p)

# Output the result
prob5_result
#question 6
# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game

# Calculate the expected value of the number of wins
prob6_result <- n * p

# Output the result
prob6_result
#question 7
# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game

# Calculate the theoretical variance of the number of wins
prob7_result <- n * p * (1 - p)

# Output the result
prob7_result
#question 8 
# Set the seed for reproducibility
set.seed(10)

# Given values
n <- 7        # number of games
p <- 0.65     # probability of winning a game
num_simulations <- 1000  # number of random values to generate

# Generate 1,000 random values for the number of wins
random_wins <- rbinom(num_simulations, n, p)

# Output the first few random values
head(random_wins)
#question 9
# Calculate the sample mean of the 1,000 random values
prob9_result <- mean(random_wins)

# Output the sample mean
prob9_result
#question 10
# Calculate the sample variance of the 1,000 random values
prob10_result <- var(random_wins)

# Output the sample variance
prob10_result
#question 11
# Given values
lambda <- 4  # average rate of calls per hour
k <- 6       # number of calls

# Calculate the probability of receiving exactly 6 calls
prob11_result <- dpois(k, lambda)

# Output the result
prob11_result
#question 12
# Given values
lambda_per_hour <- 5  # average rate of calls per hour
hours <- 8             # number of hours
lambda_total <- lambda_per_hour * hours  # total average for 8 hours

# Calculate the probability of receiving 40 or fewer calls in 8 hours
prob12_result <- ppois(40, lambda_total)

# Output the result
prob12_result
#question 13
# Given values
lambda_per_hour <- 5  # average rate of calls per employee per hour
num_employees <- 5     # number of employees
hours <- 8             # number of hours
quota <- 275           # call quota

# Calculate the total average rate of calls for all employees
lambda_total <- num_employees * hours * lambda_per_hour

# Calculate the probability of meeting the quota of 275 or more calls
prob13_result <- 1 - ppois(quota - 1, lambda_total)

# Output the result
prob13_result
#question 14
# Given values
lambda_per_hour <- 5  # average rate of calls per employee per hour
num_employees <- 4     # number of remaining employees (1 sick)
hours <- 8             # number of hours
quota <- 275           # call quota

# Calculate the total average rate of calls for the remaining employees
lambda_total <- num_employees * hours * lambda_per_hour

# Calculate the probability of meeting the quota of 275 or more calls
prob14_result <- 1 - ppois(quota - 1, lambda_total)

# Output the result
prob14_result
#question 15
# Given values
lambda_per_hour <- 5  # average rate of calls per employee per hour
hours <- 8            # number of hours

# Calculate the total average number of calls for a single employee during an 8-hour shift
lambda_total <- lambda_per_hour * hours

# Calculate the 90th percentile (i.e., top 10%) of the Poisson distribution
prob15_result <- qpois(0.90, lambda_total)

# Output the result
prob15_result
#question 16
# Set the seed for reproducibility
set.seed(15)

# Given values
lambda_per_hour <- 5  # average rate of calls per employee per hour
hours <- 8            # number of hours

# Calculate the total average number of calls for a single employee during an 8-hour shift
lambda_total <- lambda_per_hour * hours

# Generate 1,000 random values for the number of calls
random_calls <- rpois(1000, lambda_total)

# Output the first few random values
head(random_calls)
#question 17
# Calculate the sample mean of the 1,000 random values
prob17_result <- mean(random_calls)

# Output the sample mean
prob17_result
#question 18
# Calculate the sample variance of the 1,000 random values
prob18_result <- var(random_calls)

# Output the sample variance
prob18_result
#question 19
# Given values
mean_lifespan <- 2000    # mean life span in hours
sd_lifespan <- 100       # standard deviation in hours
lower_bound <- 1800      # lower bound of the range
upper_bound <- 2200      # upper bound of the range

# Calculate the cumulative probabilities
prob19_result <- pnorm(upper_bound, mean_lifespan, sd_lifespan) - pnorm(lower_bound, mean_lifespan, sd_lifespan)

# Convert to percentage
prob19_result <- prob19_result * 100

# Output the result
prob19_result
#question 20
# Given values
mean_lifespan <- 2000    # mean life span in hours
sd_lifespan <- 100       # standard deviation in hours
threshold <- 2500        # lifespan threshold in hours

# Calculate the probability of having a lifespan greater than 2,500 hours
prob20_result <- 1 - pnorm(threshold, mean_lifespan, sd_lifespan)

# Convert to percentage
prob20_result <- prob20_result * 100

# Output the result
prob20_result
#question 21
# Given values
mean_lifespan <- 2000    # mean life span in hours
sd_lifespan <- 100       # standard deviation in hours
percentile <- 0.10       # bottom 10% corresponds to the 10th percentile

# Calculate the 10th percentile (bottom 10% of life spans)
prob21_result <- qnorm(percentile, mean_lifespan, sd_lifespan)

# Round up to the nearest integer
prob21_result <- ceiling(prob21_result)

# Output the result
prob21_result
#question 22
# Set the seed for reproducibility
set.seed(25)

# Given values
mean_lifespan <- 2000    # mean life span in hours
sd_lifespan <- 100       # standard deviation in hours
n <- 10000               # number of random values to generate

# Generate 10,000 random values for the life spans of light bulbs
light_bulb_lifespans <- rnorm(n, mean_lifespan, sd_lifespan)

# Output the first few random values
head(light_bulb_lifespans)
#question 23
# Calculate the population mean of the 10,000 random values
prob23_result <- mean(light_bulb_lifespans)

# Output the population mean
prob23_result
#question 24
# Calculate the population standard deviation of the 10,000 random values
prob24_result <- sd(light_bulb_lifespans)

# Output the population standard deviation
prob24_result
#question 25
# Set the seed for reproducibility
set.seed(1)

# Parameters
num_samples <- 1000      # number of samples
sample_size <- 100       # size of each sample

# Initialize a vector to store the sample means
sample_means <- numeric(num_samples)

# Loop to take 1,000 samples and compute their means
for (i in 1:num_samples) {
  sample_values <- sample(light_bulb_lifespans, sample_size, replace = TRUE)
  sample_means[i] <- mean(sample_values)
}

# Output the vector of sample means
prob25_result <- sample_means
#question 26
# Create a histogram of the sample means
hist(prob25_result, 
     main = "Histogram of Sample Means", 
     xlab = "Sample Means", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black", 
     breaks = 30)  # You can adjust the number of breaks for better visualization
#question 27
# Calculate the mean of the sample means from problem 25
prob27_result <- mean(prob25_result)

# Output the mean of the sample means
prob27_result
#question 28
# Load necessary libraries
library(palmerpenguins)
library(ggplot2)
library(dplyr)

# Filter the data for Adélie penguins
adelie_penguins <- penguins %>% filter(species == "Adélie")

# Create a histogram with a density plot
ggplot(adelie_penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.5, fill = "blue") +
  labs(title = "Distribution of Flipper Length of Adélie Penguins",
       x = "Flipper Length (mm)",
       y = "Density") +
  theme_minimal()

# Perform a Shapiro-Wilk normality test
shapiro_test_result <- shapiro.test(adelie_penguins$flipper_length_mm)

# Print the Shapiro-Wilk test result
shapiro_test_result
#question 29
# Load necessary libraries
library(palmerpenguins)
library(ggplot2)
library(dplyr)

# Filter the data for Gentoo penguins
gentoo_penguins <- penguins %>% filter(species == "Gentoo")

# Create a scatter plot to visualize the relationship
ggplot(gentoo_penguins, aes(x = flipper_length_mm, y = bill_depth_mm)) +
  geom_point(color = "blue") +
  labs(title = "Relationship between Flipper Length and Beak Depth of Gentoo Penguins",
       x = "Flipper Length (mm)",
       y = "Beak Depth (mm)") +
  theme_minimal()

# Calculate the correlation coefficient
correlation <- cor(gentoo_penguins$flipper_length_mm, gentoo_penguins$bill_depth_mm, use = "complete.obs")

# Fit a linear regression model
model <- lm(bill_depth_mm ~ flipper_length_mm, data = gentoo_penguins)

# Summary of the regression model
model_summary <- summary(model)

# Output correlation and model summary
correlation
model_summary




