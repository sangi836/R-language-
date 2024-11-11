install.packages("testthat")
install.packages("pacman")
cat("\014") # Clears console
rm(list = ls()) # Clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # Clears packages
options(scipen = 100) # Disables scientific notation

# Load necessary packages
install.packages("pacman")
library(pacman)
p_load(testthat)

# Run tests
test_file("project1_tests.R")

# Question 1: Compute the following expressions
result1 <- 123 * 453
result2 <- 5^2 * 40
result3 <- TRUE & FALSE
result4 <- TRUE | FALSE
result5 <- 75 %% 10
result6 <- 75 / 10

# Question 2: Create first_vector
first_vector <- c(17, 12, -33, 5)

# Question 3: Create counting_by_fives
counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)

# Question 4: Create second_vector (20 down to 1)
second_vector <- 20:1

# Question 5: Create counting_vector (5 to 15)
counting_vector <- 5:15

# Question 6: Create grades vector
grades <- c(96, 100, 85, 92, 81, 72)

# Question 7: Add 3 to grades
bonus_points_added <- grades + 3

# Question 8: Create one_to_one_hundred
one_to_one_hundred <- 1:100

# Question 9: Comments for each operation
# Adds 20 to each element in second_vector
add_20 <- second_vector + 20
# Multiplies each element in second_vector by 20
multiply_20 <- second_vector * 20
# Returns TRUE if each element in second_vector is >= 20
greater_than_20 <- second_vector >= 20
# Returns TRUE if each element in second_vector is not equal to 20
not_equal_20 <- second_vector != 20

# Question 10: Sum of one_to_one_hundred
total <- sum(one_to_one_hundred)

# Question 11: Mean of one_to_one_hundred
average_value <- mean(one_to_one_hundred)

# Question 12: Median of one_to_one_hundred
median_value <- median(one_to_one_hundred)

# Question 13: Max of one_to_one_hundred
max_value <- max(one_to_one_hundred)

# Question 14: Min of one_to_one_hundred
min_value <- min(one_to_one_hundred)

# Question 15: Extract first value from second_vector
first_value <- second_vector[1]

# Question 16: Extract first three values from second_vector
first_three_values <- second_vector[1:3]

# Question 17: Extract specific elements from second_vector
vector_from_brackets <- second_vector[c(1, 5, 10, 11)]

# Question 18: Boolean indexing on first_vector
vector_from_boolean_brackets <- first_vector[c(FALSE, TRUE, FALSE, TRUE)]
# The Boolean vector selects elements from first_vector corresponding to TRUE.

# Question 19: This checks if elements in second_vector are >= 10.
# Returns a Boolean vector.

# Question 20: Extract elements from one_to_one_hundred that are >= 20
greater_than_20_values <- one_to_one_hundred[one_to_one_hundred >= 20]

# Question 21: Remove values from grades <= 85
lowest_grades_removed <- grades[grades > 85]

# Question 22: Remove 3rd and 4th elements from grades
middle_grades_removed <- grades[-c(3, 4)]

# Question 23: Remove 5th and 10th elements from second_vector
fifth_vector <- second_vector[-c(5, 10)]

# Question 24: Generate random_vector
set.seed(5)
random_vector <- runif(n = 10, min = 0, max = 1000)

# Question 25: Sum of random_vector
sum_vector <- sum(random_vector)

# Question 26: Cumulative sum of random_vector
cumsum_vector <- cumsum(random_vector)

# Question 27: Mean of random_vector
mean_vector <- mean(random_vector)

# Question 28: Standard deviation of random_vector
sd_vector <- sd(random_vector)

# Question 29: Round random_vector
round_vector <- round(random_vector)

# Question 30: Sort random_vector
sort_vector <- sort(random_vector)

# Question 31: Read the CSV file
first_dataframe <- read.csv("ds_salaries.csv")

# Question 32: Summary statistics of first_dataframe
summary_stats <- summary(first_dataframe)

# Display results
print(list(
  result1 = result1,
  result2 = result2,
  result3 = result3,
  result4 = result4,
  result5 = result5,
  result6 = result6,
  first_vector = first_vector,
  counting_by_fives = counting_by_fives,
  second_vector = second_vector,
  counting_vector = counting_vector,
  grades = grades,
  bonus_points_added = bonus_points_added,
  one_to_one_hundred = one_to_one_hundred,
  total = total,
  average_value = average_value,
  median_value = median_value,
  max_value = max_value,
  min_value = min_value,
  first_value = first_value,
  first_three_values = first_three_values,
  vector_from_brackets = vector_from_brackets,
  vector_from_boolean_brackets = vector_from_boolean_brackets,
  lowest_grades_removed = lowest_grades_removed,
  middle_grades_removed = middle_grades_removed,
  fifth_vector = fifth_vector,
  random_vector = random_vector,
  sum_vector = sum_vector,
  cumsum_vector = cumsum_vector,
  mean_vector = mean_vector,
  sd_vector = sd_vector,
  round_vector = round_vector,
  sort_vector = sort_vector,
  first_dataframe = first_dataframe,
  summary_stats = summary_stats
))

