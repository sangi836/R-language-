#question 1
# Load necessary library
# If you haven't installed the readr package, uncomment the following line:
# install.packages("readr")

library(readr)

# Download the dataset
# Make sure to specify the correct URL if the dataset is hosted online
url <- "c:/desktop/intro to r/ball-dataset.csv"  
download.file(url, destfile = "ball-dataset.csv", method = "curl")  # Use appropriate method for your OS

# Read the dataset into R
ball_data <- read_csv("ball-dataset.csv")

# Display the first few rows of the dataset
head(ball_data)
#question-2
# Load necessary libraries
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation

# Read the dataset
ball_data <- read_csv("ball-dataset.csv")

# Create the frequency table
freq_color <- ball_data %>%
  group_by(color) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))  # Optional: arrange by counts in descending order

# Display the frequency table
print(freq_color)
#question 3
# Load necessary libraries
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation

# Specify the correct path to the dataset
# Update this to the correct full path for your file
url <- "C:/Users/YourUsername/Desktop/intro to r/ball-dataset.csv"  # Replace 'YourUsername' with your actual username

# Read the dataset
ball_data <- read_csv(url)  # Use read.csv(url) if you prefer base R

# Display the first few rows of the dataset to confirm it's loaded
head(ball_data)
#question 4
# Load necessary library
library(ggplot2)

# Create a sample data frame representing the counts of different colored balls
ball_data <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)
)

# Create the bar chart
ggplot(ball_data, aes(x = Color, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  ggtitle("Color Counts of Balls") +
  xlab("Color") +
  ylab("Count") +
  scale_fill_manual(values = c("blue", "green", "red", "yellow"))
#question 5
# Load necessary library
library(ggplot2)

# Create a sample data frame representing the counts of different labels
label_data <- data.frame(
  Label = c("A", "B", "C", "D", "E"),
  Count = c(150, 320, 100, 350, 200)
)

# Create the bar chart
ggplot(label_data, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  ggtitle("Label Counts of Balls") +
  xlab("Label") +
  ylab("Count") +
  scale_fill_manual(values = c("coral", "olive", "mediumseagreen", "deepskyblue", "orchid"))
#question 6
# Sample data (counts of balls of different colors)
ball_counts <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)
)

# Number of green balls
green_balls <- ball_counts$Count[ball_counts$Color == "green"]

# Total number of balls
total_balls <- sum(ball_counts$Count)

# Calculate probability of drawing a green ball
prob6_result <- green_balls / total_balls
prob6_result
#question 7
# Sample data (counts of balls of different colors)
ball_counts <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)
)

# Number of blue and red balls
blue_balls <- ball_counts$Count[ball_counts$Color == "blue"]
red_balls <- ball_counts$Count[ball_counts$Color == "red"]

# Total number of balls
total_balls <- sum(ball_counts$Count)

# Calculate probability of drawing a blue or red ball
prob7_result <- (blue_balls + red_balls) / total_balls
prob7_result
#question 8
# Sample data (counts of balls with different labels)
label_counts <- data.frame(
  Label = c("A", "B", "C", "D", "E"),
  Count = c(150, 320, 100, 350, 200)
)

# Number of balls with label A and C
label_A_balls <- label_counts$Count[label_counts$Label == "A"]
label_C_balls <- label_counts$Count[label_counts$Label == "C"]

# Total number of balls
total_balls <- sum(label_counts$Count)

# Calculate probability of drawing a ball with label A or C
prob8_result <- (label_A_balls + label_C_balls) / total_balls
prob8_result

#question 9
# Sample data for ball colors
color_counts <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)
)

# Sample data for ball labels
label_counts <- data.frame(
  Label = c("A", "B", "C", "D", "E"),
  Count = c(150, 320, 100, 350, 200)
)

# Total number of balls
total_balls <- sum(color_counts$Count)

# Probability of drawing a yellow ball
yellow_balls <- color_counts$Count[color_counts$Color == "yellow"]
prob_yellow <- yellow_balls / total_balls

# Probability of drawing a ball with label D
label_D_balls <- label_counts$Count[label_counts$Label == "D"]
prob_label_D <- label_D_balls / total_balls

# Calculate probability of drawing a yellow ball with label D
prob9_result <- prob_yellow * prob_label_D
prob9_result
#question 10
# Sample data for ball colors
color_counts <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)
)

# Sample data for ball labels
label_counts <- data.frame(
  Label = c("A", "B", "C", "D", "E"),
  Count = c(150, 320, 100, 350, 200)
)

# Total number of balls
total_balls <- sum(color_counts$Count)

# Probability of drawing a yellow ball
yellow_balls <- color_counts$Count[color_counts$Color == "yellow"]
prob_yellow <- yellow_balls / total_balls

# Probability of drawing a ball with label D
label_D_balls <- label_counts$Count[label_counts$Label == "D"]
prob_label_D <- label_D_balls / total_balls

# Assuming no specific data on yellow balls with label D, let's assume it's 0 for simplicity.
# If you had data on how many yellow balls are labeled "D", you would use that.
yellow_and_D_balls <- 0
prob_yellow_and_D <- yellow_and_D_balls / total_balls

# Calculate the probability of drawing a yellow ball or a ball with label D
prob10_result <- prob_yellow + prob_label_D - prob_yellow_and_D
prob10_result
#question 11
# Sample data for ball colors
color_counts <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)
)

# Total number of balls
total_balls <- sum(color_counts$Count)

# Number of blue and red balls
blue_balls <- color_counts$Count[color_counts$Color == "blue"]
red_balls <- color_counts$Count[color_counts$Color == "red"]

# Calculate the probability of drawing a blue ball followed by a red ball without replacement
prob11_result <- (blue_balls / total_balls) * (red_balls / (total_balls - 1))
prob11_result
#question 12
# Sample data for ball colors
color_counts <- data.frame(
  Color = c("blue", "green", "red", "yellow"),
  Count = c(320, 220, 450, 130)  # Given counts of balls
)

# Total number of balls
total_balls <- sum(color_counts$Count)

# Number of green balls
green_balls <- color_counts$Count[color_counts$Color == "green"]

# Calculate the probability of drawing four green balls in a row without replacement
prob12_result <- (green_balls / total_balls) *
  ((green_balls - 1) / (total_balls - 1)) *
  ((green_balls - 2) / (total_balls - 2)) *
  ((green_balls - 3) / (total_balls - 3))

prob12_result
#question 13
# Example counts (replace with actual values)
red_count <- 5  # number of red balls
b_count <- 3    # number of balls with "B"
total_balls <- 10  # total number of balls

# Calculate the probability
prob_red_first <- red_count / total_balls
prob_b_second <- b_count / (total_balls - 1)

# Combine the probabilities
prob13_result <- prob_red_first * prob_b_second

# Print the result
prob13_result
#question 14
my_factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * my_factorial(n - 1))
  }
}

# Test the function
my_factorial(0)  # Should return 1
my_factorial(3)  # Should return 6
my_factorial(5)  # Should return 120
#question 15
# Create a data frame with all possible outcomes of flipping a coin four times
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "T", "H", "H", "T", "T", "H", "H", "H", "T", "T", "T", "T", "T"),
  third = c("H", "H", "T", "H", "T", "T", "H", "T", "H", "H", "T", "H", "H", "T", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "T", "T", "H", "T", "H", "T", "H", "T", "T", "T")
)

# Display the data frame
print(coin_outcomes)
#question 16
# Create the data frame with outcomes
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "T", "H", "H", "T", "T", "H", "H", "H", "T", "T", "T", "T", "T"),
  third = c("H", "H", "T", "H", "T", "T", "H", "T", "H", "H", "T", "H", "H", "T", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "T", "T", "H", "T", "H", "T", "H", "T", "T", "T")
)

# Compute the probability of each outcome
coin_outcomes$probability <- 1 / 16

# Display the updated data frame
print(coin_outcomes)
#question 17
# Create the data frame with outcomes
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "T", "H", "H", "T", "T", "H", "H", "H", "T", "T", "T", "T", "T"),
  third = c("H", "H", "T", "H", "T", "T", "H", "T", "H", "H", "T", "H", "H", "T", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "T", "T", "H", "T", "H", "T", "H", "T", "T", "T")
)

# Count the number of heads in each row
coin_outcomes$num_heads <- rowSums(coin_outcomes == "H")

# Calculate the frequency of each number of heads (0 to 4)
freq_table <- table(coin_outcomes$num_heads)

# Calculate probabilities for each number of heads
num_heads_prob <- freq_table / sum(freq_table)

# Convert to a data frame for easier viewing
num_heads_prob_df <- as.data.frame(num_heads_prob)
names(num_heads_prob_df) <- c("num_heads", "probability")

# Display the result
print(num_heads_prob_df)
#question 18
# Create the data frame with outcomes
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "T", "H", "H", "T", "T", "H", "H", "H", "T", "T", "T", "T", "T"),
  third = c("H", "H", "T", "H", "T", "T", "H", "T", "H", "H", "T", "H", "H", "T", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "T", "T", "H", "T", "H", "T", "H", "T", "T", "T")
)

# Count the number of heads in each row
coin_outcomes$num_heads <- rowSums(coin_outcomes == "H")

# Calculate the count of outcomes with exactly three heads
count_three_heads <- sum(coin_outcomes$num_heads == 3)

# Total number of outcomes
total_outcomes <- nrow(coin_outcomes)

# Calculate the probability of getting exactly three heads
prob18_result <- count_three_heads / total_outcomes

# Print the result
prob18_result
#question 19
# Create the data frame with outcomes
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "T", "H", "H", "T", "T", "H", "H", "H", "T", "T", "T", "T", "T"),
  third = c("H", "H", "T", "H", "T", "T", "H", "T", "H", "H", "T", "H", "H", "T", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "T", "T", "H", "T", "H", "T", "H", "T", "T", "T")
)

# Count the number of heads in each row
coin_outcomes$num_heads <- rowSums(coin_outcomes == "H")

# Count outcomes with exactly two heads
count_two_heads <- sum(coin_outcomes$num_heads == 2)

# Count outcomes with exactly four heads
count_four_heads <- sum(coin_outcomes$num_heads == 4)

# Total number of outcomes
total_outcomes <- nrow(coin_outcomes)

# Calculate the combined probability of getting exactly two heads or four heads
prob19_result <- (count_two_heads + count_four_heads) / total_outcomes

# Print the result
prob19_result
#question 20
# Create the data frame with outcomes
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "T", "H", "H", "T", "T", "H", "H", "H", "T", "T", "T", "T", "T"),
  third = c("H", "H", "T", "H", "T", "T", "H", "T", "H", "H", "T", "H", "H", "T", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "T", "T", "H", "T", "H", "T", "H", "T", "T", "T")
)

# Count the number of heads in each row
coin_outcomes$num_heads <- rowSums(coin_outcomes == "H")

# Count outcomes with less than or equal to three heads
count_less_equal_three_heads <- sum(coin_outcomes$num_heads <= 3)

# Total number of outcomes
total_outcomes <- nrow(coin_outcomes)

# Calculate the probability of getting less than or equal to three heads
prob20_result <- count_less_equal_three_heads / total_outcomes

# Print the result
prob20_result
#question 21
# Define the outcomes (number of heads) and their respective probabilities
outcomes <- 0:4
probabilities <- c(0.0625, 0.25, 0.375, 0.25, 0.0625)

# Create a bar plot
barplot(probabilities, 
        names.arg = outcomes, 
        xlab = "Number of Heads", 
        ylab = "Probability", 
        main = "Probability Distribution of Heads for 4 Flips", 
        col = "cyan", 
        ylim = c(0, 0.4))
#question 22
# Probabilities of winning at home and away
prob_win_home <- 0.75
prob_win_away <- 0.50

# Probability of winning all 10 games (5 at home and 5 away)
prob22_result <- (prob_win_home ^ 5) * (prob_win_away ^ 5)
prob22_result
#question 23
# Number of games
n_home <- 5
n_away <- 5

# Probability of winning more than 1 game
prob_0_home <- dbinom(0, n_home, prob_win_home)
prob_1_home <- dbinom(1, n_home, prob_win_home)
prob_0_away <- dbinom(0, n_away, prob_win_away)
prob_1_away <- dbinom(1, n_away, prob_win_away)

# Total probability of winning more than 1 game
prob23_result <- 1 - (prob_0_home * prob_0_away + prob_1_home * prob_0_away + prob_0_home * prob_1_away + prob_1_home * prob_1_away)
prob23_result
#question 24
# Calculate combinations
prob24_result <- choose(5, 3) * choose(5, 2)
prob24_result






