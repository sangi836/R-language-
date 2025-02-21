install.packages("coin")
# Load necessary packages
library(coin)  # For Wilcoxon rank sum test
library(stats) # For Spearman correlation
library(psych) # For descriptive statistics
library(ggplot2) # For visualization

# Question 6: Sign Test for Median Attendance
attendance <- c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573,
                2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)
median_claim <- 3000

# Sign test
install.packages("BSDA")
sign_test_result <- SIGN.test(attendance, md=median_claim, alternative="two.sided", conf.level=0.95)
print(sign_test_result)


# Question 10: Sign Test for Lottery Sales
below_200 <- 15
n <- 40
p_hat <- below_200 / n
binom_test_result <- binom.test(below_200, n, p=0.5, alternative="less")
print(binom_test_result)

# Question 4: Wilcoxon Rank Sum Test (Mann-Whitney U) for Prison Sentences
males <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
females <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

wilcox_test_result <- wilcox.test(males, females, alternative="two.sided")
print(wilcox_test_result)

# Question 8: Wilcoxon Rank Sum Test for Baseball Wins
nl <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
al <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

wilcox_test_baseball <- wilcox.test(nl, al, alternative="two.sided")
print(wilcox_test_baseball)

# Question 5-8: Wilcoxon Signed Rank Table Values
table_values <- c(13, 32, 65, 22)
sample_sizes <- c(15, 28, 20, 14)
alphas <- c(0.01, 0.025, 0.05, 0.10)

results <- data.frame(ws = table_values, n = sample_sizes, alpha = alphas)
print(results)

# Question 2: Kruskal-Wallis Test for Mathematics Literacy Scores
western <- c(527, 406, 474, 381, 411)
europe <- c(520, 510, 513, 548, 496)
asia <- c(523, 547, 547, 391, 549)

group <- factor(rep(c("Western", "Europe", "Asia"), times=c(length(western), length(europe), length(asia))))
scores <- c(western, europe, asia)

kruskal_test_result <- kruskal.test(scores ~ group)
print(kruskal_test_result)

# Question 6: Spearman Rank Correlation for Subway vs Rail Passengers
subway <- c(845, 494, 425, 313, 108, 41)
rail <- c(39, 291, 142, 103, 33, 38)

spearman_result <- cor.test(subway, rail, method="spearman")
print(spearman_result)

# Question 16: Simulation for Caramel Corn Prizes
set.seed(42)
num_trials <- 40
prizes_needed <- numeric(num_trials)

for (i in 1:num_trials) {
  prizes_collected <- numeric(4)
  count <- 0
  while (sum(prizes_collected) < 4) {
    new_prize <- sample(1:4, 1)
    prizes_collected[new_prize] <- 1
    count <- count + 1
  }
  prizes_needed[i] <- count
}

mean(prizes_needed)

# Question 18: Simulation for Lotto Winning
set.seed(42)
num_trials <- 30
tickets_needed <- numeric(num_trials)

for (i in 1:num_trials) {
  letters_collected <- numeric(3)
  count <- 0
  while (sum(letters_collected) < 3) {
    draw <- sample(c("b", "i", "g"), 1, prob = c(0.6, 0.3, 0.1))
    if (draw == "b") letters_collected[1] <- 1
    if (draw == "i") letters_collected[2] <- 1
    if (draw == "g") letters_collected[3] <- 1
    count <- count + 1
  }
  tickets_needed[i] <- count
}

mean(tickets_needed)
