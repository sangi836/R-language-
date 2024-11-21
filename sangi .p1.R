# clears global environment
rm(list = ls())  
# clears packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
# clears plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) 
# disables scientific notion for entire R session
options(scipen = 100) 
# clears console
cat("\014")  

install.packages('pacman')
library(pacman)
#need tidyverse to run "summarise". If doesn't work try p_load(tidyverse)
install.packages('tidyverse')
library(tidyverse)
#p_load(tidyverse)

filename <- file.choose()
data <- readRDS(filename)

#Reduce my table to feature only relevant columns
data <- data[, c("date", "location", "subject_race", "subject_sex", "type", 
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
# Bar Chart of stops by day of week
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Traffic Stops", 
       x = "Day of Week", y = "Frequency")

# Bar plot of stops by race
ggplot(data, aes(x = subject_race, fill = subject_race)) +
  geom_bar() +
  labs(title = "Traffic Stops by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted histogram using ggplot2 to compare across multiple classes
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~ subject_race) +
  labs(title = "Distribution of Stop Times by Race",
       x = "Time of Day",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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



