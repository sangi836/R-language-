# question 1
#load the data
data_2015<-read.csv("2015.csv")
#Test the loaded data by displaying the first few rows
head(data_2015)
#question 2
#Assuming you have already loaded the data into data_2015
column_names <-(data_2015)
#print the column names
print(column_names)
# question 3
#view the function data_2015 data setin a separate tab
View(data_2015)
#question 4
#load the dplyr package if you haven't already
library(dplyr)
#use the glimpse function to view the data_2015 dataset
glimpse(data_2015)
# install the janitor package if you haven't already
install.packages("janitor")
#question 5
#load the janitor package
library(janitor)
#clean the column names of the data_2015 dataframe
data_2015 <- clean_names (data_2015)
#view the clearned dataframe
data_2015
#question 6
#load the dplyr package if you haven't already 
library(dplyr)
#select the desired columns and store in happy_df
happy_df <- data_2015 %>%
  select(country,region,happiness_score,freedom)
#view the new happy_df
happy_df
#question 7
#slice the first 10 rows and store in top_ten_df
top_ten_df <-happy_df %>%
  slice(1:10)
#view the new top_ten_df
top_ten_df
#question 8
#filter for freedom values under 0.20 and store in no_freedom_df
no_freedom_df<-happy_df %>%
  filter(freedom<0.20)
#view the new no_freedom_df
no_freedom_df
#question 9
#arrange the values in happy_df in descending order by freedom and store in best_freedom_df
best_freedom_df <-happy_df %>%
  arrange(desc(freedom))
#view the new best_freedom_df
best_freedom_df
#question 10 
#create a new coloumn gff_stat that is the sum of family,freedom,and generosity
data_2015 <-data_2015%>%
  mutate(gff_stat = family + freedom + generosity)
#view the updated data_2015
data_2015
#question 11
library(dplyr)
#group by region and summarize 
reginal_stats_df <-happy_df %>%
  group_by(region) %>%
  smummarise (
    country_court =n(),
    mean_happiness =mean(happiness_score na.rm=true),
    mean_freedom=mean(freedom,na.rm = true)
  )
#view the resulting table
reginal_stats_df
#question 14 
library(dplyr)
#remove players with 0 at bats
baseball <-baseball %>%
  filter(AB >0)
#view the resulting table
baseball
#question 15
library(dplyr)
#Add the batting average column to the baseball dataset
baseball <-baseball %>%
  mutate(BA=H /AB)
#view the updated dataset
baseball
#question 16
library(dplyr)
#Add the OBP coloumn to the baseball dataset
baseball <-baseball %>%
  mutated (OBP =(H +BB)/ (AB/BB))
#view the updated dataset
baseball
#question 17
library(dplyr)
#get the top 10 players with the most strikeouts(so)
strikeout_artist <-baseball%
arrange(desc(so)) %>%
  slice_head(n = 10)
#view the results
stikeout_artist
#question18
library(dplyr)
#filter eligible players
eligible_df <-baseball %>%
  filter(AB >=300 | G >=100)
#view the result
eligible_df
