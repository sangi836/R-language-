#part 1
#question 1
library(dplyr)
data <-reaqd.csv("your_dataset.csv")
str(data)
summary(data)
#question 2
#a 
colnames(data)<-c("new_coli","new_col2","new_col3")
#b
data <-na.omit(data)
data[is.na(data)] <-0
#c
data$column_name <-as.numeric(data$column_name)
data$date_column <-as.date(data$date_column)
#d
data <-data %>% select(-unwanted_column)
#e
library(stringr)
data$new_string_coloumn <-str_to_lower(data$original_string_column)
#f
data <-data %>% arrange(column_name)
#g
subset_data <-data %>% filter(condition)
#question 3
summary(data$interesting_variable)
#question 4
library(ggplot2)
ggplot(data,aes(x=factor_variable))+
  geom_bar()
ggplot(data,aes(x=numeric_variable)+
         geom_histogram(binwidth = 1)
ggplot(data,aes(x=var1,y=var2))+
  geom_point()
#part 2
#question 1
data <- data %>%
  mutate(monthly_change = sales-lag(sales))
#question 2
summary_data <- data %>%
  group_by(category) %>%
  summarise(total_sales = sum(sales),average_sales =mean(sales)) %>%
  arrange(desc(total_sales))
#question 3
intresting_data <-data %>%
  filter(sales>threshold)
ggplot(intresting_data, aes(x= category,y =sales)) +
  geom_co()
#question 4
library(ggplot2)
ggplot(data, aes(x=factor_variable))+
  goem_bar(fill="blue")+
  labs(title = "bar chart of factor variable",x="factor variable",y="count")
ggplot(data,aes(x = numeric_variable)) +
  goem_histogram(binwidth =1,fill="lightblue",color="black")+
  labs(title = "histogram of numeric variable",x="numeric variable",y="frequency")
ggplot(data,aes(x=date_variable,y=numeric_variable))+
  geom_line(color ="red")+
  labs(title ="line graph of numeric variable variable over time",x="date",y="numeric variable")
ggplot(data,aes(x=var1,y = var2))+
  geom_point(color ="green")+
  labs(title ="scatterd plot of var1 vs var2",x="var1",y="var2")+
  theme_minimal()
#part 2
#question 1 
data <-data %>%
  arrange(date_variable) %>%
  mutate(monthly_change = sales -lag(sales))
#question 2
summary_data <-data %>%
  group_by(category_variable) %>%
  summarise(
    total_sales =sum(sales,na.rm=TRUE),
    average_sales=mean(sales,na.rm= TRUE),
    count =n()
  )
%>%
  arrange(desc(total_sales))
#question 3
ggplot(summary_data,aes(x=reorder(category_variable,total_sales),y=total_sales))+
  geom_col(fill="purple")+
  labs(title = "total sales by category",x="category",y="total sales")+
  coord_flip()
    
  )
