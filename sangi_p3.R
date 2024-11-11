# question -1
install.packages("janitor")
#load the janitor package 
library(janitor)
#assuming 'books' is your data frame 
# clean the column names and store the result back into 'books'
books <-clean_names(books)
#display the cleaned data frame
print(books)
#question -2
install.packages("lubridate")
library(lubridate)
#assuming 'books' is your data frame 
#convert the first_publish_date column to data type
books$first_publish_date <-mdy(books$first_publish_date)
#display the updated data frame  
print(books)
#question-3
library(lubridate)
#assuming 'books' is your data frame and first_publish_date is already a data type
#extract the year and create a new column named 'year'
books$year <-year(book$first_publish_date)
#display the updated data frame 
print(books)
#question-4
install.packages("dplyr")
#load the dplyr package
library(dplyr)
#assuming 'books' is your data frame and it has a 'year' coloumn
#filter the dataset for books published between 1990 and 2020 (inclusive)
filtered_books <-books %>%
  #display the filtered dataset 
  print(filterd_books)
#question-5
library(dplyr)
assuming 'books' is your data frame 
remove the specified columns
books <- books %>%
  #display the updated data frame
  print(books)
#question-6 
library(dplyr)
#assuming 'books' is your data frame and it has a 'pages' column
#filter the dataset for books with fewer than 700 pages 
filtered_books <-books %>%
  filter(pages <700)
#display the filtered dataset 
print(filtered_books)
#question-7
#assuming 'books' is your data frame 
#remove rows with any NAs
books <-na.omit(books)
#display the updated data frame 
print(books)
#question -8
library(dplyr)
#question -9
#asuming 'books' is your data frame
#use glimse to produce a longview of the dataset 
glimpse(books)
#asuming 'books' is your dataframe
#use summary to produce a breakdown of statistics
sumary(books)
#question-10
install.packages("gglot2")
#load the ggplot2 package 
library(ggplot2)
#asuming 'books' is your data frame and has a coloumn named 'rating'
#create the histogram 
ggplot(books,aes(x = rating)) +
  geom_histogram(bindwidth =0.25, fill ="red",color="black")+
  labs(x="rating",y="number of books",title="histogram of book ratings")+
  theme_minimal()
#question-11
install.packages("ggplot2")
#load the ggplot2 package 
library(ggplot2)
#assuming 'books'is your data frame and has a coloumn named 'pages'
#create the horizontal boxplot
ggplot2(books,aes(x = pages))+
  geom_boxplot(fill = "red")+
  labs(x="pages", title ="box plot of page counts")+
  coord_flip() + 
  theme_minimal()
#question-12
library(dplyr)
#assuming 'books' is your data frame and it has a 'year' coloumn
#create a data frame with the count of books by year 
by_year <-books %>%
  group_by(year)%>%
  summarise(total_booksn =(),.groups = "drop")
#display the new data frame 
print(by_year)
#question -13
library(ggplot2)
#filter the by_year data frame for the years 1990 to 2020
by_year_filtered <- by_year %>%
  filter(year >=1990 & year <=2020)
#create the line plot with points 
ggplot2(by_year_filtered, aes(x=year,y= total_books))+
  geom_line()+
  geom_point()+
  labs(title = "total number of books rated per year",
       x= "year",
       y= "total books")+
  theme_minimal()
#question-14
library(dplyr)
#assuming 'books' is your data frame and it has a 'publisher'column
#create the book_publisher data frame 
book_publisher <-books %>%
  summarize(book_count = n(),.groups ='drop')
#display  the new dataframe
print(book_publisher)
#question-15
library(dplyr)
#remove publishers with fewer than 125 books
book_publishers <- book_publisher %>%
  filter(book_count >=125)
#display the updated data frame
print(book_publisher)
#question-16
library(dplyr)
#order book_publisher by book_count in desending order 
book_publisher <-book_publisher %>%
  arrange (desc(book_count))
#display the updated data frame 
print(book_publisher)
#question-17
#assuming book_publisher is already created and contains the book_count coloumn
#add a column for cumulative counts
books_publisher <-book_publisher %>%
  mutate(cum_counts = cumsum(book_count))
#display the updated data frame
print(book_publisher)
#question-18
#assumming book_publisher is already created and contains the book_count coloumn 
#caluclate the total number of books 
total_books <-sum(book_publisher$book_count)
#add a coloumn for relative frequency 
book_publisher <-book_publisher %>%
  mutate(rel_freq =book_count/total_books)
#display the updated data frame
print(book_publisher)
#question-19
#assumming book_publisher is already created and contains the rel_freq coloumn
#add a coloumn for cumulative frequency 
book_publisher <-book_publisher %>%
  mutate(cum_freq = cumsum(rel_freq))
#display the updated data frame
print(book_publisher)
#question-20
#convert the publisher coloumn to a factor with level levels defined by the current ordering 
book_publisher <-book_publisher %>%
  mutate(publisher =factor(publisher,levels =publisher))
#display the updated data frame
print(book_publisher)
#question-21
library(ggplot2)
library(dplyr)
#create the pareto chart with ogive 
ggplot(book_publisher,aes(x=publisher,y=book_count))+
  geom_bar(stat ="identity",fill="cyan") + #bar plot 
  geom_line(aes(y=cum_counts),color ="blue",group=1,size=1)+ #cumulative count line
  geom_point(aes(y=cum_counts),color="blue",size=2)+ #points on the cumulative line
  labs(x="publisher",y="number of books"title="books count (1990-2020)") +
  theme_mimimal()+
  theme(axis.text.x=element_text(angle45,hjust=1))#rotate x-axis lables 
#question-22
ggplot(book_publisher,aes(x=reorder(publisher,-rel_freq),y=rel_freq))+
  geom_bar(stat = "identity",fill="lightblue")+
  labs(x="publisher",y="relativefrequency",title = "relative frequency of books by publisher(1990-2020")+
  theme_minimal()+
  
  theme(axis.text.x=element_text(angle=45,hjust=1))


