#MODULE 1 - R PRACTICE


#importing libraries

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)


#reading csv file
imdb_dataset <- read.csv("imdb.csv")
imdb_dataset


#descriptive analysis
colnames(imdb_dataset)

start_records <- head(imdb_dataset,10)
start_records

end_records <- tail(imdb_dataset,10)
end_records

dataset_summary <- summary(imdb_dataset)
dataset_summary

sapply(imdb_dataset,class)

str(imdb_dataset)


#data cleanup

#revalue
imdb_dataset$Rate = revalue(imdb_dataset$Rate, c("No Rate"=0))
imdb_dataset$Rate
imdb_dataset$Votes = revalue(imdb_dataset$Votes, c("No Votes"=0))
imdb_dataset$Votes
imdb_dataset$Duration = revalue(imdb_dataset$Duration, c("None"=0))
imdb_dataset$Duration
imdb_dataset$Episodes = revalue(imdb_dataset$Episodes, c("-"="NA"))
imdb_dataset$Episodes


#creating new attributes

#subset of 'series' record
imdb_dataset_filtered_series = filter(imdb_dataset, Type=="Series")
imdb_dataset_filtered_series

#first 20 and last 20 records of series record
filtered_series1 <- head(imdb_dataset_filtered_series,20)
filtered_series2 <- tail(imdb_dataset_filtered_series,20)
filtered_series <- rbind.data.frame(filtered_series1,filtered_series2)
filtered_series
 
#subset of 'film' record
imdb_dataset_filtered_films = filter(imdb_dataset, Type=="Film")
imdb_dataset_filtered_films

#first 20 and last 20 records of films record
filtered_films1 <- head(imdb_dataset_filtered_films,20)
filtered_films2 <- tail(imdb_dataset_filtered_films,20)
filtered_films <- rbind.data.frame(filtered_films1,filtered_films2)
filtered_films


#genre of series and film 
data.frame(unique(imdb_dataset_filtered_series$Genre))
result_series <- data.frame(count(imdb_dataset_filtered_series$Genre))
result_series

data.frame(unique(imdb_dataset_filtered_films$Genre))
result_films <- data.frame(count(imdb_dataset_filtered_films$Genre))
result_films


#frequency table
genre_series <- table(imdb_dataset_filtered_series$Genre)
genre_series
genre_films <- table(imdb_dataset_filtered_films$Genre)
genre_films



#data visualization

#graph 1 : Genre Frequency - Series
barplot(genre_series,
        main = "Genre Frequency for Series",
        xlab = "COUNTS",
        col = "lightgreen",
        horiz = TRUE,
        cex.names = 0.8)


#graph 2 : Genre Frequency - Films
barplot(genre_films,
        main = "Genre Frequency for Films",
        xlab = "COUNTS",
        col = "lightblue",
        horiz = TRUE,
        cex.names = 0.8)


#graph 3 : Name of Series and its Rating
ggplot(filtered_series, aes(x=Name, y=Rate, fill=Name)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    x = "Name of Series",
    title = "Name of the Series and its Ratings") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))


#graph 4 : Name of Series and the Votes
ggplot(filtered_series, aes(x=Name, y=Votes, fill=Name)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    x = "Name of Series",
    title = "Name of the Series and the Votes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))


#graph 5 : Name of Film and its Rating
ggplot(filtered_films, aes(x=Name, y=Rate, fill=Name)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    x = "Name of Film",
    title = "Name of the Film and its Ratings") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))


#graph 6 : Name of Film and the Votes
ggplot(filtered_films, aes(x=Name, y=Votes, fill=Name)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    x = "Name of Film",
    title = "Name of the Film and the Votes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))


#graph 7 : Series and its Total no. of Episodes
ggplot(filtered_series, aes(x=Name, y=Episodes, fill=Name)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    x = "Name of the Series",
    title = "Episodes for a particular series") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))

