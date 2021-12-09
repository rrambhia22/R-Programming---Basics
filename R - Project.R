#importing libraries

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(scales)


#reading csv files

continent_dataset <- read.csv("COVID-19Dataset.csv")
continent_dataset

country_dataset <- read.csv("country_wise_latest.csv")
country_dataset

daywise_dataset <- read.csv("day_wise.csv")
daywise_dataset

world_dataset <- read.csv("worldometer_data.csv")
world_dataset



#descriptive analysis

head(continent_dataset,10)
tail(continent_dataset,10)
str(continent_dataset)
summary(continent_dataset)


head(country_dataset,10)
tail(country_dataset,10)
str(country_dataset)
summary(country_dataset)


head(daywise_dataset,10)
tail(daywise_dataset,10)
str(daywise_dataset)
summary(daywise_dataset)


head(world_dataset,10)
tail(world_dataset,10)
str(world_dataset)
summary(world_dataset)


continent <- unique(continent_dataset$Continent)
continent



#data cleanup

continent_dataframe <- data.frame(continent_dataset$Continent,continent_dataset$Total_Cases)
continent_dataframe

table1 <- table(continent_dataset$Continent, continent_dataset$Total_Cases)


#removing NAN values
continent_filtered = subset(continent_dataset, Continent != "")
continent_filtered




#visualization

#graph 1 : Total cases in each continent
ggplot(continent_filtered, aes(x=Continent, y=Total_Cases, fill=Continent)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    y = "Cases Count",
    title = "Total Covid Cases in each Continent") +
    theme(plot.title = element_text(hjust = 0.5))
new_graph = last_plot()
new_graph + coord_polar()



#graph 2 : Total deaths in each continent
ggplot(continent_filtered, aes(x=Continent, y=Total_Deaths, fill=Continent)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    y = "Cases Count",
    title = "Total Deaths in each Continent") +
  theme(plot.title = element_text(hjust = 0.5))
  


#graph 3 : Number of people vaccinated in each continent 
ggplot(continent_filtered, aes(x=Continent, y=People_Vaccinated, fill=Continent)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    y = "Cases Count",
    title = "People Vaccinated in each Continent") +
  theme(plot.title = element_text(hjust = 0.5))



#graph 4 : Total deaths vs Total recovered depending on total cases from world dataset
x <- world_dataset$TotalCases
y1 <- world_dataset$TotalDeaths
y2 <- world_dataset$TotalRecovered
par(mar = c(5,5,3,5))
plot(x,y1, type ="l", ylab="Death Count", xlab="Total Cases", main="Total Deaths vs Total Recovered depending on Total Cases from the World Data", col="blue")
par(new=TRUE)
plot(x,y2, type="l", xaxt = "n", yaxt = "n", ylab="",xlab="",col="red", lty= 2)
axis(side = 4)
mtext("Recovered Count",side = 4, line=3)
legend("topleft",c("Deaths","Recovered"),col = c("blue","red"),lty = c(1,2))



#creating new attributes and computing mean, median, etc..

world_data_total_cases <- c(world_dataset$TotalCases)
world_data_total_cases

#minimum
min(world_data_total_cases)
#maximum
max(world_data_total_cases)
#mean
mean(world_data_total_cases)
#median
median(world_data_total_cases)
#range
range(world_data_total_cases)
#standard deviation
sd(world_data_total_cases)
#summary of the attribute of world dataset
summary(world_data_total_cases)

#summary of the attribute of continent dataset
summary(continent_dataset$Total_Cases)

#summary of the attribute of daywise dataset
summary(daywise_dataset$Confirmed)

#summary of the attribute of country wise dataset
summary(country_dataset$Confirmed)

