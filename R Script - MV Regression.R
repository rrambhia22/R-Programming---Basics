#MODULE 6 - R PRACTICE


#installing the packages
install.packages("broom")
install.packages("corrplot")
install.packages("gtsummary")
install.packages("caret")
install.packages("leaps")


#importing libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(psych)
library(MASS)
library(ggpubr)
library(broom)
library(corrplot)
library(gtsummary)
library(car)
library(caret)
library(leaps)


#reading csv file
heart_dataset <- read.csv("heart.csv")
heart_dataset



#describing the data set
colnames(heart_dataset)

start_records <- head(heart_dataset,10)
start_records

end_records <- tail(heart_dataset,10)
end_records

dataset_summary <- summary(heart_dataset)
dataset_summary

sapply(heart_dataset,class)

str(heart_dataset)



#descriptive analysis
min(heart_dataset$RestingBP)
max(heart_dataset$RestingBP)
mean(heart_dataset$RestingBP)
median(heart_dataset$RestingBP)
mode(heart_dataset$RestingBP)
range(heart_dataset$RestingBP)
sd(heart_dataset$RestingBP)
summary(heart_dataset$RestingBP)

min(heart_dataset$Cholesterol)
max(heart_dataset$Cholesterol)
mean(heart_dataset$Cholesterol)
median(heart_dataset$Cholesterol)
mode(heart_dataset$Cholesterol)
range(heart_dataset$Cholesterol)
sd(heart_dataset$Cholesterol)
summary(heart_dataset$Cholesterol)

min(heart_dataset$MaxHR)
max(heart_dataset$MaxHR)
mean(heart_dataset$MaxHR)
median(heart_dataset$MaxHR)
mode(heart_dataset$MaxHR)
range(heart_dataset$MaxHR)
sd(heart_dataset$MaxHR)
summary(heart_dataset$MaxHR)



#creating subset of data
random_dataset1 <- sample_n(heart_dataset,20)
random_dataset1


heart_dataset_new <- data.frame(heart_dataset$Age, heart_dataset$RestingBP, heart_dataset$Cholesterol,
                                heart_dataset$MaxHR,heart_dataset$HeartDisease)


#data visualization

#graph 1 : Boxplot of the attributes
par(mfrow=c(2,2))
boxplot(heart_dataset$RestingBP, col = "darkblue", main = "Resting Blood Pressure")
boxplot(heart_dataset$Cholesterol, col = "lightblue", main = "Cholesterol")
boxplot(heart_dataset$MaxHR, col = "cadetblue", main = "Max HR")


#graph 2 : Barplot of heart disease wrt gender
ggplot(heart_dataset, aes(x=Sex, y=HeartDisease, fill=Sex)) + geom_bar(stat = "identity") + 
  labs(
    x = "Gender",
    title = "Heart Disease with respect to Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))


#graph 3 : Resting BP wrt Age
ggplot(heart_dataset, aes(x=RestingBP, y=Age, fill=Age)) + geom_point(stat = "identity") + 
  stat_smooth(method = "lm",
              col = "#C42126",
              se = TRUE) +
  labs(
    x = "Resting BP",
    title = "Resting BP with respect to Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))


#graph 4 : Cholesterol wrt Age
ggplot(heart_dataset, aes(x=Cholesterol, y=Age, fill=Age)) + geom_point(stat = "identity") + 
  stat_smooth(method = "lm",
              col = "#C42126",
              se = TRUE) +
  labs(
    x = "Cholesterol",
    title = "Cholesterol with respect to Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))



#Correlation table & chart
heart_dataset_new.cor = cor(heart_dataset_new)
heart_dataset_new.cor

corrplot(heart_dataset_new.cor)



#REGRESSION MODEL


#SUBSET 1

#creating dummy variables
dummy_male <- ifelse(heart_dataset$Sex == "M", 1,0)
dummy_female <- ifelse(heart_dataset$Sex == "F", 1,0)


st_slope_down <- ifelse(heart_dataset$ST_Slope == "Down", 1,0)
st_slope_up <- ifelse(heart_dataset$ST_Slope == "Up", 1,0)
st_slope_up


#create data frame
dataframe_regression <- data.frame(Age = heart_dataset$Age,
                                   Male = dummy_male,
                                   Female = dummy_female,
                                   Heart_Diease = heart_dataset$HeartDisease,
                                   Resting_BP = heart_dataset$RestingBP,
                                   Cholesterol = heart_dataset$Cholesterol,
                                   St_slope_Down = st_slope_down,
                                   St_slope_Up = st_slope_up)
dataframe_regression



#MV regression model
reg_model <- lm(Heart_Diease ~ Age + Resting_BP + St_slope_Down + St_slope_Up, data = dataframe_regression)
reg_model
summary(reg_model)


#regression plot
avPlots(reg_model)




#SUBSET 2

#dummy variables created by R for gender attribute and MV regression model
model1 <- lm(HeartDisease ~ Sex + Age + Cholesterol, data = heart_dataset)
model1
summary(model1)


#regression plot
avPlots(model1)




#using the re level function for regression model
heart_dataset <- heart_dataset %>%
  mutate(Sex = relevel(Sex, ref = "M"))


#regression model
model2 <- lm(HeartDisease ~ Sex + Age + Cholesterol , data = heart_dataset)
model2
summary(model2)


#regression plot
avPlots(model2)




#PART 2
ggplot(heart_dataset, aes(x=RestingBP, y=Age, fill=Age)) + geom_point(stat = "identity") + 
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE) +
  labs(
    x = "RestingBP") 
 

ggplot(heart_dataset, aes(x=MaxHR, y=Age, fill=Age)) + geom_point(stat = "identity") + 
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE) +
  labs(
    x = "Max HR") 




#SUBSET ANALYSIS

subset_model <- regsubsets(HeartDisease ~., data = heart_dataset, nvmax = 10)
subset_model
summary(subset_model)



#best 1 variable model 
best1_model <- lm(HeartDisease ~ ST_Slope, data = heart_dataset)
best1_model

summary(best1_model)

avPlots(best1_model)



#best 2 variable model 
best2_model <- lm(HeartDisease  ~ ST_Slope + ExerciseAngina, data = heart_dataset)
best2_model

summary(best2_model)

avPlots(best2_model)



#best 3 variable model 
best3_model <- lm(HeartDisease  ~ ST_Slope + ExerciseAngina + Sex, data = heart_dataset)
best3_model

summary(best3_model)

avPlots(best3_model)



#best 4 variable model 
best4_model <- lm(HeartDisease  ~ ST_Slope + ExerciseAngina + Sex + ChestPainType, data = heart_dataset)
best4_model

summary(best4_model)

avPlots(best4_model)
