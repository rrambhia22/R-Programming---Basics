#print Name
print("Plotting Basics: Rambhia")

#installing libraries
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")

#importing libraries
require(FSA) #not working
require(FSAdata)
require(magrittr)
require(dplyr)
require(plotrix)
require(ggplot2)
require(moments)

#load dataset
library(FSAdata)
data(BullTroutRML2)

#display all content of dataset
library(FSAdata)
dataset <- data(BullTroutRML2)
dataset

#print first and last 3 records
head(BullTroutRML2,3)
tail(BullTroutRML2,3)

#remove all records except from Harrison Lake
library(FSAdata)
dataset <- BullTroutRML2
dataset_filtered = filter(dataset, lake=="Harrison")
dataset_filtered

#displaying first and last 5 records from filtered dataset
head(dataset_filtered,5)
tail(dataset_filtered,5)

#display structure of filtered dataset
str(dataset_filtered)

#display summary of filtered dataset
summary(dataset_filtered)

#create scatterplot
ggplot(dataset_filtered, aes(x=fl , y=age))+
  scale_x_continuous(breaks = seq(0,500,100))+
  scale_y_continuous(breaks = seq(0,15,5))+
  geom_point()+
  labs (
    title = "Plot 1: Harrison Lake Trout",
    x = "Fork Length (mm)",
    y = "Age (yrs)"
  ) 

#plot histogram
hist (dataset_filtered$age, 
      xlab ='Age (yrs)', 
      ylab='Frequency', 
      col='cadetblue',
      breaks=c(seq(0,15)),
      main='Plot 2: Harrison Fish Age Distribution',
      col.main="cadetblue")

#overdense plot
ggplot(dataset_filtered, aes(x=fl , y=age))+
  scale_x_continuous(breaks = seq(0,500,100))+
  scale_y_continuous(breaks = seq(0,15,5))+
  geom_point(color="green")+
  labs (
    title = "Plot 3: Harrison Density Shaded by Era",
    x = "Fork Length (mm)",
    y = "Age (yrs)"
  )

#creating new object having first and last 3 records of BullTroutRML2 dataset using list
first_record <- head(BullTroutRML2,3)
last_record <- tail(BullTroutRML2,3)
tmp <- list(first_record,last_record)
tmp

#creating new object having first and last 3 records of BullTroutRML2 dataset using dataframe
record1 <- head(BullTroutRML2,3)
record2 <- tail(BullTroutRML2,3)
tmp <- rbind.data.frame(record1,record2)
tmp

#display era column in new object tmp
record1 <- head(BullTroutRML2,3)
record2 <- tail(BullTroutRML2,3)
tmp <- rbind.data.frame(record1,record2)
tmp
select(tmp,era)

#Create a pchs vector with the argument values for + and x.
pchs <- c("+","x")
pchs

#Create a cols vector with the two elements "red" and "gray60"
cols <- c("red","gray60")
cols

#Convert the tmp era values to numeric values.
tmp$era <- as.numeric(tmp$era)
tmp

#Initialize the cols vector with the tmp era values
tmp$era <- as.numeric(tmp$era)
print(initialize(cols,tmp$era))

#symbol and color by era plot
pchs <- c("+","x")
cols <- c("red","gray60")
pchs <- pchs[as.numeric(dataset_filtered$era)]
cols <- cols[as.numeric(dataset_filtered$era)]
ggplot(dataset_filtered, aes(x=fl , y=age))+
  scale_x_continuous(breaks = seq(0,500,100))+
  scale_y_continuous(breaks = seq(0,15,5))+
  geom_point(pch = pchs, col = cols)+
  labs (
    title = "Plot 4: Symbol & Color by Era",
    x = "Fork Length (mm)",
    y = "Age (yrs)"
  )

#regression line overlay plot
pchs <- c("+","x")
cols <- c("red","gray60")
pchs <- pchs[as.numeric(dataset_filtered$era)]
cols <- cols[as.numeric(dataset_filtered$era)]
ggplot(dataset_filtered, aes(x=fl , y=age))+
  scale_x_continuous(breaks = seq(0,500,100))+
  scale_y_continuous(breaks = seq(0,15,5))+
  geom_point(pch = pchs, col = cols)+
  geom_smooth(method="lm",col="black", se=FALSE)+
  labs (
    title = "Plot 5: Regression Overlay",
    x = "Fork Length (mm)",
    y = "Age (yrs)"
  )

#legend overlay plot
pchs <- c("+","x")
cols <- c("red","gray60")
pchs <- pchs[as.numeric(dataset_filtered$era)]
cols <- cols[as.numeric(dataset_filtered$era)]
ggplot(dataset_filtered, aes(x=fl , y=age))+
  scale_x_continuous(breaks = seq(0,500,100))+
  scale_y_continuous(breaks = seq(0,15,5))+
  geom_point(pch = pchs, col = cols)+
  geom_smooth(method="lm",col="black", se=FALSE)+
  labs (
    title = "Plot 6: Legend Overlay",
    x = "Fork Length (mm)",
    y = "Age (yrs)"
  )
legend("topleft", legend=c("1977-80","1997-01"), pch = pchs, col=cols)

