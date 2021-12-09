#print Name
print("Richa")

#install package
install.packages("vcd")

#import package
require(vcd)

#scatter plot
sales <- c(8,11,15,20,21,11,18,10,6,22)
temp <-  c(69,80,77,84,80,77,87,70,65,90)
plot(sales,temp)

#finding mean of temp
mean(temp)

#deleting 3rd element from sales vector
sales <- c(8,11,15,20,21,11,18,10,6,22)
sales <- sales[-3]
sales


#inserting element into sales vector
sales <- c(8,11,15,20,21,11,18,10,6,22)
sales <- sales[-3]
append(sales,16,2)


#creating a vector
names <- c("Tom","Dick","Harry")
names

#creating a matrix
x <- matrix (1:10, nrow=5, ncol=2)
x

#creating a dataframe
sales <- c(8,11,15,20,21,11,18,10,6,22)
temp <-  c(69,80,77,84,80,77,87,70,65,90)
icSales <- data.frame(sales,temp)
icSales


#displaying the dataframe structure
sales <- c(8,11,15,20,21,11,18,10,6,22)
temp <-  c(69,80,77,84,80,77,87,70,65,90)
icSales <- data.frame(sales,temp)
icSales
str(icSales)


#displaying the dataframe summary
sales <- c(8,11,15,20,21,11,18,10,6,22)
temp <-  c(69,80,77,84,80,77,87,70,65,90)
icSales <- data.frame(sales,temp)
icSales
str(icSales)
summary(icSales)

#importing the student.csv dataset
student_data <- read.csv("~/Desktop/student.csv")
head(student_data)


#displaying variable names of the dataset
student_data <- read.csv("~/Desktop/student.csv")
attributes(student_data)
