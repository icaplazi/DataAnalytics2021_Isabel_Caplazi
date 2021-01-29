install.packages("MASS") # install MASS package

library(MASS)
attach(Boston)

?Boston # help function
head(Boston) # show head of dataset
dim(Boston) # dimensions of dataset
names(Boston) # column names
str(Boston) # structure of the dataset
nrow(Boston) # number of rows
ncol(Boston) # number of columns
summary(Boston) # summary stats of dataset
summary(Boston$crim) # summary of the "crime" column
summary(Boston$age) # summary of the "age" column


install.packages("ISLR") # install ISLR

library(ISLR)
data(Auto)
head(Auto, 10) # first 10 rows
tail(Auto, 6) # last 6 rows
names(Auto)

help("Auto") # same as ?Auto
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)

summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)



help("read.csv")
data1 <-read.csv(file.choose(), header = TRUE) # data1 = EPI data
data1

View(data1) # opens data up in another tab
summary(data1$EPI)
fivenum(data1$EPI,na.rm=TRUE)

