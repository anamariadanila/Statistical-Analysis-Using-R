library(lubridate)
library(stringr)
library(foreign)
library(dplyr)
library(tidyverse)
library(tidyr)
library(editrules)
library(data.table)
library(psych)
library(ggplot2)

#setting the working directory
setwd("/Users/anadanila/Documents/GitHub/Statistical-Analysis-Using-R")

#verify if the directory was set correctly
getwd()

#import the database 
#read the file using the function read.csv
house_offers <- read.csv('house_offers.csv')

#check if the database was imported
view(house_offers)


# 1. Preliminary operations


#one of the non-numeric variables will be obtained by transforming a numeric variable into a factor type one
#variable 'price' will be transformed in the factor type variable called 'price_range'
#check the type for 'price'

class(house_offers$price)

#it is of type integer 
# by using the summary function we get a list of information for the variable price 
summary(house_offers$price)

#for creating the factory variable price_range we can use the m
#we can divide the data into three categories as follows: the first low price category will be houses with values between min value and 1st quartile, 
#medium price between 1st quartile and 3rd quartile and high price between 3rd quartile and max value
house_offers$price_range <- cut(house_offers$price, c(10000 - 0.1, 65000 - 0.1, 142000 - 0.1, 3880000 + 1 ), c("mic", "mediu", "mare"))

#check the type for price_range
class(house_offers$price_range)


#initial number of registrations
nrow(house_offers) #8320

#selection over price and bathrooms_count variables
house_offers_new <- subset(house_offers, price <= 1000000 & bathrooms_count >= 1)

#check the current number of registrations
nrow(house_offers_new) #7937

#removal of variables that are no longer needed
#first we display all variables
names(house_offers_new)

#we need the variables on positions 6, 8, 9, 10, 11, 18, 22 and the rest we will eliminated
house_offers_new <- house_offers_new[, -c(1,2,3,4,5,7,12,13,14,15,16,17,19,20,21)]

#check the variables again
names(house_offers_new)

#verify the type of variables
str(house_offers_new)


#'price_range' has the correct type but 'partitioning' does not have the correct type
#change it from character to factor
house_offers_new$partitioning <- as.factor(house_offers_new$partitioning)

#check again the types
str(house_offers_new)

#verify if the factory variables have the categories well defined
summary(house_offers_new$partitioning)
summary(house_offers_new$price_range)

#verify if in the database are NULL and NA values
sum(is.null(house_offers_new))
sum(is.na(house_offers_new))

#remove the rows with NA values
house_offers_new <- na.omit(house_offers_new)

sum(is.na(house_offers_new))

nrow(house_offers_new) #7253

#export the new database created
write.csv(house_offers_new, "house_offers_new.csv")

# 2. Database presentation 
#database description

#show data from database
glimpse(house_offers_new)

#variables' classes
sapply(house_offers_new, class)

#summary for each variable
sapply(house_offers_new, summary)

#variables' names
names(house_offers_new)

#database dimension
dim(house_offers_new)

# 3. Graphical and numerical analysis of the analysed variables

# 3.1. Descriptive numerical analysis of numeric and non-numeric variables

#for numerical values

summary(house_offers_new$price)
summary(house_offers_new$rooms_count)
summary(house_offers_new$useful_surface)
summary(house_offers_new$built_surface)
summary(house_offers_new$bathrooms_count)

#example of a result interpretation: for price variable
# Min: The lowest price of a house is 17000 euro
# 1st Quartile: 25% of houses were priced less than 65000 euro, and 75% of houses were priced more than 65000 euro
# Median (2nd Quartile): 50% of houses were priced less than 89000 euro and 50% were priced more than 89000 euro
# Mean: The average price of a house is 128526 euro
# 3rd Quartila: 75% of houses were priced less than 141000 euro and 25% of houses were priced more than 141000 euro
# Max: The highest price of a house is 1000000 euro

#analysis of descriptive indicators by groups

#for price_range 

#price
tapply(house_offers_new$price, house_offers_new$price_range, mean)
describeBy(house_offers_new$price, house_offers_new$price_range)

#rooms_count
tapply(house_offers_new$rooms_count, house_offers_new$price_range, mean)
describeBy(house_offers_new$rooms_count, house_offers_new$price_range)

#useful_surface
tapply(house_offers_new$useful_surface, house_offers_new$price_range, mean)
describeBy(house_offers_new$useful_surface, house_offers_new$price_range)

#built_surface
tapply(house_offers_new$built_surface, house_offers_new$price_range, mean)
describeBy(house_offers_new$built_surface, house_offers_new$price_range)

#bathrooms_count
tapply(house_offers_new$bathrooms_count, house_offers_new$price_range, mean)
describeBy(house_offers_new$bathrooms_count, house_offers_new$price_range)

#for partitioning

#price
tapply(house_offers_new$price, house_offers_new$partitioning, mean)
describeBy(house_offers_new$price, house_offers_new$partitioning)

#rooms_count
tapply(house_offers_new$rooms_count, house_offers_new$partitioning, mean)
describeBy(house_offers_new$rooms_count, house_offers_new$partitioning)

#useful_surface
tapply(house_offers_new$useful_surface, house_offers_new$partitioning, mean)
describeBy(house_offers_new$useful_surface, house_offers_new$partitioning)

#built_surface
tapply(house_offers_new$built_surface, house_offers_new$partitioning, mean)
describeBy(house_offers_new$built_surface, house_offers_new$partitioning)

#bathrooms_count
tapply(house_offers_new$bathrooms_count, house_offers_new$partitioning, mean)
describeBy(house_offers_new$bathrooms_count, house_offers_new$partitioning)

#analysis of non-numeric variables

#partitioning variable
table(house_offers_new$partitioning)
table(house_offers_new$partitioning)/length(house_offers_new$partitioning)

#price_range variable
table(house_offers_new$price_range)
table(house_offers_new$price_range)/length(house_offers_new$price_range)

#less common category for non-numeric variables
table(house_offers_new$price_range)[table(house_offers_new$price_range) == min(table(house_offers_new$price_range))]
table(house_offers_new$partitioning)[table(house_offers_new$partitioning) == min(table(house_offers_new$partitioning))]

#most common category
table(house_offers_new$price_range)[table(house_offers_new$price_range) == max(table(house_offers_new$price_range))]
table(house_offers_new$partitioning)[table(house_offers_new$partitioning) == max(table(house_offers_new$partitioning))]


# 3.2. Graphical analysis of numeric and non-numeric variables

#for numeric variables; we use the hist function to create the histograms
hist(house_offers_new$price, col = 'orange', main = paste('"price" variable distribution'), xlab = 'value')
hist(house_offers_new$rooms_count, col = 'orange', main = paste('"rooms_count" variable distribution'), xlab = 'value')
hist(house_offers_new$useful_surface, col = 'orange', main = paste('"useful_surface" variable distribution'), xlab = 'value')
hist(house_offers_new$built_surface, col = 'orange', main = paste('"built_surface" variable distribution'), xlab = 'value')
hist(house_offers_new$bathrooms_count, col = 'orange', main = paste('"bathrooms_count" variable distribution'), xlab = 'value')

#for factory variables
ggplot(house_offers_new, aes(partitioning)) + geom_bar(fill = 'orange') + labs(x='value', y='frequency')
ggplot(house_offers_new, aes(price_range)) + geom_bar(fill = 'orange') + labs(x='value', y='frequency')


# 3.3. Identification of outliers and their elimination from the base (or their replacement with missing values) 
#for this step we will use the boxplot function

boxplot(house_offers_new$price, horizontal = TRUE)
boxplot(house_offers_new$price, horizontal = TRUE, range = 12)
boxplot(house_offers_new$rooms_count, horizontal = TRUE)
boxplot(house_offers_new$built_surface, horizontal = TRUE)
boxplot(house_offers_new$useful_surface, horizontal = TRUE)
boxplot(house_offers_new$bathrooms_count, horizontal = TRUE)
