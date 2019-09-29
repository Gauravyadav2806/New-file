setwd("D://")
getwd()

df<-read.csv("Demog_Data.csv")
summary(df)
library(readr)
head(df)
demo<- df
head(demo)
tail(demo )
rm(df)
# cntrl L to remove  everything from console
dim(demo) # dim function is for checking dimension
names(demo) # to check the names of the dataframe (only for dataframe)
colnames(demo) # colnames can be applied to table and matrix as well along with dataframe
calories <- demo$Cal..Intake..Cal.
names(demo)
colnames(demo)[4] <- "cal" # to change the name of the columns number wise
names(demo)
colnames(demo)[5] <- "Weight"
names(demo)demo$Age_Cat <- "Age_Category" # to change the column name by entering the name
names(demo)

class(demo$Gender)
class(demo$Age_Cat)
summary(demo$Gender)
# A character is taken as a nominal variable
# R do not understand character variable so we have to convert them into factor
demo$Gender <- as.character(demo$Gender) # to change into character
class(demo$Gender)

demo$Gender <- as.factor(demo$Gender) # to change into factor
class(demo$Gender)

head(demo,20)
# how to change the value of a variable in the dataset
edit(demo)
head(demo ,20)
# it will change the value in data only and will not fix the changed variable

fix(demo)
head(demo ,20)
# to change the value of the data peramanently in the main data

demo_1<- edit(demo) # to make change in main data and make new data  by making changes

rm(demo_1)
demo[4,5] # value of 4th row and 5th column
demo[19,4] # value is 2000
demo[19,4] <- 1700 # to change the value directly by row and column
demo[19,4] # value has become 1700

demo[1:10, 1:3]
demo[c(5,8,75), c(3,5)] # to see the 5th,8th,75th and 3rd, 5th column
summary(demo)
library(psych)
describe(demo) # library pysch is used
describe(demo$cal)

mean(demo$Weight)
mean(demo$Weight, trim = 0.5)
 
str(demo) # to see the structure of the data
summary(demo$Age_Cat)

table(demo$Gender, demo$Age_Cat) # to check cross tabulation of categorical variables
tapply(demo$Weight,demo$Gender, FUN = mean) # to find the mean of two different varibales
tapply(demo$Weight,demo$Gender, FUN = function(x) {sd(x/mean(x))}) # to make the function according to the need


# Second day

