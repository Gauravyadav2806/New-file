setwd("D://")
getwd()

df1<-read.csv("bike_buyers.csv")
summary(df1)
library(readr)
head(df1)
tail(df1 )
dim(df1) # dim function is for checking dimension
colnames(df1) # to check the names of the dataframe (only for dataframe)

# tocheck the class of the variables whether they are categorical or factor
# if variables are not factor then we have to convert them to factor by using as.factor
class(df1$Marital.Status)
class(df1$Education)
class(df1$Occupation)
class(df1$Home.Owner)

# to get the view of the data and check the values
edit(df1)
head(df1 ,20)

library(psych)
describe(df1) # library pysch is used
# there os no missinf value in the data
describe(df1$Income)

# To check the mean of any variable
mean(df1$Income)
# to check the trimmed mean of variable
mean(df1$Income, trim = 0.5)

str(df1) # to see the structure of the data

# to check the variables and their distribution in their levels
summary(df1$Marital.Status)
summary(df1$Home.Owner)
summary(df1$Income)
summary(df1$Education)
summary(df1$Occupation)

table(df1$Education, df1$Home.Owner) # to check cross tabulation of categorical variables
tapply(df1$Income,df1$Occupation, FUN = mean) # to find the mean of two different varibales
tapply(df1$Age,df1$Purchased.Bike, FUN = function(x) {sd(x/mean(x))}) # to find the standard deviation

aggregate(df1$Age ~ df1$Education+df1$Home.Owner+df1$Purchased.Bike, FUN = mean)

# Removing outliers

summary(df1)

head(df1$Commute.Distance)
summary(df1$Children)

boxplot(df1$Income, main="Boxplot of Weight", col="red") # there is outlier in the data
benchmark1 <- 70000 + 1.5*IQR(df1$Income) # setting bechmark or limit for upper limit outlier
benchmark1

new_income <- df1$Income[df1$Income<benchmark1]
summary(new_income)
boxplot(new_income)

boxplot(df1$Children, main="Boxplot of Weight", col="red") # there is no outlier in this variable
boxplot(df1$Cars, main="Boxplot of Weight", col="red") # There is outlier in the variable
benchmark2 <- 2 + 1.5*IQR(df1$Cars)
benchmark2

new_car <- df1$Cars[df1$Cars<benchmark2]
summary(new_car)
boxplot(new_car)

boxplot(df1$Age, main="Boxplot of Weight", col="red")
benchmark3 <- 52 + 1.5*IQR(df1$Age)
benchmark3

new_age <- df1$Age[df1$Age<benchmark3]
summary(new_age)
boxplot(new_age)

install.packages("Hmisc")
install.packages("mice")
install.packages("VIM")
library(Hmisc)
library(mice)
library(VIM)

require(VIM)
aggr(df1)
aggr(df1, las=1, numbers=T) # part of exploratory data analysis


df1$Children[df1$Gender=="F"]

# how to find the frequency of categorical variables
table(df1$Education)
table(df1$Occupation)

# how to get table proportion wise
prop.table(table(df1$Purchased.Bike, df1$Occupation), margin=1)# proportion by row
prop.table(table(df1$Purchased.Bike, df1$Occupation), margin=2)# proportion by column
# first table by row tells us that, out of people who own bike, 18.29% have clerical job
# 15.17% of people are in management, 11.43% have manual occupation,31.11% are proffessional and 23.9% are skilled mannual
# Similarly, in second table out of clerical people 50.28% people owns bike and 49.71 % does not own a bike.
# Likewise the proportion of other occupation who owns and does not owns a bike is given


# Checking the classification of people according to different variables on purchasing a bike or not
table(df1$Education, df1$Purchased.Bike)
# Bachelors own maximum number of bikes 

# Making barplot for number of people owning a bike education wise
barplot(table(df1$Purchased.Bike, df1$Education), beside = T, col = c(3,4)) # colouring the barplot


table(df1$Marital.Status, df1$Purchased.Bike)
# single people own bike more than married people

# Making barplot for number of people owning a bike marital status wise
barplot(table(df1$Purchased.Bike, df1$Marital.Status), beside = T, col = c(3,4)) # colouring the barplot


table(df1$Children, df1$Purchased.Bike)
# People having no children own maximum niber of bike

# Making barplot for number of people owning a bike number of children wise
barplot(table(df1$Purchased.Bike, df1$Children), beside = T, col = c(3,4)) # colouring the barplot


table(df1$Occupation, df1$Purchased.Bike)
# proffessional prople owns maximum nuber of bikes and manual people owns minimum number of file

# Making barplot for number of people owning a bike occupation wise
barplot(table(df1$Purchased.Bike, df1$Occupation), beside = T, col = c(3,4)) # colouring the barplot

table(df1$Home.Owner, df1$Purchased.Bike)
# More number of people purchase bike who owns a house

# Making barplot for number of people owning a bike homw owner wise
barplot(table(df1$Purchased.Bike, df1$Home.Owner), beside = T, col = c(3,4)) # colouring the barplot


table(df1$Cars, df1$Purchased.Bike)
# People having only one owns maximum number of bikes 152. As number of cars are more bike purchased is less

# Making barplot for number of people owning a bike numver of cars wise
barplot(table(df1$Purchased.Bike, df1$Cars), beside = T, col = c(3,4)) # colouring the barplot


table(df1$Region, df1$Purchased.Bike)
# People of North AMerica owns more number of bikes than other region

# Making barplot for number of people owning a bike region wise
barplot(table(df1$Purchased.Bike, df1$Region), beside = T, col = c(3,4)) # colouring the barplot

install.packages("lattice")
library(lattice)

# in lattice the function of barplot is histogram
# Making the histogram of people purchasing a bike according to their occupation
histogram(~df1$Purchased.Bike|df1$Occupation , data = df1, col = c("red", "green"))

# making the hsitogram of people owning a bike according to gender
histogram(~df1$Purchased.Bike|df1$Gender , data = df1, col = c("red", "green"))

# Making the histogram of people owning a bike region wise
histogram(~df1$Purchased.Bike|df1$Region , data = df1, col = c("red", "green"))

# Making the histogram of people owning a bike according to home owner wise
histogram(~df1$Purchased.Bike|df1$Home.Owner , data = df1, col = c("red", "green"))


# to plot the scatter plot between two continous variable
# first argument comes on x axis and second argument on y axis
plot(df1$Income,df1$Age, col = "red", main = "Scatter plot o Incomes vs Age")

# to make the line chart 
abline(h =mean(df1$Cars), col = "green")
abline(v =mean(df1$Age), col = "blue")
