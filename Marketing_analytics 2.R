setwd("D://")
getwd()

df<-read.csv("Demog_Data.csv")
summary(df)
library(readr)
head(df)
demo1<- demo[, 1:3]
head(demo1)
View(demo1)
str(demo1)

# how to extract any two variable from the dataset
Gender <- demo$Gender
Weight <- demo$Weight
class(Gender)
class(Weight)
cbind(Gender,Weight)

demo2 <- cbind(Gender,Weight)
str(demo2)
demo2<- as.data.frame(demo2)
demo2$Weight <- as.numeric(demo2$Weight)
str(demo2)
View(demo2)

# Mean weight of male and mean weight of female
tapply(demo2$Weight, demo2$Gender, FUN = mean)

head(demo)
# how to find men weight of femle with occupation. (Three conditions in total)
aggregate(demo$Weight ~ demo$Gender, FUN = mean)

aggregate(demo$Weight ~ demo$Gender+demo$Occupation, FUN = mean)
aggregate(demo$Weight ~ demo$Gender+demo$Occupation+demo$Age_Cat, FUN = mean)

aggregate(demo$Weight~ demo$Gender+demo$Age_Cat+demo$Occupation, FUN = sd)
a <- aggregate(demo$Weight~ demo$Gender+demo$Age_Cat+demo$Occupation, FUN = sd)
summary(a)

# how to remove outliers
head(Weight)
summary(Weight)
Weight1 <- c(Weight, 120, 135)
summary(Weight1)

boxplot(Weight, main="Boxplot of Weight", col="red")
benchmark <- 72.28 + 1.5*IQR(Weight)
benchmark  # 89.5675 is the outlier in this case

# Now getting the data ny removing the outliers

new_weight <- Weight[Weight<benchmark]
summary(new_weight)
boxplot(new_weight)

install.packages("Hmisc")
install.packages("mice")
install.packages("VIM")
library(Hmisc)
library(mice)
library(VIM)

# imort missing sheet
fix(missing)
md.pattern(missing) # to find the pattern of missing value and know in which column misiing values are present
md.pairs(missing)

require(VIM)
aggr(missing)
aggr(missing, las=1, numbers=T) # part of exploratory data analysis

require(Hmisc)
# impute data in the missing sheet
new_data<- with(missing,impute(missing$x1, mean)) 
new_data

# how to make decimal number into whole number
new_data<- round(with(missing,impute(missing$x1, mean)),1)
new_data
# impute with medain
new_data<- round(with(missing,impute(missing$x1, median)),1)
new_data
# impute in row 2
new_data1<- round(with(missing,impute(missing$x2, mean)),1)
new_data1

#multiple imputation
cor_data <- mice(missing, m=5, maxit = 7, method = "pmm")
complete(cor_data)
