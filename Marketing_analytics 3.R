setwd("D://")
getwd()

df<-read.csv("Demog_Data.csv")
summary(df)
demo <- df
boxplot(demo$Weight..Kg.)
boxplot(demo$Weight..Kg., horizontal = T)
boxplot(demo$Weight..Kg.~demo$Gender)
boxplot(demo$Weight..Kg.~demo$Gender, col = "red", "blue")
boxplot(demo$Weight..Kg.~demo$Gender, col = c(2,3))
boxplot(demo$Weight..Kg.~demo$Gender, col = c(2,3), horizontal = T) # In male there is no outlier of weight
# lower whisker = 1st quartile - 1.5* IQR, Upper whisker = 3rd quartile + 1.5*IQR
# In female there is outlier and we need to remove it
demo$Weight..Kg.[demo$Gender=="F"] # to find the weight of female only
summary(demo)
benchmark <- 71.25 + 1.5*IQR(demo$Weight..Kg.)
benchmark1 <- 60.25 - 1.5*IQR(demo$Weight..Kg.)
new_weight <- demo$Weight..Kg.[demo$Weight..Kg.<benchmark & demo$Weight..Kg.>benchmark1]
summary(new_weight)
boxplot(new_weight)

# how to find the mean weight of female whose occulpation is govt employee
mean(demo$Weight..Kg.[demo $Gender=="F" & demo$Occupation=="Govt Employee"])
# to find the number of calories taken by women who are govt employee
median(demo$Cal..Intake..Cal.[demo $Gender=="F" & demo$Occupation=="Govt Employee"])
# how to find the frequency of categorical variables
table(demo$Gender)
table(demo$Age_Cat)
# how to get table proportion wise
prop.table(table(demo$Gender, demo$Occupation), margin=1)# proportion of row
prop.table(table(demo$Gender, demo$Occupation), margin=2)# proportion of column
round(prop.table(table(demo$Gender, demo$Occupation), margin=2),2) # rounding the proportion upto two places
round(prop.table(table(demo$Gender, demo$Occupation), margin=1),2) # rounding the proportion upto two places
# barplot of gender wise ocuupation
barplot(table(demo$Gender, demo$Occupation))
# beside is used to unstack the bar and make different bar of female and male
barplot(table(demo$Gender, demo$Occupation), beside = T, col = c(3,4)) # colouring the barplot
# Trying to make lables without  installing the package
Gen_col <- c(3,4)
legend("topright", paste("Gender:", levels(demo$Gender), col=Gen_col))
barplot(table(demo$Gender, demo$Occupation), beside = T, col = c(3,4))
# to get the lables of the bargraph, pacakge used is lattice
install.packages("lattice")
library(lattice)
# in lattice the function of barplot is histogram
histogram(~demo$Gender|demo$Occupation , data = demo, col = c("red", "green"))
# to give the actual count of the data, not in percentage as above
histogram(~demo$Gender|demo$Occupation , data = demo, col = c("red", "green"), type = "count")
# to change the layout representation of the data
histogram(~demo$Gender|demo$Occupation , data = demo, col = c("red", "green"), type = "count", layout = c(2,4))
histogram(~demo$Gender|demo$Occupation , data = demo, col = c("red", "green"), type = "count", auto.key = T)

# another function in lattice to make bargraphs is barchart
# to make the frequency of categorical variables
aggregate(demo$Weight..Kg.~demo$Gender, FUN = mean)          
barchart(aggregate(demo$Weight..Kg.~demo$Gender, FUN = mean), col=c(2,3), auto.key = T)

# to plot the scatter plot between two continous variable
# first argument comes on x axis and second argument on y axis
plot(demo$Weight..Kg.,demo$Cal..Intake..Cal., col = "red", main = "Scatter plot o calories vs weight")
# to change the number from vertical to horizontal
plot(demo$Weight..Kg.,demo$Cal..Intake..Cal., col = "red", main = "Scatter plot o calories vs weight", las = 1)
# to provide limit to the data we use ylim and xlim
# to increase the size of the dots we usecex
# to chaange the shape the dots we use pch
plot(demo$Weight..Kg.,demo$Cal..Intake..Cal., col = "red", main = "Scatter plot o calories vs weight", las = 1, ylim = c(1500,3000), xlim = c(30,100), cex =1.3, pch=2)


# to make the line chart 
abline(h =mean(demo$Cal..Intake..Cal.), col = "green")
abline(v =mean(demo$Weight..Kg.), col = "blue")
# by making horizontal and vertical line we make four quadrants and see people with high calorie and high weight
# low calorie and low weight and so on.

# to give different colours to male and female
# to give different shapes to male and female in scatter plot
Gen_col <-c("red", "blue")
Gen_pch <- c(1,2)
plot(demo$Weight..Kg.,demo$Cal..Intake..Cal.,col = Gen_col[demo$Gender], pch = Gen_pch[demo$Gender])
# to give legend to the plot
# gender shuld be factor otherwise it will not work
legend("topright", legend = paste("Gender:", levels(demo$Gender), col= Gen_col), pch = Gen_pch, cex = 0.7)
