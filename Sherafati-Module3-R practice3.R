#install.packages("plyr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("psych")
#install.packages("ggpubr")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("moments")
#install.packages('gmodels')
#install.packages("stringr")


library(plyr) 
library(dplyr)
library(tidyr)
library(tidyverse)
library(psych)
library(ggpubr)
library(ggplot2)
library(plotly)
library(moments)
library(gmodels)
library(stringr)

FoodSurvey = read.csv("~/Documents/Northeastern University/ALY 6010/dataset/Food Survey - Data.csv", header = TRUE)
FoodSurvey

summary(FoodSurvey) #Initial summary

df_foodsurvey <- as.data.frame(read.csv("~/Documents/Northeastern University/ALY 6010/dataset/Food Survey - Data.csv", header = TRUE))
df_foodsurvey

# Rename column 
names(FoodSurvey)[names(FoodSurvey) == 'DHBreakfast'] <- 'breakfast.in.DH'
names(FoodSurvey)[names(FoodSurvey) == 'NDHBreakfast'] <- 'breakfast.NOTin.DH'
names(FoodSurvey)[names(FoodSurvey) == 'BClass'] <- 'breakfast.in.Class'
names(FoodSurvey)[names(FoodSurvey) == 'DHBoxes'] <- 'BoxOfFood.in.DH'
names(FoodSurvey)[names(FoodSurvey) == 'NDHBoxes'] <- 'BoxOfFood.NOTin.DH'
names(FoodSurvey)[names(FoodSurvey) == 'NDHMeals'] <- 'Meal.in.Dorm'

head(FoodSurvey)

# Drop column "Timestamp "
FoodSurvey_drop <- select(FoodSurvey, -1)
head(FoodSurvey_drop)

# delete 'NA' value
Data_FoodSurvey <- na.omit(FoodSurvey_drop)

head(Data_FoodSurvey)

summary(Data_FoodSurvey) #get final summary after cleaning data

str(Data_FoodSurvey)

summary(Data_FoodSurvey$Nutrition)

mean(Data_FoodSurvey$Nutrition) #get the mean of Nutrition
sd (Data_FoodSurvey$Nutrition) #get the Standard Deviation of Nutrition

t.test(Data_FoodSurvey$Nutrition, mu = 0, alternative = "two.sided")

#If the p-value in a null hypothesis is greater than the significance level, then the null hypothesis is accepted
#If the p-value in an alternate hypothesis is smaller than the significance level, then the alternative hypothesis is accepted

ggboxplot(Data_FoodSurvey$Nutrition, 
          ylab = "Nutrition", xlab = FALSE,
          ggtheme = theme_minimal())

shapiro.test(Data_FoodSurvey$Nutrition) # => p-value < 2.2e-16

ggqqplot(Data_FoodSurvey$Nutrition, ylab = "Nutrition",
         ggtheme = theme_minimal()) #From the normality plots, we conclude that the data may come from normal distributions.

#We want to know, if the average Nutrition of the students differs from 5 (two-tailed test)?
# One-sample t-test
test1 <- t.test(Data_FoodSurvey$Nutrition, mu = 5)
# Printing the results
test1 

mean(Data_FoodSurvey$Meal.in.Dorm) #get the mean of Meal.in.Dorm
sd (Data_FoodSurvey$Meal.in.Dorm) #get the Standard Deviation of Meal.in.Dorm

#We want to know, if the average Nutrition of the students greater than 10 (greater-tailed test)?
test2 <- t.test(Data_FoodSurvey$Meal.in.Dorm, mu = 10, alternative = 'greater') #One Sample t-test of Meal.in.Dorm
test2

ggboxplot(Data_FoodSurvey$Meal.in.Dorm, 
          ylab = "Meal.in.Dorm", xlab = FALSE,
          ggtheme = theme_minimal())

shapiro.test(Data_FoodSurvey$Meal.in.Dorm) # => p-value = 1

ggqqplot(Data_FoodSurvey$Meal.in.Dorm, ylab = "Meal in Dorm",
         ggtheme = theme_minimal()) #From the normality plots, we conclude that the data may come from normal distributions.

mean(Data_FoodSurvey$breakfast.in.DH) #get the mean of breakfast.in.Dinng Hall
sd(Data_FoodSurvey$breakfast.in.DH) #get the standard deviation of breakfast.in.Dinng Hall

##We want to know, if the average Nutrition of the students less than 7 (less-tailed test)?
test3 <- t.test(Data_FoodSurvey$breakfast.in.DH, mu = 7, alternative = 'less') #One Sample t-test of breakfast.in.DH with 
test3

ggboxplot(Data_FoodSurvey$breakfast.in.DH, 
          ylab = "breakfast in dining hall", xlab = FALSE,
          ggtheme = theme_minimal())

shapiro.test(Data_FoodSurvey$breakfast.in.DH) # => p-value < 2.2e-16

#We want to know, if the average of breakfast.in.DH of the students less than 7 (left-tailed test)?
# One-sample t-test
test2 <- t.test(Data_FoodSurvey$breakfast.in.DH, mu = 7)
# Printing the results
test2

A <- Data_FoodSurvey$BoxOfFood.NOTin.DH  #create data A for one variable n dataset

B <- Data_FoodSurvey$breakfast.NOTin.DH # #create data B for another variable n dataset

wilcox.test(A,B, paired = TRUE, correct = TRUE)  #Wilcoxon rank sum exact test for A and B 


t = (mean(Data_FoodSurvey$Nutrition)-2.5)/(sd(Data_FoodSurvey$Nutrition)/sqrt(length(Data_FoodSurvey$Nutrition)))
2*pt(-abs(t),df=length(Data_FoodSurvey$Nutrition)-1)
pt(t, df=136, lower.tail=T)
pt(t, df=136, lower.tail=F)

t = (mean(Data_FoodSurvey$breakfast.in.DH)-2.5)/(sd(Data_FoodSurvey$breakfast.in.DH)/sqrt(length(Data_FoodSurvey$breakfast.in.DH)))
2*pt(-abs(t),df=length(Data_FoodSurvey$breakfast.in.DH)-1)
pt(t, df=136, lower.tail=T)
pt(t, df=136, lower.tail=F)

t = (mean(Data_FoodSurvey$Meal.in.Dorm)-2.5)/(sd(Data_FoodSurvey$Meal.in.Dorm)/sqrt(length(Data_FoodSurvey$Meal.in.Dorm)))
2*pt(-abs(t),df=length(Data_FoodSurvey$Meal.in.Dorm)-1)
pt(t, df=136, lower.tail=T)
pt(t, df=136, lower.tail=F)

## NA
