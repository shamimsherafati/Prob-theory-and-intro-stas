#install.packages("Hmisc")
#install.packages("corrplot")
#install.packages("GGaly")
#install.packages("corrgram")
#library(GGaly)
#install.packages("devtools")
#install.packages("ggiraphExtra")
library(ggiraphExtra)
library(devtools)
library(corrgram)
library(corrplot)
library("Hmisc")
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

data1 = read.csv("~/Downloads/Module-5-6-R-Practice-Assignment-Data.csv", header = TRUE)
data1

# Rename column 
names(data1)[names(data1) == 'hrt_months'] <- 'hormone.therapy' #hrt stands for hormone.replacement.therapy
names(data1)[names(data1) == 'wbc'] <- 'White.blood.cells'

head(data1)

# delete 'NA' value if exist
data1new <- na.omit(data1)
data1new

summary(data1new) # Final summary
str(data1new) #Final String of dataset

view(data1new) #access to data
dim(data1new) #number of obs. and number of variables

# use ifelse function in creating a dummy variable and a categorical variable

#create a dummy variable for White.blood.cells , 1 denoting high White.blood.cells ; 0 denoting low White.blood.cells 
summary(data1new$White.blood.cells ) # summary statistics White.blood.cells 
data1new$White.blood.cells_dummy =  ifelse(data1new$White.blood.cells > 6.40,1,0 )

names(data1new) # check the variables in data
table(data1new$White.blood.cells_dummy) #a frequency table

#create a categorical variable, high/low
data1new$White.blood.cells_cat = ifelse(data1new$White.blood.cells > 6.40, "high", "low")
table(data1new$White.blood.cells_cat) #a frequency table

#create a categorical variable, high/mid/low
data1new$White.blood.cells_3cat = ifelse(data1new$White.blood.cells>7.40, "high",
                                       ifelse(data1new$White.blood.cells<5.30, "low", "mid"))

table (data1new$White.blood.cells_3cat)


# Subset for having low White.blood.cells
Sub1<- filter(data1new, White.blood.cells_dummy == 0)
Sub1

# Subset for having high White.blood.cells
Sub2 <- filter(data1new,White.blood.cells_dummy ==1)
Sub2

# Checking for linearity
plot(White.blood.cells_dummy ~ smoker, data=data1new,
main= "Smoking Vs White.blood.cells PLot",
col="red",ylab = "White.blood.cells_dummy ",pch = 12)



# use ifelse function in creating a dummy variable and a categorical variable

#create a dummy variable for outcome , 1 denoting high outcome ; 0 denoting low outcome 
summary(data1new$outcome ) # summary statistics outcome 
data1new$outcome_dummy =  ifelse(data1new$outcome > 10.60,1,0 )

names(data1new) # check the variables in data
table(data1new$outcome_dummy) #a frequency table

#create a categorical variable, high/low
data1new$outcome_cat = ifelse(data1new$outcome > 10.60, "high", "low")
table(data1new$outcome_cat) #a frequency table

#create a categorical variable, high/mid/low
data1new$outcome_3cat = ifelse(data1new$outcome>17.62, "high",
                                       ifelse(data1new$outcome< 5.25 , "low", "mid"))

table (data1new$outcome_3cat)


# Subset for negative outcome
Sub3 <- filter(data1new, outcome_dummy == 0)
Sub3

# Subset for positive outcome
Sub4 <- filter(data1new,outcome_dummy ==1)
Sub4 

# Checking for linearity
plot(outcome_dummy ~ smoker, data=data1new,
main= "Smoking Vs outcome_dummy PLot",
col="green",ylab = "outcome_dummy ",pch = 12)

#create a model for regression for white blood cell's dummy and categorical variables
mod <- lm(data1new$White.blood.cells_dummy ~ data1new$outcome_dummy) # create model

summary(mod) #summary of the model

cor(data1new$outcome_dummy, data1new$White.blood.cells_dummy) # get the correlation

# H0: Y = b0
# H1: Y = b0+b1X

#Q-Q Plot for white blood cell's dummy variables
qqnorm(data1new$White.blood.cells_dummy, pch = 1, frame = FALSE)
qqline(data1new$White.blood.cells_dummy, col = "green", lwd = 2)

#run a correlation in R is with the cor function
box1 <- boxplot( data1new$White.blood.cells_dummy ~ data1new$smoker, main="Compare White.blood.cells_dummy and smoker", xlab = "smoker", ylab = "White.blood.cells_dummy ", col = "purple")

# Normality test for smoking
hist(data1new$smoker,main = "Checking for normality of smoking", col = "pink")

# Normality test for White.blood.cells
hist(data1new$White.blood.cells_dummy,main = "Checking for normality of White.blood.cells_dummy data",
col = "red",border = "cadetblue",xlab = "White.blood.cells_dummy Data")

# Normality test for outcome
hist(data1new$outcome_dummy,
main = "Checking for normality of outcome_dummy data",
col = "steelblue",border = "cadetblue",
xlab = "outcome dummy Data")

#combined all the dummy and independant variable together
combinedSummaryModel1 <-lm(smoker ~
White.blood.cells_dummy +            #the dummy variables
outcome_dummy, 
data = data1new)       #the dataset

#combined all the dummy and independant variable together
combinedSummaryModel2<-lm(smoker ~
White.blood.cells_dummy +            #the dummy variables
outcome_dummy +
  White.blood.cells_3cat, 
data = data1new) 


#combined all the dummy and independant variable together
combinedSummaryModel3<-lm(smoker ~
White.blood.cells_dummy +            #the dummy variables
White.blood.cells_3cat +
  outcome_3cat, 
data = data1new) 


summary(combinedSummaryModel1)  # getting the summary
summary(combinedSummaryModel2)  # getting the summary
summary(combinedSummaryModel3)  # getting the summary

ggPredict(combinedSummaryModel1,interactive = FALSE)
ggPredict(combinedSummaryModel2,interactive = FALSE)
ggPredict(combinedSummaryModel3,interactive = FALSE)

#creating plot for the trend of the hormone.therapy and White.blood.cells and their relation
ggplot(data1new, aes(White.blood.cells_dummy, outcome_dummy)) +
  geom_point() +
  stat_smooth(method = lm)

outcome_dummy = data1new$outcome_dummy 
White.blood.cells_dummy = data1new$White.blood.cells_dummy 
mdata1 <- cbind.data.frame(outcome_dummy,White.blood.cells_dummy)

#creating plot for these two variables
p1 <- ggplot(mdata1, aes(x=outcome_dummy, y=White.blood.cells_dummy))+
  geom_point(color="black")+
  theme_bw()
p1

#add regression line
p2 <- p1 + geom_smooth(method = lm, color="red", se= FALSE)
p2

#get the Standard Error of Estimate
p3 <- p1 + geom_smooth(method=lm, color= "pink", fill= "pink",se= TRUE)
p3

## NA
