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

bank = read.csv("~/Downloads/Archive (1)/bank.csv")
bank

# Sorted dataframe with descending age
bank[order(-bank$age),]

#Drop column 'poutcome'
bank_drop <- bank[, -16]
bank_drop

# remove any 'NA' value if exsit
na.omit(bank_drop)

# Rename column 
names(bank_drop)[names(bank_drop) == 'contact'] <- 'Contact_Info'
head(bank_drop)

# Replace column string to Captial letters using GSUB
bank_drop$marital <- gsub("single","SINGLE",as.character(bank$marital))
bank_drop$marital <- gsub("married","MARRIED",as.character(bank$marital))
bank_drop$marital <- gsub("divorced","DIVORCED",as.character(bank$marital))
head(bank_drop)

bank <- dim(bank_drop)
bank

# get the string and summary of bank dataset
str(bank_drop)
summary(bank_drop)


# create histogram for balance of the customers account

ggplot(bank_drop, aes(x = factor(1), y = balance)) +
  geom_jitter(aes(color = "balance"), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label


#We want to know, if their balance of their account is higher than 1000 (right-tailed test)?

#H0= The account balance is equal or lower than 1000
#H1= The account balance is more than 1000
  

#First get the mean of 'balance'
mean(bank_drop$balance)

# One-sample t-test
test1 <- t.test(bank_drop$balance, mu = 1000 , alternative = 'greater')
# Printing the results
test1 


mean(bank_drop$balance)
median(bank_drop$balance)

ggplot(bank_drop,                  # Draw all densities in ggplot2 plot
       aes(balance,
           fill = balance)) +
  geom_density(alpha = 1)

attributes(test1) #get the attributes in test1 (t test)

test1$conf.int #extract "conf.int" attribute from test1

test1$null.value #extract "null.value" attribute from test1
test1$parameter #extract "parameter" attribute from test1

#getting separate vectors
Management <- subset(bank_drop, job == "management") #First need to get separate vectors for jobs to management only
Management

tertiary <- subset(bank_drop, education == "tertiary") #First need to get separate vectors for education to tertiary only
tertiary

#get two sample test for education and jobs to see if those who work in management sector have university degree or not

# H0= Management job need university degree.
# H1= Management job may not need university degree.

test2 <- t.test(Management$age, tertiary$age,  mu= 0 ,conf.level = 0.05) #two sample test
test2

#create ggplot for managrment job and education
ggplot(data = Management) + geom_bar(mapping = aes(x=education, fill= job), position="dodge") + labs(title="Impact of different education level on Management", x ="Education", y = "count")

ggplot(bank_drop,                  # Draw all densities in ggplot2 plot
       aes(education,
           fill = "tertiary")) +
  geom_density(alpha = 1)

attributes(test2) #get the attributes in test2 (t test)

test2$conf.int #extract "conf.int" attribute from test2


test2$null.value #extract "null.value" attribute from test2

test2$parameter #extract "parameter" attribute from test2

# get separate vectors
Divorced <- subset(bank_drop, marital == "DIVORCED") #First need to get separate vectors for marital to divorced only
Divorced

Ages <- subset(bank_drop, age < 40) #then need to get separate vectors for age below 40 years old
Ages

#get two sample test for divorced people to see if it happens below 40 years old or not

# H0= Most of the divorce happened at 40 years old or below
# H1= Most of the divorce happened after 40 years old 

test3 <- t.test(Divorced$age, Ages$age,mu=0, conf.level = 0.05) #two sample test
test3

#create ggplot for marital status based on ages
ggplot(data = Divorced) + geom_bar(mapping = aes(x=age, fill= marital), position="dodge") + labs(title="Impact of different education level on Management", x ="Age ", y = "count")

ggplot(bank_drop,                  # Draw all densities in ggplot2 plot
       aes(marital,
           fill = "divorced")) +
  geom_density(alpha = 1)

attributes(test3) #get the attributes in test3 (t test)

test3$conf.int #extract "conf.int" attribute from test3


test3$null.value #extract "null.value" attribute from test3

test3$parameter #extract "parameter" attribute from test3

## NA
