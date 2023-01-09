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
bank_drop <- bank[, -9, -16]
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


#getting separate vectors
AgeSubset <- subset(bank_drop, age > 40) #First need to get separate vectors for age more than 40 years old
AgeSubset

BalanceSubset <- subset(bank_drop, balance > 2000) #First need to get separate vectors for balance more than 2000
BalanceSubset

#get the shapiro test to see if they distributed normal or not
# H0= the data for AgeSubset are normally distributed.
# H1= the data for AgeSubset are not normally distributed.

shapiro.test(AgeSubset$age)

# H0= the data for BalanceSubset are normally distributed.
# H1= the data for BalanceSubset are not normally distributed.

shapiro.test(BalanceSubset$balance)

#Q-Q Plot for age
qqnorm(AgeSubset$age, pch = 1, frame = FALSE)
qqline(AgeSubset$age, col = "green", lwd = 2)

#Q-Q Plot for balance
qqnorm(BalanceSubset$balance, pch = 1, frame = FALSE)
qqline(BalanceSubset$balance, col = "green", lwd = 2)

#ggally function for correlation
GGally::ggpairs(AgeSubset, columns = 6, ggplot2::aes(color= "speciecs")) #checking balance in age subset
GGally::ggpairs(BalanceSubset, columns = 1, ggplot2::aes(color= "speciecs")) #checking age in balance subset

# ggcore function for correlation

GGally::ggcorr(AgeSubset, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE, size=2.5)

GGally::ggcorr(BalanceSubset, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE, size=2.5)

#Correlation table
cor.test(AgeSubset$age [4:8], BalanceSubset$balance [4:8])


#building a mode

#getting the correlation of age and account balance
model1 <- lm(balance ~ age, data = AgeSubset) #building a model for these two variables in AgeSubset subset
model1

model2 <- lm(balance ~ age, data = BalanceSubset) #building a model for these two variables in BalanceSubset subset
model2


#getting the sumary of model which created 
summary(model1) 
summary(model2) 

#getting the conflit of the created model
confint(model1) 
confint(model2) 

sigma(model1)*100/mean(AgeSubset$age)

sigma(model2)*100/mean(BalanceSubset$balance)

boxplot( AgeSubset$age , main="Checking Age  in age subset", xlab = "balance", ylab = "age ", col = "pink")

boxplot(BalanceSubset$balance, main="Checking Balance in balance subset", xlab = "balance", ylab = "age ", col = "purple")



mean(AgeSubset$age)
median(AgeSubset$age)

ggplot(AgeSubset,                  # Draw all densities in ggplot2 plot
       aes(age,
           fill = age)) +
  geom_density(alpha = 1)


mean(BalanceSubset$balance)
median(BalanceSubset$balance)

ggplot(BalanceSubset,                  # Draw all densities in ggplot2 plot
       aes(balance,
           fill = balance)) +
  geom_density(alpha = 1)

#creating plot for the trend of the age and balance and their relation
ggplot(bank_drop, aes(age, balance)) +
  geom_point() +
  stat_smooth(method = lm)

a1 = AgeSubset$age [1:6]
b1 = BalanceSubset$balance [1:6]
mdata1 <- cbind.data.frame(a1,b1)

#creating plot for these two variables
p1 <- ggplot(mdata1, aes(x=a1, y=b1))+
  geom_point(color="black")+
  theme_bw()
p1

#add regression line
p2 <- p1 + geom_smooth(method = lm, color="red", se= FALSE)
p2

#get the Standard Error of Estimate
p3 <- p1 + geom_smooth(method=lm, color= "pink", fill= "pink",se= TRUE)
p3


fit <- lm(a1 ~ b1, data = mdata1) # fit the model
mdata1$predicted <- predict(fit)   # Save the predicted values
mdata1$residuals <- residuals(fit) # Save the residual values
ggplot(mdata1, aes(x = a1, y = b1)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = a1, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

fit1 <- lm(a1~ b1, data= mdata1)

summary(fit1)


res <- residuals(fit1)

fitted(fit1) # line prediction it gives the points through which line is close

residuals(fit1)# epsilon thst is the distance between line and the point.

#get two sample test for age and account balance to see if those who are above 40 years old, have higher account balance of more than 2000 or not?

# H0= Above 40 years old's people, have the account balance higher than 2000.
# H1= Having the higher account balance of more than 2000, is not limited to above 40 years old.

test1 <- t.test(AgeSubset$age, BalanceSubset$balance,  mu= 0 ,conf.level = 0.05) #two sample test
test1

attributes(test1) #get the attributes in test1 (t test)

#getting separate vectors
DaysSubset <- subset(bank_drop, day == 28) #First need to get separate vectors for 28 days
DaysSubset
DurationSubset <- subset(bank_drop, duration > 1000) #getting seprate vectors for duration higher than 1000
DurationSubset

#get the shapiro test to see if they distributed normal or not
# H0= the data for DaysSubset are normally distributed.
# H1= the data for DaysSubset are not normally distributed.

shapiro.test(DaysSubset$balance)

# H0= the data for DurationSubset are normally distributed.
# H1= the data for DurationSubset are not normally distributed.

shapiro.test(DurationSubset$day)

#Q-Q Plot for balance equal to 28 days
qqnorm(DaysSubset$balance, pch = 1, frame = FALSE)
qqline(DaysSubset$balance, col = "green", lwd = 2)

qqnorm(DurationSubset$day, pch = 1, frame = FALSE)
qqline(DurationSubset$day, col = "green", lwd = 2)


#ggally function for correlation
GGally::ggpairs(DaysSubset, columns = 11, ggplot2::aes(color= "speciecs")) #checking duration in DaysSubset 

GGally::ggpairs(DurationSubset, columns = 9, ggplot2::aes(color= "speciecs")) #checking days in DurationSubset 

# ggcore function for correlation

GGally::ggcorr(DaysSubset, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE, size=2.5)

GGally::ggcorr(DurationSubset, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE, size=2.5)

#Correlation table
cor.test(DaysSubset$duration [9:11], DurationSubset$day [9:11])


#building a mode

#getting the correlation of age and account balance
model1 <- lm(duration ~ day, data = DaysSubset) #building a model for these two variables in DaysSubset subset
model1

model2 <- lm(duration ~ day, data = DurationSubset) #building a model for these two variables in DurationSubset subset
model2


#getting the sumary of model which created 
summary(model1) 
summary(model2) 

#getting the conflit of the created model
confint(model1) 
confint(model2) 

sigma(model1)*100/mean(DaysSubset$duration)

sigma(model2)*100/mean(DurationSubset$day)

boxplot( DaysSubset$duration , main="Checking duration  in DaysSubset", xlab = "day", ylab = "duration ", col = "pink")

boxplot(BalanceSubset$day, main="Checking day in BalanceSubset", xlab = "day", ylab = "duration ", col = "purple")



mean(DaysSubset$duration)
median(DaysSubset$duration)

ggplot(DaysSubset,                  # Draw all densities in ggplot2 plot
       aes(duration,
           fill = duration)) +
  geom_density(alpha = 1)


mean(DurationSubset$day)
median(DurationSubset$day)

ggplot(DurationSubset,                  # Draw all densities in ggplot2 plot
       aes(day,
           fill = day)) +
  geom_density(alpha = 1)

#creating plot for the trend of the duration and days and their relation
ggplot(bank_drop, aes(day, duration)) +
  geom_point() +
  stat_smooth(method = lm)

a2 = DaysSubset$duration [9:11]
b2 = DurationSubset$day [9:11]
mdata2<- cbind.data.frame(a2,b2)

#creating plot for these two variables
p1 <- ggplot(mdata2, aes(x=a2, y=b2))+
  geom_point(color="black")+
  theme_bw()
p1

#add regression line
p2 <- p1 + geom_smooth(method = lm, color="yellow", se= FALSE)
p2

#get the Standard Error of Estimate
p3 <- p1 + geom_smooth(method=lm, color= "yellow", fill= "purple",se= TRUE)
p3

mdata2<- cbind.data.frame(a2,b2)

fit <- lm(a2 ~ b2, data = mdata2) # fit the model
mdata2$predicted <- predict(fit)   # Save the predicted values
mdata2$residuals <- residuals(fit) # Save the residual values
ggplot(mdata2, aes(x = a2, y = b2)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = a2, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

fit2 <- lm(a2~ b2, data= mdata2)

summary(fit2)


res <- residuals(fit2)

fitted(fit2) # line prediction it gives the points through which line is close

residuals(fit2)# epsilon thst is the distance between line and the point.

#get two sample test for duration and days to see if duration of loans which are with 28 days are equal to 1000 or not.

# H0= Duration of loans are equal to 1000 with 28 days.
# H1= Duration of loans are not equal to 1000 with 28 days.

test2 <- t.test(DaysSubset$duration, DurationSubset$day,  mu= 0 ,conf.level = 0.05) #two sample test
test2

attributes(test2) #get the attributes in test2 (t test)

#getting separate vectors
Balance1 <- subset(bank_drop, balance > 5000 ) #First need to get separate vectors for balance higher than 5000
Balance1

CampaignSubset <- subset(bank_drop, campaign > 10) #getting seprate vectors for CampaignS is below 100.
CampaignSubset

#get the shapiro test to see if they distributed normal or not
# H0= the data for Balance1 are normally distributed.
# H1= the data for Balance1 are not normally distributed.

shapiro.test(Balance1$campaign)

#Q-Q Plot for balance equal to 28 days
qqnorm(Balance1$campaign, pch = 1, frame = FALSE)
qqline(Balance1$campaign, col = "green", lwd = 2)

qqnorm(CampaignSubset$balance, pch = 1, frame = FALSE)
qqline(CampaignSubset$balance, col = "green", lwd = 2)


#ggally function for correlation
GGally::ggpairs(Balance1, columns = 12, ggplot2::aes(color= "speciecs")) #checking Campaign in Balance1 

GGally::ggpairs(CampaignSubset, columns = 6, ggplot2::aes(color= "speciecs")) #checking balance in CampaignSubset 

# ggcore function for correlation

GGally::ggcorr(Balance1, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE, size=2.5)

GGally::ggcorr(CampaignSubset, method = c("everything", "spearman"), label = TRUE, label_alpha = TRUE, size=2.5)

#Correlation table
cor.test(CampaignSubset$balance [6:12], Balance1$campaign [6:12])


#building a mode

#getting the correlation of age and account balance
model1 <- lm(campaign ~ balance, data = Balance1) #building a model for these two variables in Balance1 subset
model1

model2 <- lm(campaign ~ balance, data = CampaignSubset) #building a model for these two variables in CampaignSubset subset
model2


#getting the sumary of model which created 
summary(model1) 
summary(model2) 

#getting the conflit of the created model
confint(model1) 
confint(model2) 

sigma(model1)*100/mean(DaysSubset$duration)

sigma(model2)*100/mean(DurationSubset$day)

boxplot( Balance1$campaign , main="Checking campaign  in Balance1", xlab = "balance", ylab = "campaign ", col = "red")

boxplot(CampaignSubset$balance, main="Checking balance in CampaignSubset", xlab = "campaign", ylab = "balance ", col = "blue")



mean(Balance1$campaign)
median(Balance1$campaign)

ggplot(Balance1,                  # Draw all densities in ggplot2 plot
       aes(campaign,
           fill = campaign)) +
  geom_density(alpha = 1)


mean(CampaignSubset$balance)
median(CampaignSubset$balance)

ggplot(CampaignSubset,                  # Draw all densities in ggplot2 plot
       aes(balance,
           fill = balance)) +
  geom_density(alpha = 1)

#creating plot for the trend of the campaign and balance and their relation
ggplot(bank_drop, aes(campaign, balance)) +
  geom_point() +
  stat_smooth(method = lm)

a3 = CampaignSubset$balance [6:12]
b3 = Balance1$campaign [6:12]
mdata3<- cbind.data.frame(a3,b3)

#creating plot for these two variables
p1 <- ggplot(mdata3, aes(x=a3, y=b3))+
  geom_point(color="black")+
  theme_bw()
p1

#add regression line
p2 <- p1 + geom_smooth(method = lm, color="red", se= FALSE)
p2

#get the Standard Error of Estimate
p3 <- p1 + geom_smooth(method=lm, color= "red", fill= "yellow",se= TRUE)
p3

fit <- lm(a3 ~ b3, data = mdata3) # fit the model
mdata3$predicted <- predict(fit)   # Save the predicted values
mdata3$residuals <- residuals(fit) # Save the residual values
ggplot(mdata3, aes(x = a3, y = b3)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = a3, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

fit3 <- lm(a3~ b3, data= mdata3)

summary(fit3)


res <- residuals(fit3)

fitted(fit3) # line prediction it gives the points through which line is close

residuals(fit3)# epsilon thst is the distance between line and the point.

#get two sample test for campaign depository  for accounts with balance higher than 5000 to see if its higher than 10 or not.

# H0= Campaign depository with balance higher than 5000 is equal or higher than 10
# H1= Campaign depository with balance higher than 5000 is not higher than 100.

test3 <- t.test(bank_drop$campaign, bank_drop$balance,  mu= 0 ,conf.level = 0.05, alternative = "greater") #two sample test
test3

attributes(test3) #get the attributes in test3 (t test)
