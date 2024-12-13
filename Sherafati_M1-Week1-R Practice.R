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

bank_unclean = read.csv("~/Downloads/Archive (1)/bank.csv")
bank_unclean

# Sorted dataframe with descending age
bank_sort <- bank_unclean[order(-bank_unclean$age),]
head(bank_sort)

# Drop column "default"
bank_drop <- select(bank_sort, -5)
head(bank_drop)

# Rename column 
names(bank_drop)[names(bank_drop) == 'contact'] <- 'Contact_Info'
head(bank_drop)

# Remove the top 55 % and bottom 45%
bank <- bank_drop[6139:5022,] # Remove the top 55 % and bottom 45%

# Replace column string to Captial letters using GSUB
bank$marital <- gsub("single","SINGLE",as.character(bank$marital))
bank$marital <- gsub("married","MARRIED",as.character(bank$marital))
bank$marital <- gsub("divorced","DIVORCED",as.character(bank$marital))
head(bank)

str(bank)
#get the Mean, Min, Max, STD of each age balance
bank %>%
group_by(age)%>%
summarise_at(vars(balance),list(Mean=mean, Min=min, Max=max,STD=sd))

#get the Mean, Min, Max, STD of each campaign duration
bank %>%
group_by(campaign)%>%
summarise_at(vars(duration),list(Mean=mean, Min=min, Max=max,STD=sd))


subset(bank, balance == 1) #Find subset of balance 
subset(bank, age == 20:40) #subset of ages between 20-40 years old
subset(bank, campaign == 10) #subset of campaign between only counted 10

#1st type of- Frequency Table
bank_table <- table(bank$education, bank$job)
bank_table

#Frequency Table for deposit and age
bank_Ftable2 <- table(bank$deposit, bank$age) 
ftable(bank_Ftable2)

#Frequency Table for Contact_Info and marital status
bank_Ftable3 <- table(bank$Contact_Info, bank$marital) 
ftable(bank_Ftable3)

#create CrossTable for 'Job' and 'Education'
banl_ct1 <- CrossTable(bank$education, bank$job,
                 dnn = c("Job", "Education"))

#create CrossTable for 'age' and 'Housing'
banl_ct2 <- CrossTable(bank$age, bank$housing,
                 dnn = c("Age", "Housing"))

#ggplot for education and marital status
ggplot(data = bank) + geom_bar(mapping = aes(x=marital, fill=education), position="dodge") + labs(title="The Impact Of Marital On Education", x ="Marital Stauts", y = "Count")

# ggplot for housing and different age
ggplot(data = bank) + geom_bar(mapping = aes(x=age, fill=housing), position="dodge") + labs(title="Housing At Different Ages", x ="Age", y = "count")

#ggplot for housing and different age before cleaning the data, bigger range of age
ggplot(data = bank_unclean) + geom_bar(mapping = aes(x=age, fill=housing), position="dodge") + labs(title="Housing At Different Ages-unclean data", x ="Age", y = "count")

#create ggplot for age and contact-info
ggplot(data = bank) + geom_bar(mapping = aes(x=age, fill= Contact_Info), position="dodge") + labs(title="Different contact info based on different Ages", x ="Age", y = "contact-info")

#Top 6 jobs based on different ages
bankrank <- bank %>%
  
select(education,age, job)

Mostjob <- aggregate(age ~ job, bankrank, mean)
slice( Mostjob[order(-Mostjob$age),],
1:10)


#create box plot for top 6 jobs based on the previous code

boxplot( bankrank$age ~ bankrank$job, main="Top 6 jobs based on different ages", xlab = "job", ylab = "age ", col = "purple")

#create box plot for housing and different ages

box1 <- boxplot( bank$age ~ bank$housing, main="box plot of Housing based on different ages", xlab = "housing", ylab = "age ", col = "light green")

#create box plot; if different ages has house or not
box2 <- boxplot( bank$age ~ bank$loan, main="box plot of getting loan based on different ages", xlab = "loan", ylab = "age ", col = "light pink")

#create ggplot-box plot based age and contact-info

ggplot(bank, aes(x = age, y = Contact_Info, color = "red")) +  # ggplot function
  geom_boxplot()

#plotly for job

hist1 <-  plot_ly(data = bank ,x =  ~job, color = ~"pink", type = 'histogram', mode = 'markers')%>%
        layout(title = 'Job histogram plot', plot_bgcolor = "white")
hist1

#plotly for loan based on different ages and in different day

bank%>%
  group_by(loan) %>%
  do(p=plot_ly(., x = ~log(day),name =~age, type = "histogram")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = FALSE)%>%
        layout(title = 'Loan based on different ages and in different day', plot_bgcolor = "white")


#plotly for contact based on different Contact_Info in different ages

bank%>%
  group_by(age) %>%
  do(p=plot_ly(., x = ~log(age),name =~Contact_Info, type = "histogram")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
        layout(title = 'Different contact info based on different ages', plot_bgcolor = "white")


#plotly with density axis and dual y-axis for relation between age and education
bankdens <- density(log(bank$age))
 
plot_ly( data = bank,
  x = ~log(age), 
  type = "histogram",
  name = "age") %>% 
  add_lines(x = bankdens$x, y = bankdens$y, yaxis = "y2", name = "education") %>% 
  layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                       side = "right", #Adds the density axis on the right side
                       rangemode = "tozero"))%>% #Forces both y-axes to start at 0
        layout(title = 'relation between age and education', plot_bgcolor = "white")  


#plotly with density axis and dual y-axis for relation between age and education
plot_ly( data = bank,
  x = ~duration,
  y = ~log(campaign),
  type = "violin",
  color = ~"pink",
  side = "positive",
  meanline = list(visible = T))%>%
        layout(title = 'relation between duration and campaign', plot_bgcolor = "white")

#plotly with density axis and dual y-axis for relation between age and education
plot_ly( data = bank,
  x = ~age,
  y = ~log(campaign),
  type = "violin",
  color = ~"pink",
  side = "negative",
  meanline = list(visible = T))%>%
        layout(title = 'Relation between age and campaign in violin plot', plot_bgcolor = "white")


## 
