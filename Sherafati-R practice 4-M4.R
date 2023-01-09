#install.packages ("MASS")
library(MASS)

library(MASS) #get "cats" dataset from 'MASS' library package
data(cats)
cats #read dataset

# Rename column 
names(cats)[names(cats) == 'Bwt'] <- 'Body.Weight'
names(cats)[names(cats) == 'Hwt'] <- 'Heart.Weight'

head(cats)

# delete 'NA' value if exist
na.omit(cats)

head(cats)

str(cats) #First get string of dataset 
summary(cats) #summary of cat dataset to gain information for taking t test

box1 <- boxplot( cats$Body.Weight ~ cats$Heart.Weight, main="Compare body weight and heart weight between cats", xlab = "Heart.Weight", ylab = "Body.Weight ", col = "light pink")

Male.Cat <- subset(cats, Sex == "M") #get separate vectors for male cat bodyweights
Male.Cat

Female.Cat <- subset(cats, Sex == "F") #get separate vectors for female cat bodyweights
Female.Cat

#get two sample test for male and female cats body weight to see if they have the same body weight or not
# H0= they have the same body weight
# H1= Their body weight are different

t.test(Male.Cat$Body.Weight, Female.Cat$Body.Weight) #two sample of male and female body weight test

t = (mean(Male.Cat$Body.Weight)-2.5)/(sd(Male.Cat$Body.Weight)/sqrt(length(Male.Cat$Body.Weight)))
2*pt(-abs(t),df=length(Male.Cat$Body.Weight)-1)
pt(t, df=136.84, lower.tail=T)
pt(t, df=136.84, lower.tail=F)

t = (mean(Female.Cat$Body.Weight)-2.5)/(sd(Female.Cat$Body.Weight)/sqrt(length(Female.Cat$Body.Weight)))
2*pt(-abs(t),df=length(Female.Cat$Body.Weight)-1)
pt(t, df=136.84, lower.tail=T)
pt(t, df=136.84, lower.tail=F)

#install.packages("devtools")
library(devtools)
library(ggplot2)
library(ggpubr)

p1 <- ggplot(Male.Cat) + geom_point(aes(Body.Weight, Heart.Weight)) #create chart for BWT and HWT for male cats
p2 <- ggplot(Female.Cat) + geom_boxplot(aes(Body.Weight, Heart.Weight, group = Heart.Weight)) #create chart for BWT and HWT for female cats

#combine two charts together
ggarrange(p1, p2 + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)    

sleeping.score_Before =c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4) #The average sleeping quality scores in the week before the workshop
sleeping.score_After=c( 6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9) #The average sleeping quality scores in the week following the workshop

#mean of sleeping score before medication
mean(sleeping.score_Before)

#mean of sleeping score after medication
mean(sleeping.score_After)

#summary of sleeping score before and after medication
summary(sleeping.score_Before, sleeping.score_After)


#two sample for sleeping score before and after medication
t.test(sleeping.score_Before, sleeping.score_After) 

# H0= sleeping quality has same effect before and after meditation
# H1= sleeping quality is different after meditation


t = (mean(sleeping.score_Before)-2.5)/(sd(sleeping.score_Before)/sqrt(length(sleeping.score_Before)))
2*pt(-abs(t),df=length(sleeping.score_Before)-1)
pt(t, df=136.84, lower.tail=T)
pt(t, df=136.84, lower.tail=F)

t = (mean(sleeping.score_After)-2.5)/(sd(sleeping.score_After)/sqrt(length(sleeping.score_After)))
2*pt(-abs(t),df=length(sleeping.score_After)-1)
pt(t, df=136.84, lower.tail=T)
pt(t, df=136.84, lower.tail=F)

#get paired T test with level of significance changes of 0.05
t.test(sleeping.score_Before,sleeping.score_After,paired = TRUE,conf.level = 0.95) #significant level of 0.05 is equal to 95% confidence level (0.95)

# H0= sleeping quality has same effect before and after meditation
# H1= sleeping quality is different after meditation

#changed the level of significance changes from 0.05 to 0.1
t.test(sleeping.score_Before,sleeping.score_After,paired = TRUE,conf.level = 0.90) #significant level of 0.1 is equal to 90% confidence level (0.90)

# H0= sleeping quality has same effect before and after meditation
# H1= sleeping quality is different after meditation

## NA
