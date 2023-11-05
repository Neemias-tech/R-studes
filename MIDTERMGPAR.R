#Neemias Moreira

library(tidyverse)
#some good info, R Value greater= more correlation 
#F - Value smaller better correlation
#GPA, number of hours they study at night,
#number of nights they go out, and their gender


gpa <- read.csv("gpa.csv")

str(gpa)

summary(gpa)

gpa4 <- filter(gpa, between(gpa, 0.0,4.0))


summary(gpa4)

#Need to exclude the value of 4.670 of GPA

#Model 1 with one variable

plot(gpa4$gpa, xlab="GPA", gpa4$sleepnight, ylab = "Hours of Sleep at Night")
abline(lineargpa, col="red")


lineargpa <- lm(gpa ~ sleepnight  , gpa4)  
abline(lineargpa, col="red")
summary(lineargpa)

coef(lineargpa)

confint(lineargpa, level=0.95)

#2 values of test

plot(lineargpa)


plot(lineargpa$residuals)
abline(h=0, col="blue")

summary(pred)



#train test 1st model

set.seed(199)
Traingpa <- sample(nrow(gpa4), 0.70*nrow(gpa4), replace=FALSE)
Train <- gpa4[Traingpa,]
Test <-  gpa4[-Traingpa,] 

gpalmTT <- lm(gpa ~ sleepnight, data=Train)   
summary(gpalmTT)

predict(gpalmTT, Test)

predGpa <- predict(gpalmTT, Test) 
actGpa <- Test$gpa
Outcome <- data.frame(cbind(actGpa, predGpa))

Outcome <- mutate(Outcome, percentdiff = (actGpa - predGpa)/actGpa*100)
mean(Outcome$percentdiff)
sd(Outcome$percentdiff)
median(Outcome$percentdiff)
Outcome

df <-data.frame(sleepnight = c(3, 10))
predict(lineargpa, df)
pred <- predict(lineargpa,df, interval= "confidence", level=0.95)
pred
plot(lineargpa$residuals)
abline(h=0, col="blue")


#GG PLOT #model2
plotgpa <- ggplot(data=gpa4, aes(x= gpa, y= sleepnight, 
                                 size = sleepnight, color = gpa)) +
  geom_point()



plotgpa + geom_smooth(span=1) 

plotgpa + geom_smooth(method = lm)


##Model 3

plot(gpa4$gpa, gpa4$sleepnight)

gpa4_M3 <- mutate(gpa4, sq = sleepnight^2)
str(gpa4_M3)

gpa4_M3sq <- lm (gpa ~ sleepnight, gpa4_M3)

df3 <-data.frame(sleepnight = c(3,9))
predict(gpa4_M3sq, df3)
pred3 <- predict(gpa4_M3sq,df3, interval= "confidence", level=0.95)
pred3
gpa4_M3sq

ggplot(gpa4_M3, aes(x=gpa, y=sleepnight))+
  geom_point() + plotgpa + geom_smooth(span=1) 

#train model 3
set.seed(199)
Traingpa3 <- sample(nrow(gpa4_M3), 0.70*nrow(gpa4_M3), replace=FALSE)
Train3 <- gpa4[Traingpa3,]
Test3 <-  gpa4[-Traingpa3,] 

gpalmTT3 <- lm(gpa ~ sleepnight, data=Train3)   
summary(gpalmTT3)

predict(gpalmTT3, Test3)

predGpa3 <- predict(gpalmTT3, Test3) 
actGpa3 <- Test3$gpa
Outcome3 <- data.frame(cbind(actGpa3, predGpa3))

Outcome3 <- mutate(Outcome3, percentdiff = (actGpa3 - predGpa3)/actGpa3*100)
Outcome3
mean(Outcome3$percentdiff)

df3 <-data.frame(sleepnight = c(3, 10))
predict(gpalmTT3, df3)
pred3 <- predict(gpalmTT3,df3, interval= "confidence", level=0.95)
pred3
plot(lineargpa3$residuals)
abline(h=0, col="red")


#2 values 

df3 <-data.frame(sleepnight = c(3, 10))


predict(gpa4_M3sq, df3)
pred3 <- predict(gpa4_M3sq,df, interval= "confidence", level=0.95)
pred3
plot(gpa4_M3sq$residuals)
abline(h=0, col="blue")


## Model 4 One qualitative and one quantitative


plot(ggplot(gpa4, aes(x= gender, y=gpa))+ geom_boxplot()+labs(x ="Gender", y= "GPA")
)

lineargpa4 <- lm(gpa ~ sleepnight + gender  , gpa4)  
summary(lineargpa4)


predict(lineargpa4)

summary(lineargpa4)
lineargpa4


plot(lineargpa4$residuals) + abline(h=0, col="blue")


## Model 5 -  Linear Multivariate Model with 3 or 4 variables


lineargpa5 <- lm(gpa ~ sleepnight + out + studyweek  , gpa4)  
summary(lineargpa5)

set.seed(199)
Traingpa5 <- sample(nrow(gpa4), 0.70*nrow(gpa4), replace=FALSE)
Train5 <- gpa4[Traingpa5,]
Test5 <-  gpa4[-Traingpa5,] 

gpalmTT5 <- lm(gpa ~ sleepnight + out + studyweek , data=Train5)   
summary(gpalmTT5)

predict(gpalmTT5, Test5)

predGpa5 <- predict(gpalmTT5, Test5) 
actGpa5 <- Test5$gpa
Outcome5 <- data.frame(cbind(actGpa5, predGpa5))
Outcome5 <- mutate(Outcome, percentdiff = (actGpa5 - predGpa5)/actGpa5*100)
Outcome5



mean(Outcome5$percentdiff)



df5 <-data.frame(sleepnight = c(3, 10), out = c(5,3), studyweek= c(2,5))
predict(lineargpa5, df5)
pred5 <- predict(lineargpa5,df5, interval= "confidence", level=0.95)
pred5

plot(lineargpa5$residuals)
abline(h=0, col="blue")


#Model 6

#I will use a log base because  the result is a new vector that is less 
#skewed than the original. And I still believe that Study Week it's the best variable
# to predict the GPA, so i wanna try to sharp my model using Study Week on a Logarithm base.





hist(gpa4$sleepnight)
hist(log(gpa4$sleepnight))

sleepnightlog <- mutate(gpa4, logp=(sleepnight))
sleepnightlog

plot(sleepnightlog$gpa, xlab="GPA", sleepnightlog$sleepnight, ylab = "Hours of Sleep at Night")



lineargpa6_log <- lm(gpa ~ sleepnight  , sleepnightlog)  
summary(lineargpa6_log)
coef(lineargpa6_log)
confint(lineargpa6_log, level=0.95)

#2 values of test




plot(lineargpa6_log$residuals)
abline(h=0, col="blue")

summary(pred)



#train test 6st model, log base

set.seed(199)
Traingpa6 <- sample(nrow(sleepnightlog), 0.70*nrow(sleepnightlog), replace=FALSE)
Train6 <- sleepnightlog[Traingpa,]
Test6 <-  sleepnightlog[-Traingpa,] 

gpalmTT6 <- lm(gpa ~ sleepnight, data=Train6)   
summary(gpalmTT6)

predict(gpalmTT6, Test6)

predGpa6 <- predict(gpalmTT6, Test6) 
actGpa6 <- Test6$gpa
Outcome6 <- data.frame(cbind(actGpa6, predGpa6))

Outcome6 <- mutate(Outcome6, percentdiff = (actGpa6 - predGpa6)/actGpa6*100)
mean(Outcome6$percentdiff)
sd(Outcome6$percentdiff)
median(Outcome6$percentdiff)
Outcome6


#Predict 2 values

df6 <-data.frame(sleepnight = c(3, 10))
predict(lineargpa6_log, df6)
pred <- predict(lineargpa6_log,df6, interval= "confidence", level=0.95)
pred
plot(lineargpa6_log$residuals)
abline(h=0, col="blue")  
summary(lineargpa6_log)



confint(lineargpa6_log, level=0.95)






