#Objective: Predict the percentage of marks of an student based on the 
#number of study hours

#Solution:
#Installing required packages and libraries
install.packages("caTools")
install.packages('ggplot2')
library('caTools')
library('ggplot2')

#Importing the data from the url
scores<-read.csv(url("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"))

#Checking the data structure and understand the data properly
str(scores)
summary(scores)

#Understand the data visually
plot(scores$Scores,scores$Hours)

#Split the data into 70/30 ratio
set.seed(2020)
split<-sample.split(scores$Scores,SplitRatio = 0.80)
split
train_scores<-subset(scores,split==T)
test_scores<-subset(scores,split==F)

#Cross Checking the split
nrow(train_scores)
nrow(test_scores)
nrow(scores)

#linear regression model
model<-lm(Scores~Hours,data=train_scores)

#Understand the model
summary(model)

#Corelation between the variables
cor(scores$Scores,scores$Hours)

#prediction from the model comparing with the test data
test_scores$pred<-predict(model,test_scores)
test_scores

#understand the model's output visually
ggplot(data = test_scores, aes(x = Scores, y = Hours)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

#What will be predicted score if a student studies for 9.25 hrs/ day?
P<-2.6294+(9.25*9.7041) #p=B0+B1*Hours from the equation p=B0+B1*A....
P
