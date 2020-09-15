library(ggplot2)
library(MASS)
library(e1071)
library(caTools)
library(rpart)
library(rpart.plot)


#setting the working directory
setwd("C:/Users/Shubham/Downloads/NCI/Data Mining & Machine Learning/Project/Datasets/travel insurance.csv")

#importing the dataset
dataset <- read.csv("travel insurance.csv")
str(dataset)


#checking for null values
table(is.na(dataset))
plot(dataset$Gender)

boxplot(dataset[,c(6,11)])

#Factorization
dataset$Agency.Type<- factor(dataset$Agency.Type,
                             levels = c("Airlines","Travel Agency"),
                             labels = c(0,1))

dataset$Distribution.Channel<- factor(dataset$Distribution.Channel,
                             levels = c("Offline","Online"),
                             labels = c(0,1))

dataset$Claim<- factor(dataset$Claim,
                       levels = c("No","Yes"),
                       labels = c(0,1))


#Correlation
tb1 = table(dataset$Agency.Type, dataset$Distribution.Channel)
chisq.test(tb1)

tb2 = table(dataset$Agency.Type, dataset$Claim)
chisq.test(tb2)

cor1 <- aov(dataset$Duration~dataset$Agency.Type)
summary(cor1)

cor2 <- aov(dataset$Net.Sales~dataset$Agency.Type)
cor3 <- aov(dataset$Commision..in.value.~dataset$Agency.Type)
cor4 <- aov(dataset$Age~dataset$Agency.Type)

summary(cor2)
summary(cor3)
summary(cor4)

#EDA

ggplot(dataset,aes(x=Agency.Type, fill = Agency.Type)) + geom_bar() + theme(plot.title=element_text(hjust = 0.5)) +
  labs(title = "Agency Type Distribution") + xlab("Agency Type")

ggplot(dataset,aes(x=Age, fill = Claim)) + geom_histogram(bins= 20) + theme(plot.title=element_text(hjust = 0.5)) +
  labs(title = "Insurance Claim based on Age") + xlab("Age")




#spliting data into train and test sets
set.seed(10)
dataset1 <- dataset[,c(2,3,5,6,8,9,11)] #dimentionality reduction
index <- sample(1:nrow(dataset1), nrow(dataset1) * .75, replace=FALSE)
train <- dataset1[index, ]
test <- dataset1[-index, ]



#Decision Tree Classification
classifier_dt= rpart(formula = Agency.Type ~ Distribution.Channel + Claim + Duration + Age + Net.Sales + Commision..in.value., data = train)
y_pred_dt = predict(classifier_dt, newdata = test, type = 'class')


rpart.plot(classifier_dt, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)

#consfusionMatrix
table(test[,2], y_pred_dt)
confusionMatrix(table(y_pred_dt, test$Agency.Type))


#Naive Bayes

classifier_nb = naiveBayes(x = train[-1],
                        y = train$Agency.Type)



y_pred_nb = predict(classifier_nb, newdata = test[-1])
cm = table(test[,1], y_pred_nb)
n = sum(cm)
diagonal = diag(cm)
accuracy = sum(diagonal) / n
confusionMatrix(table(y_pred_nb, test[,1]))
