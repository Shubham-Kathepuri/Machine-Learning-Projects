library(ggplot2)
library(MASS)
library(e1071)
library(caTools)
library(rpart)
library(scorer)
library(randomForest)
library(caret)
library(corrgram)
library(corrplot)
library(pROC)

#setting working directory
setwd("C:/Users/Shubham/Downloads/NCI/Data Mining & Machine Learning/Project/Datasets/Video Games Sales")

#importing the dataset
dataset <- read.csv("Video Games Sales.csv")
str(dataset)

table(is.na(dataset))

vgs= dataset[complete.cases(dataset), ]


platform_names <- unique(dataset$Platform)
genre_names <- unique(dataset$Genre)
publisher_names <- unique(dataset$Publisher)

dataset$Platform <- factor(dataset$Platform,
                           levels = platform_names,
                           labels = c(1:31))

dataset$Genre <- factor(dataset$Genre,
                           levels = genre_names,
                           labels = c(1:12))

dataset$Publisher <- factor(dataset$Publisher,
                           levels = publisher_names,
                           labels = c(1:15))

dataset$Year <- as.numeric(dataset$Year)

#EDA
ggplot(data= dataset, aes(x=Rank, y = Global_Sales))+geom_point(color = "green")+
  labs(title =  "Variation of Global Sales based on Game Rank") +xlab("Rank") + ylab("Global Sales") + theme(plot.title=element_text(hjust = 0.5))

ggplot(data= dataset, aes(x=Publisher, y=Global_Sales))+geom_point(color='Blue')+  labs(title =  "Distribution of Global Sales per Publisher") + xlab("Publisher") +  ylab("Global Sales") 

#Correlation check
corrplot(cor(dataset[,c("Rank", "Year", "Global_Sales"),]), method="color")

cor1 <- aov(dataset$Global_Sales~dataset$Platform)
summary(cor1)

cor2 <- aov(dataset$Global_Sales~dataset$Genre)
summary(cor2)

cor3 <- aov(dataset$Global_Sales~dataset$Publisher)
summary(cor3)

set.seed(1234) 
# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(dataset$Global_Sales, SplitRatio = 0.75) # SplitRatio = percent of sample==TRUE
# Training Data
train = subset(dataset, sample == TRUE)
# Testing Data
test = subset(dataset, sample == FALSE)


#Model
regresor_ml = lm(formula = Global_Sales ~ Rank + Platform + Genre + Publisher + Year, data =  train)
summary(regresor_ml)


y_pred_ml <- predict(regresor_ml, newdata = test)

mean_absolute_error(test$Global_Sales, y_pred_ml)
mean_squared_error(test$Global_Sales, y_pred_ml)
RMSE(y_pred_ml, obs= test$Global_Sales)
par(mfrow =c(2,2))
plot(regresor_ml)


#Random Forrest
#training the model
regresor_rf<- randomForest (formula = Global_Sales ~ Rank + Platform + Genre + Publisher + Year, data = train,importance=TRUE,ntree=200)
summary(regresor_rf)


#Prediction
y_pred_rf<-predict(regresor_rf,test)
head(y_pred_rf,5)



mean_squared_error(test$Global_Sales, y_pred_rf)
mean_absolute_error(test$Global_Sales, y_pred_rf)
RMSE(y_pred_rf, obs = test$Global_Sales)



