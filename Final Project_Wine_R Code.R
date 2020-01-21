# Final Project Week 6
library(readr)
library(dplyr)
library(ggplot2)
library(nnet)
library(caret)
library(MASS)
library(corrplot)
library(caTools)
library(InformationValue)


getwd()
setwd("C:/Users/Hp/Desktop/CPS Analytics/ALY 6105 Intermediate Analytics/Final Project")
winequality <- read.csv("WinequalityRed.csv")
head(winequality)
str(winequality)
summary(winequality)

sapply(winequality, function(x) {sum(is.na(x))})
mydata <- na.omit(winequality)
mydata<-scale(mydata)
mydata

table(winequality$quality)


ggplot(winequality,aes(x=quality))+geom_bar(stat = "count",position = "dodge", color = "black", fill = "darkred")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()


# finding correlation
wine_corr <- data.frame(cor(winequality))
print(cbind(colnames(winequality),round(wine_corr$quality, 3)))

# splitting data in training and test dataset

set.seed(1) 
row.number <- sample(x=1:nrow(winequality), size=0.8*nrow(winequality))
train = winequality[row.number,]
test = winequality[-row.number,]
head(train)
head(test)

winequality_dist <- data.frame(table(winequality$quality))
train_dist <- data.frame(table(train$quality))
test_dist <- data.frame(table(test$quality))
class_dist <- cbind(winequality_dist, train_dist[2], test_dist[2])
class_dist

# simple linear regression with all variables
quality_lm <- lm(quality ~ ., data=train)
summary(quality_lm)

# checking for correlation among significant variables
corrplot(cor(train))
head(train)
# generalized linear regression
quality_glm <- glm(quality ~ alcohol + sulphates + chlorides + fixed.acidity+ 
                     total.sulfur.dioxide +volatile.acidity,
                   data = train, family = gaussian(link='log'))
summary(quality_glm)

train$quality = as.factor(train$quality)
test$quality = as.factor(test$quality)

# Setting quality of wine more than 6 as 1 and remaining 0

winequality$good_wine <- ifelse(winequality$quality > 6,1,0)
head(winequality)
View(winequality)
winequality$good_wine <- as.factor(winequality$good_wine)

# splitting into test & train

set.seed(1) 
row.number <- sample(x=1:nrow(winequality), size=0.8*nrow(winequality))
l_train = winequality[row.number,]
l_test = winequality[-row.number,]
head(l_train)
head(l_test)

#Binomial

quality_bi_glm <- glm(good_wine ~ alcohol + sulphates + total.sulfur.dioxide 
                      + chlorides + volatile.acidity+ pH,
                       data = l_train, family=binomial(link='logit'))
summary(quality_bi_glm)


View(winequality)

# constructing confusing matrix
prediction_bi_mult <- predict.glm(quality_bi_glm, newdata = l_test, type = 'response')
predicted_values <- ifelse(prediction_bi_mult > 0.5,1,0)

str(predicted_values)
str(l_test$good_wine)
l_test$good_wine<-as.factor(l_test$good_wine)
predicted_values<- as.factor(predicted_values)

confusionMatrix(predicted_values, l_test$good_wine)
