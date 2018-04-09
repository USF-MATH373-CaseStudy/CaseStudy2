library(tidyverse)
library(magrittr)
library(ISLR)
library(tree)
library(caret)

setwd("/Users/chunheisiu/Dropbox/Documents/USF/2018_Spring/MATH_373/CaseStudy/CaseStudy2")

# Read training and testing data
data.train <- read.csv("OnlineNewsPopularityTraining.csv", header = T)
data.test <- read.csv("OnlineNewsPopularityTest.csv", header = T)

# Function for dropping variables
dropVars <- function(df) {
  return (dplyr::select(df, -c(shares, url, timedelta)))
}

# Set up variables
data.train <- na.omit(data.train)
data.test <- na.omit(data.test)
data.train <- dropVars(data.train)
data.test <- dropVars(data.test)

data.train$popular <- factor(data.train$popular)
data.test$popular <- factor(data.test$popular)

data.train.class <- data.train$popular
data.test.class <- data.test$popular

# Upsampling the data since it is unbalanced towards 0
set.seed(1)
data.train.upsample <- upSample(data.train, data.train.class, T)$x

# Classification tree
tree.train <- tree(popular ~ ., data = data.train.upsample)
summary(tree.train)

plot(tree.train)
text(tree.train, pretty = 0)

tree.pred <- predict(tree.train, data.test, type = "class")
table(tree.pred, data.test.class)
mean(tree.pred == data.test.class)

cv.pop <- cv.tree(tree.train, FUN = prune.misclass)
cv.pop

par(mfrow = c(1, 2))
plot(cv.pop$size, cv.pop$dev, type = "b")
plot(cv.pop$k, cv.pop$dev, type = "b")

par(mfrow = c(1,1))
prune.pop <- prune.misclass(tree.train, best = cv.pop$size[which.min(cv.pop$dev)])
plot(prune.pop)
text(prune.pop, pretty = 0)

prune.pred <- predict(prune.pop, data.test, type = "class")
table(prune.pred, data.test.class)
mean(prune.pred == data.test.class)
