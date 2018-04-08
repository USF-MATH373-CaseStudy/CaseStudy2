library(tidyverse)
library(magrittr)
library(ISLR)
library(tree)

setwd("/Users/chunheisiu/Dropbox/Documents/USF/2018_Spring/MATH_373/CaseStudy/CaseStudy2")

# Read training and testing data
data.train <- read.csv("OnlineNewsPopularityTraining.csv", header = T)
data.test <- read.csv("OnlineNewsPopularityTest.csv", header = T)

# Function for dropping variables
dropVars <- function(df) {
  return (dplyr::select(df, -c(shares, url, timedelta)))
}

# Set up variables
data.train <- dropVars(data.train)
data.test <- dropVars(data.test)
data.train$popular <- as.factor(data.train$popular)
data.test$popular <- as.factor(data.test$popular)
data.train.class <- data.train$popular
data.test.class <- data.test$popular
length_data.train <- dim(data.train)[1]

data.train %<>% 
  mutate(content = as.factor(ifelse(n_tokens_content <= 500, 1, 0)))

# Classification tree
tree.train <- tree(content ~ .-popular, data = data.train)
summary(tree.train)

plot(tree.train)
text(tree.train, pretty = 0)

tree.pred <- predict(tree.train, data.test, type = "class")