#cc01

library(dslabs)
str(read_mnist())

#cc02

options(digits = 3)
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% group_by(type) %>% summarise(per = mean(sex == "Female"))
y_hat <- ifelse(x == "inclass", "Female", "Male")
mean(y_hat == y)
confusion <- table(y_hat, y)
confusion
sensitivity(confusion)
specificity(confusion)
mean(y == "Female")

library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
train %>% summarise(min = min(Sepal.Length), max = max(Sepal.Length))
cutoff <- seq(5, 7.9, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + geom_line()
ggsave("plots/species_sepal_length_cutoff_accuracy.jpg")

train %>% summarise(min = min(Sepal.Width), max = max(Sepal.Width))
cutoff <- seq(2, 3.8, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + geom_line()
ggsave("plots/species_sepal_width_cutoff_accuracy.jpg")

train %>% summarise(min = min(Petal.Length), max = max(Petal.Length))
cutoff <- seq(3, 6.9, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + geom_line()
ggsave("plots/species_petal_length_cutoff_accuracy.jpg")

train %>% summarise(min = min(Petal.Width), max = max(Petal.Width))
cutoff <- seq(1, 2.5, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + geom_line()
ggsave("plots/species_petal_width_cutoff_accuracy.jpg")

cutoff <- 4.7
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
accuracy

cutoff <- seq(min(test$Sepal.Length), max(test$Sepal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]

cutoff <- seq(min(test$Sepal.Width), max(test$Sepal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]

cutoff <- seq(min(test$Petal.Length), max(test$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]

cutoff <- seq(min(test$Petal.Width), max(test$Petal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)
cutoff[which.max(accuracy)]

plot(iris,pch=21,bg=iris$Species)

cutoff1 <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
cutoff2 <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy1 <- map_dbl(cutoff1, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy1)
cutoff1[which.max(accuracy1)]
accuracy2 <- map_dbl(cutoff2, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy2)
cutoff2[which.max(accuracy2)]

y_hat <- ifelse(test$Petal.Length > 4.7 | test$Petal.Width > 1.5, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)
