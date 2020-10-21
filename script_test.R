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

#cc03

options(digits = 3)
(.85*.02)/(.85*.02+.1*.98)

set.seed(1, sample.kind = "Rounding")
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

.85*.02+.1*.98
mean(test)
mean(disease[test==0])
(.85*.02)/(.85*.02+.1*.98)
mean(disease[test==1])
mean(disease[test==1])/.02
(.85*.02)/(.85*.02+.1*.98)/.02

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
ggsave("plots/height_condition_male_qplot.jpg")

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)
ggsave("plots/height_condition_male_qplot_quantile.jpg")

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0, 1, 0.1)
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
ggsave("plots/height_bivariate_extraction.jpg")

# cc04

set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
y <- dat$y
rmse <- replicate(n, {
  ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-ind_test)
  test_set <- dat %>% slice(ind_test)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
sd(rmse)

options(digits = 3)
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1, sample.kind="Rounding")
op_dat <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    ind_test <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-ind_test)
    test_set <- dat %>% slice(ind_test)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(n = n, avg_rmse = mean(rmse), sd_rmse = sd(rmse))
})
op_dat

set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
y <- dat$y
rmse <- replicate(n, {
  ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-ind_test)
  test_set <- dat %>% slice(ind_test)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
sd(rmse)

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
y <- dat$y

set.seed(1, sample.kind="Rounding")
ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-ind_test)
test_set <- dat %>% slice(ind_test)
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(1, sample.kind="Rounding")
ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-ind_test)
test_set <- dat %>% slice(ind_test)
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(1, sample.kind="Rounding")
ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-ind_test)
test_set <- dat %>% slice(ind_test)
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
y <- dat$y

set.seed(1, sample.kind="Rounding")
ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-ind_test)
test_set <- dat %>% slice(ind_test)
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(1, sample.kind="Rounding")
ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-ind_test)
test_set <- dat %>% slice(ind_test)
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(1, sample.kind="Rounding")
ind_test <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-ind_test)
test_set <- dat %>% slice(ind_test)
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))
