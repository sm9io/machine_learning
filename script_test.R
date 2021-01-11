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

set.seed(2, sample.kind="Rounding")
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()
ggsave("plots/cc04_1.jpg")

set.seed(1, sample.kind="Rounding")
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)
ggsave("plots/cc04_1_ans.jpg")

# cc05

library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = 2)
ggsave("plots/date_deaths_scatter_smooth_loess.jpg")

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)
ggsave("plots/date_deaths_year_smooth_loess.jpg")

library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)
ggsave("plots/7_vs_2_scatter_x2y.jpg")
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) +
  geom_point() +
  geom_smooth(method = "loess")
ggsave("plots/7_vs_2_scatter_x2y_smooth_loess.jpg")

# cc06

if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images
y <- mnist$train$labels
ind <- (x > 50 & x < 205)
ind <- rowMeans(ind)
mean(ind)

# cc07

library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)

x_1 <- as.matrix(d)[1:2,1:2]
x_2 <- as.matrix(d)[39:40,39:40]
x_3 <- as.matrix(d)[73:74,73:74]
x_1
x_2
x_3
x_a <- c(x_1, x_2, x_3)
x_a

d <- dist(tissue_gene_expression$x)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))
ggsave("plots/distance_image.jpg")

options(digits = 3)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
heights_train <- heights %>% slice(-test_index)
heights_test <- heights %>% slice(test_index)
ks <- seq(1, 101, 3)
f_1 <- sapply(ks, function(k){
  heights_knn <- knn3(sex ~ height, heights_train, k = ks)
  heights_y <- predict(heights_knn, heights_test, type = "class")
  F_meas(data = heights_y, reference = factor(heights_test$sex))
})
max(f_1)
ks[which.max(f_1)]

library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
tissue_gene_train_y <- tissue_gene_expression$y[-test_index]
tissue_gene_test_y <- tissue_gene_expression$y[test_index]
tissue_gene_train_x <- tissue_gene_expression$x[-test_index,]
tissue_gene_test_x <- tissue_gene_expression$x[test_index,]
tissue_gene_train <- list(x = tissue_gene_train_x, y = tissue_gene_train_y)
tissue_gene_test <- list(x = tissue_gene_test_x, y = tissue_gene_test_y)
ks <- seq(1, 11, 2)
tissue_gene_test_acc <- sapply(ks, function(ks){
  tissue_model <- knn3(y~., data = as.data.frame(tissue_gene_train), k = ks)
  tissue_y_hat <- predict(tissue_model, as.data.frame(tissue_gene_test), type = "class")
  data.frame(k = ks, acc = confusionMatrix(data = tissue_y_hat, reference = tissue_gene_test_y)$overall["Accuracy"])
})
tissue_gene_test_acc

# cc08

library(tidyverse)
library(caret)
set.seed(1996, sample.kind="Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y)
fit$results
fit <- train(x_subset, y, method = "glm")
fit$results

library(genefilter)
tt <- colttests(x, y)
str(tt)
pvals <- tt$p.value
ind <- which(pvals < 0.01)
length(ind)
x_subset <- x[ ,ind]
str(x_subset)
fit <- train(x_subset, y)
fit <- train(x_subset, y, method = "glm")
fit$results
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
ggsave("plots/cross_validation_knn_tunegrid_1.jpg")

data(tissue_gene_expression)
dat_x <- tissue_gene_expression$x
dat_y <- tissue_gene_expression$y
set.seed(1, sample.kind="Rounding")
fit <- train(dat_x, dat_y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit
plot(fit)
ggsave("plots/cross_validation_knn_tunegrid_2.jpg")

library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
str(indexes)
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

sum(sapply(1:10, function(i){
  sum(indexes[[i]] == 3)
}))

y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

B <- 10000
set.seed(1, sample.kind = "Rounding")
y_sam <- replicate(B, quantile(rnorm(100, 0, 1), 0.75))
mean(y_sam)
sd(y_sam)

set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
y_sam_ind <- createResample(y, 10)
mean(sapply(1:10, function(i){
  quantile(y[y_sam_ind[[i]]], 0.75)
}))
sd(sapply(1:10, function(i){
  quantile(y[y_sam_ind[[i]]], 0.75)
}))

set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
B <- 10000
y_sam_ind <- createResample(y, B)
mean(sapply(1:B, function(i){
  quantile(y[y_sam_ind[[i]]], 0.75)
}))
sd(sapply(1:B, function(i){
  quantile(y[y_sam_ind[[i]]], 0.75)
}))
