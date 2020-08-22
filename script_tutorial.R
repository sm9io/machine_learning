options(digits = 3)
library(caret)
library(dslabs)
library(tidyverse)
ds_theme_set()
data("heights")
y <- heights$sex
x <- heights$height

# create data partition
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index,]
test_set <- heights[test_index,]

# first model (guess)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>%
  summarise(mean(height), sd(height))

# second model (use cutoff)
y_hat <- ifelse(x >62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
mean(y_hat == y)

# third model (test and select cutoff)
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean((y_hat == train_set$sex))
})
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() + geom_line()
ggsave("plots/heights_sex_cutoff_accuracy.jpg")
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
