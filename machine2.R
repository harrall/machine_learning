# Comprehension Check: Practice with Machine Learning

library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#q1
dat_tbl <- dat %>% group_by(sex,type) %>% tally()
Fi <- dat_tbl[1,3]
Mi <- dat_tbl[3,3]
Fo <- dat_tbl[2,3]
Mo <- dat_tbl[4,3]
Fi_share <- Fi / (Fi + Mi)
Fi_share
Fi_share2 <- Fo / (Fo + Mo)
Fi_share2

#q2
y_hat <- ifelse(x == "inclass", "Female", "Male") 
dat <- dat %>% mutate(y_hat = y_hat)
mean(dat$sex == dat$y_hat)
mean(y == y_hat)

#q3
table(predicted = y_hat, actual = y)
table(y_hat, y)
#q4
y_hat <- as.factor(y_hat)
sensitivity(data = y_hat, reference = y)
#q5
specificity(data = y_hat, reference = y)
specificity(y_hat, y)
#q6
Fi <- dat_tbl[1,3]
Mi <- dat_tbl[3,3]
Fo <- dat_tbl[2,3]
Mo <- dat_tbl[4,3]

rate <- (Fi + Fo) / (Fi + Fo + Mi + Mo)
rate

# q7
library(tidyverse)
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#q8
range(iris$Sepal.Length)
SLcut <- seq(4.9, 7.9, 0.10)
range(iris$Sepal.Width)
SWcut <- seq(2.0,3.8,0.10)
range(iris$Petal.Length)
PLcut <- seq(3.0,6.9,0.10)
range(iris$Petal.Width)
PWcut <- seq(1.0,2.5,0.10)



SLaccuracy <- map_dbl(SLcut, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
SLaccuracy
max(SLaccuracy)

SWaccuracy <- map_dbl(SWcut, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(SWaccuracy)

PLaccuracy <- map_dbl(PLcut, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(PLaccuracy)

PWaccuracy <- map_dbl(PWcut, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(PWaccuracy)

# winner Q9
max(PLaccuracy)
best_cutoff <- PLcut[which.max(PLaccuracy)]
y<-factor(y,exclude="setosa") 
test$Species <- factor(test$Species,exclude = "setosa")

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
mean(y_hat == test$Species)

#q10
SLaccuracy <- map_dbl(SLcut, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
SLaccuracy
max(SLaccuracy)

SWaccuracy <- map_dbl(SWcut, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(SWaccuracy)

PLaccuracy <- map_dbl(PLcut, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(PLaccuracy)

PWaccuracy <- map_dbl(PWcut, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(PWaccuracy)


#q11
plot(iris,pch=21,bg=iris$Species)
range(iris$Petal.Length)
PLcut <- seq(3.0,6.9,0.10)
range(iris$Petal.Width)
PWcut <- seq(1.0,2.5,0.10)

bothP_cut <- seq(1.0,6.9,0.1)

PL_cutoff <- PLcut[which.max(PLaccuracy)]
PW_cutoff <- PWcut[which.max(PWaccuracy)]

bothPaccuracy <- map_dbl(bothP_cut, function(x){
  y_hat <- ifelse(test$Petal.Width > PW_cutoff | test$Petal.Length > PL_cutoff,
                  "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(bothPaccuracy)

# Conditional probabilities

#Q1
p_a <- (0.85 * 0.02) + (0.1 * 0.98)
ps <- 0.115/0.02
px <- 0.85 / ps
px

q2 <- 0.85 * 0.02 + 0.1 * 0.98
pc <- 1 - p_a
pbc <- 0.15* 0.02 / 0.885
pbc

pba <- 0.85 * 0.02 / 0.115
pba

q5 <- pba / 0.02
q5

#Q6
library(dslabs)
data("heights")
# MISSING CODE
qplot(height, p, data =.)

#1
data("heights")
heights %>% 
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

#2
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Female")) %>%
qplot(height, p, data =.)

#3
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

#4 - winner
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)


#q7
ps <- seq(0, 1, 0.1)
heights %>% 
#  MISSING CODE
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#1
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(male, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#2 - winner
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#3
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(female, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#4
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps))) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#q8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

#orig
ps <- seq(0, 1, 0.1)
dat %>% 
#  MISSING CODE	
qplot(x, y, data =.)

#1 - Winner
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

#2
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps))) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

#3
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

#4
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y =(y), x =(x)) %>%
  qplot(x, y, data =.)






