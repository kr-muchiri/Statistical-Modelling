######################################################
##  R Code for homework 8                           ##
######################################################

## Reset memory
rm(list=ls())

## Call packages
library(ISLR)
library(ggplot2)

data_auto <- Auto

data_auto$log_mpg <- log(data_auto$mpg)

Model1 <- lm(log_mpg ~ weight, data = data_auto)
summary(Model1)

Model2 <- lm(log_mpg ~ weight + horsepower, data = data_auto)
summary(Model2)

Model3 <- lm(log_mpg ~ weight + horsepower + displacement + acceleration, data = data_auto)
summary(Model3)

# F test
anova(Model1, Model3)
anova(Model2, Model3)


## Define new qualitative variable
data_auto$New_Model <- "Pre-80"
data_auto$New_Model[which(data_auto$year>=80)] <- "Post-80"

Model4 <- lm(log_mpg ~ weight + factor(New_Model), data = data_auto)
summary(Model4)

Model5 <- lm(log_mpg ~ weight*factor(New_Model), data = data_auto)
summary(Model5)

Model6 <- lm(log_mpg ~ weight + factor(origin), data = data_auto)
summary(Model6)

Model7 <- lm(log_mpg ~ factor(New_Model)*factor(origin), data = data_auto)
summary(Model7)











