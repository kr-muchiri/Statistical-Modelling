###############################################################################
##  R Coding Session 4, Multiple Regression with qualitative variables       ##
###############################################################################

## Reset memory
rm(list=ls())

## Set work directory
setwd("~/Dropbox/Centre College Courses/DSC205_Fall2022/R Intro")

## Call packages
library(ISLR)
library(ggplot2)
library(dplyr)

data_auto <- Auto

data_auto$American <- 0
data_auto$American[which(data_auto$origin==1)] <-1
## American Car is 1, Foreign Car is 0


data_auto$Foreign <- 0
data_auto$Foreign[which(data_auto$origin!=1)] <-1
## American Car is 0, Foreign Car is 1

data_auto$Post_1980 <- 0
data_auto$Post_1980[which(data_auto$year>=80)] <- 1

model1 <- lm(log(mpg) ~ factor(Foreign), data = data_auto)
summary(model1)

model2 <- lm(log(mpg) ~ factor(Foreign) + factor(Post_1980), data = data_auto)
summary(model2)

model3 <- lm(log(mpg) ~ factor(Foreign)*factor(Post_1980), data = data_auto)
summary(model3)

## Quantitative and qualitative Independent Variables
model4 <- lm(log(mpg) ~ factor(Post_1980) + weight, data = data_auto)
summary(model4)

data_auto$log_mpg_hat <- model4$fitted.values
ggplot(data_auto, aes(x=weight, y=log(mpg), group=factor(Post_1980), color=factor(Post_1980))) +
  geom_point(size=0.5) +
  geom_line(aes(y=log_mpg_hat), size=2, alpha=0.6)


model5 <- lm(log(mpg) ~ factor(Post_1980) * weight, data = data_auto)
summary(model5)

anova(model4,model5)


model6 <- lm(log(mpg) ~ factor(Foreign) * horsepower, data = data_auto)
summary(model6)

data_auto$log_mpg_hat <- model6$fitted.values
ggplot(data_auto, aes(x=horsepower, y=log(mpg), group=factor(Foreign), color=factor(Foreign))) +
  geom_point(size=0.5) +
  geom_line(aes(y=log_mpg_hat), size=2, alpha=0.6)
