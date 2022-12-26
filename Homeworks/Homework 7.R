######################################################
##  R Code for homework 7                           ##
######################################################

## Reset memory
rm(list=ls())

## Set work directory
setwd("~/Dropbox/Centre College Courses/DSC205_Fall2022/R Intro")

## Call packages
library(ISLR)
library(ggplot2)


## Question 1
x <- c(1,2,3,4,5)
y <- c(0,1,2,5,2)

data <- as.data.frame(cbind(x,y))

# Part 1
SS_xy <- sum(data$x*data$y)-nrow(data)*mean(data$x)*mean(data$y)
SS_xx <- sum((data$x-mean(data$x))^2)
beta_1_hat <- SS_xy/SS_xx
beta_0_hat <- mean(data$y)-mean(data$x)*beta_1_hat

# Part 2
SS_yy <- sum((data$y-mean(data$y))^2)

data$y_hat <- beta_0_hat+data$x*beta_1_hat

SSE <- sum((data$y-data$y_hat)^2)

R_sq <- (SS_yy-SSE)/SS_yy

# Part 3
s_sq <- SSE/(nrow(data)-2)
s <- sqrt(s_sq)
s_beta_1_hat <- s/(sqrt(SS_xx))

# Part 4
t <- (beta_1_hat-0)/s_beta_1_hat
P_value <- 2*pt(t, nrow(data)-2, lower.tail = FALSE)

# Part 5
result <- lm(y~x, data = data)
summary(result)

ggplot(data, aes(x=x,y=y))+
  geom_point() +
  geom_line(aes(y=y_hat))

## Question 2

x <- c(1,2,3,4,5)
y <- c(0,2,3,5,3)

data <- as.data.frame(cbind(x,y))
# Part 1
beta_1_hat <- sum(data$x*data$y)/(sum(data$x^2))

# Part 2
SS_yy <- sum((data$y-mean(data$y))^2)

data$y_hat <- data$x*beta_1_hat

SSE <- sum((data$y-data$y_hat)^2)

R_sq <- (SS_yy-SSE)/SS_yy

# Part 3
s_sq <- SSE/(nrow(data)-1)
s <- sqrt(s_sq)
s_beta_1_hat <- s/(sqrt(sum(data$x^2)))

# Part 4
t <- (beta_1_hat-0)/s_beta_1_hat
P_value <- 2*pt(t, nrow(data)-1, lower.tail = FALSE)

# Part 5
result <- lm(y~0+x, data = data)
summary(result)

data$y_hat <- result$fitted.values

ggplot(data, aes(x=x,y=y))+
  geom_point() +
  geom_line(aes(y=y_hat))


## Question 3 
data_auto <- Auto

#Part 1
American <- subset(data_auto, origin == 1)
Japanese <- subset(data_auto, origin == 3)
t.test(American$mpg,Japanese$mpg,alternative = "less")

# Part 2 & 3
result <- lm(mpg ~ horsepower, data = data_auto)
summary(result)

data_auto$mpg_hat <- result$fitted.values

ggplot(data_auto, aes(x=horsepower,y=mpg))+
  geom_point() +
  geom_line(aes(y=mpg_hat))

# Or take log on mpg:
data_auto$log_mpg <- log(data_auto$mpg)
result <- lm(log_mpg ~ horsepower, data = data_auto)
summary(result)

data_auto$mpg_hat <- exp(result$fitted.values)

ggplot(data_auto, aes(x=horsepower,y=mpg))+
  geom_point() +
  geom_line(aes(y=mpg_hat))




