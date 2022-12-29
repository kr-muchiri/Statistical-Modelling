######################################################
##  R Coding Session 2, Simple Linear Regression    ##
######################################################

## Reset memory
rm(list=ls())

## Set work directory
setwd("~/Dropbox/Centre College Courses/DSC205_Fall2022/R Intro")

## Call packages
library(ISLR)
library(ggplot2)
library(dplyr)

## Example 1 on the slides

# Construct the dataset
X <- c(1,2,3,4,5,6)
Y <- c(1,2,2,3,5,5)
data <- as.data.frame(cbind(X,Y))

# Calculate manually
SS_xx <- sum((data$X-mean(data$X))^2)
SS_xy <- sum(data$X*data$Y)-nrow(data)*mean(data$X)*mean(data$Y)

beta1_hat <- SS_xy/SS_xx
beta0_hat <- mean(data$Y)-beta1_hat*mean(data$X)

SSE <- sum((data$Y-(beta0_hat+beta1_hat*data$X))^2)

s <- sqrt(SSE/(nrow(data)-2))

s_beta1_hat <- s/sqrt(SS_xx)

SS_yy <- sum((data$Y-mean(data$Y))^2)
R_Square <- (SS_yy-SSE)/SS_yy

#95% C.I.
t_score <- qt(0.025, nrow(data)-2, lower.tail = FALSE )
upper_bond <- beta1_hat+t_score*s_beta1_hat
lower_bond <- beta1_hat-t_score*s_beta1_hat

# Hypothesis testing, beta1=0?
t_statistics <- (beta1_hat-0)/s_beta1_hat
P_value <- 2*pt(t_statistics, nrow(data)-2, lower.tail = FALSE )

# Simple Linbear Regression
Result <- lm(Y ~ X, data)
summary(Result)









## Regression







