##############################################
## R Coding Session 3, Multiple Regression  ##
##############################################

rm(list=ls())

library(ISLR)

data_auto <- Auto

model1 <- lm(mpg ~ weight, data_auto)
summary(model1)

SST <- sum((data_auto$mpg-mean(data_auto$mpg))^2)
SSE <- sum((data_auto$mpg-model1$fitted.values)^2)
SSR <- sum((model1$fitted.values-mean(data_auto$mpg))^2)

k <- 1
n <- nrow(data_auto)

F_Statistics <- (SSR/k)/(SSE/(n-(k+1)))

pf(F_Statistics, k, n-(k+1), lower.tail = FALSE)

#####
model2 <- lm(mpg ~ weight + horsepower + displacement, data = data_auto)
summary(model2)

SSE <- sum((data_auto$mpg-model2$fitted.values)^2)
SSR <- sum((model2$fitted.values-mean(data_auto$mpg))^2)

k <- 3
F_Statistics <- (SSR/k)/(SSE/(n-(k+1)))
P_Value <- pf(F_Statistics, k, n-(k+1), lower.tail = FALSE)

## F test between two nested models
model_reduced <- lm(mpg~weight + horsepower, data = data_auto)
summary(model_reduced)

model_complete <- lm(mpg~weight + horsepower + displacement + acceleration, data = data_auto)
summary(model_complete)

SSE_r <- sum((data_auto$mpg-model_reduced$fitted.values)^2)
SSE_c <- sum((data_auto$mpg-model_complete$fitted.values)^2)

n <- nrow(data_auto)
g <- 2
k <- 4

F_stat <- ((SSE_r-SSE_c)/(k-g))/(SSE_c/(n-(k+1)))
P_value <- pf(F_stat, k-g, n-(k+1), lower.tail = FALSE)

anova(model_reduced, model_complete)


sum_model_reduced <- summary(model_reduced)
R_a_reduced <- 1-(n-1)/(n-(g+1))*(1-sum_model_reduced$r.squared)

## Dummy variables for qualitative variables

data_auto$American <- 0
data_auto$American[which(data_auto$origin==1)] <- 1

data_auto$Japan <- 0
data_auto$Japan[which(data_auto$origin==3)] <- 1


 








