##########################################
## R Coding Session 1                   ##
##########################################
## Introduced Rstudio Interface

## Reset memory
rm(list=ls())


## Set work directory
setwd("~/Dropbox/Centre College Courses/DSC205_Fall2022/R intro")

## Call packages
library(ISLR)
library(ggplot2)
library(dplyr)
library(scales)

# Auto is a built-in dataset in package ISLR, and we create a dataset called "data" for it
Auto_Data <- Auto

## Snapshot of how data looks like
head(Auto_Data)

## Out data
save(Auto_Data, file="Auto_Data.Rdata")

write.csv(Auto_Data, file="Auto_Data.csv")

## Load the data
load("Auto_Data.Rdata")

Auto_Data <- read.csv("Auto_Data.csv")

################################################################
## Data Visulization 

## Reset memory
rm(list=ls())
load("Auto_Data.Rdata")

## Create new variable
Auto_Data$`Origin of Car` <- NA
Auto_Data$`Origin of Car`[which(Auto_Data$origin==1)] <- "American"
Auto_Data$`Origin of Car`[which(Auto_Data$origin==2)] <- "European"
Auto_Data$`Origin of Car`[which(Auto_Data$origin==3)] <- "Japanese"

## Drop variables/columns
Auto_Data1 <- Auto_Data[-8]
Auto_Data1 <- Auto_Data[c("mpg","weight")]


##Qulitative Data Visualization
##Bar Chart with count/frequency
ggplot(Auto_Data, aes(x=`Origin of Car`)) +
  geom_bar(color="black", fill="yellow") + 
  ggtitle("Orgin of Many Cars") +
  ylab("Frequency") +
  xlab("Which County?") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color="blue"),
        plot.title = element_text(color="red", size=30, face="bold.italic", hjust=0.5),
        axis.text = element_text(color="purple",size=15, face="italic"),
        axis.title = element_text(color="brown", size=20, face="bold"))
  
##Bar Chart with relative frequency
ggplot(Auto_Data, aes(x=`Origin of Car`)) +
  geom_bar(aes(y = (..count..)/(sum(..count..))), color="black", fill="yellow") + 
  ggtitle("Orgin of Many Cars") +
  ylab("Relative Frequency") +
  xlab("Which County?") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color="blue"),
        plot.title = element_text(color="red", size=30, face="bold.italic", hjust=0.5),
        axis.text = element_text(color="purple",size=15, face="italic"),
        axis.title = element_text(color="brown", size=20, face="bold"))


## Pie Chart
Count_by_Orgin=count(Auto_Data, `Origin of Car`)
ggplot(Count_by_Orgin, aes(x="", y=n, fill=`Origin of Car`)) + 
  geom_bar(position = "stack", stat = "identity") + 
  ggtitle("Origin of Many Cars") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label=percent(n/(sum(n)))), size = 3, position = position_stack(vjust=0.5)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(color="red", size=30, face="bold.italic", hjust=0.5))


## Qunatitative Data Summarization and Visualization

## Histogram of MPG count/frequency
ggplot(Auto_Data, aes(x=mpg)) +
  geom_histogram(binwidth = 2)

## Histogram of MPG relative frequency
ggplot(Auto_Data, aes(x=mpg)) +
  geom_histogram(binwidth = 2, aes(y=(..count..)/(sum(..count..)))) +
  ylab("Relative Frequency")

## Probability Density of MPG
ggplot(Auto_Data, aes(x=mpg)) +
  geom_density(col="red", linetype="dashed")

## Stem-and-Leaf
stem(Auto_Data$mpg, scale=2)


## Statistics
# Mean
mean(Auto_Data$mpg)

## Variance and Standard Deviation
var(Auto_Data$mpg)
sd(Auto_Data$mpg)

## Median
median(Auto_Data$mpg)

## Range
range(Auto_Data$mpg)
max(Auto_Data$mpg)
min(Auto_Data$mpg)
max(Auto_Data$mpg)-min(Auto_Data$mpg)

## In case of missing value
a=c(1,2,3,4,5,NA,10,12,15,21)
mean(a, na.rm = T)
var(a, na.rm = T)


#################################################################
## Normal Distribution
rm(list=ls())

## Simulate normal distribution
simulate_normal <- as.data.frame(rnorm(50000, mean = 0, sd = 1))
names(simulate_normal)[1] <- "Simulated Normal Distribution"

## Plot the simulated normal distribution: histogram
ggplot(simulate_normal, aes(x = `Simulated Normal Distribution`)) +
  geom_histogram(binwidth = 0.1)

## Plot the simulated normal distribution: density
ggplot(simulate_normal, aes(x = `Simulated Normal Distribution`)) +
  geom_density()

setwd("~/Dropbox/Centre College Courses/DSC205_Fall2022/R intro")
wd <- getwd()
Figure_Save <- file.path(wd, "Sim_Normal.jpg")
jpeg(file=Figure_Save, width=1000, height=500)
ggplot(simulate_normal, aes(x = `Simulated Normal Distribution`)) +
  geom_density()
dev.off()

## Probability of Normal Distribution
# P(y<2.16) if y follows N(mean=0, sd=1)?
pnorm(2.16, mean=0, sd =1, lower.tail = TRUE)
# P(y<-0.58) if y follows N(mean=2, sd=2.5)?
pnorm(-0.58, mean=2, sd =2.5, lower.tail = TRUE)
# P(y>1.12) if y follows N(mean=2, sd=2.5)?
pnorm(1.12, mean=2, sd =2.5, lower.tail = FALSE)

## Quantile of Normal Distribution
qnorm()



