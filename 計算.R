
library(dplyr)
library(magrittr)
library(ggplot2)


x <- c(26,25,30,37,8,20,12)
y<- c(14.3,  10.2, 16.8, 12, 8.2, 3.7 ,2.9)
plot(x,y)
result <- lm(y~x)
abline(result)
