library(dplyr)
library(magrittr)
library(ggplot2)

#2.1

#分布の特徴
x <- c(52,65,42,55,48,62,95,74,58,52)
hist(x,breaks = seq(0,100,10))
hist(x,breaks = seq(0,100,20))

#確立密度
d<- density(x)
plot(d, xlim =c(30,100) )



#例2.1
x1 <- c(170,173,175,170,172,165,167,168,172,174,168,170,173,178,173,163,172,173,179,172,168,167,168,174,
        167,180,171,181,177,170,170,175,169,175,173,171,172,176,176,167,173,172,168,168,171,170,176,173,
        170,185)
hist(x1)

#2.2
x2<- c(52,65,42,55,48,62,95,74,58,52)
mean(x2)
heikinn<- sum(x2)/10
heikinn

a<- 70

#平均値の性質
sum((x2-a)^2)
sum((x2-mean(x2)+mean(x2)-a)^2)

#sum( ( x2-mean(x2) )^2 ) +  2 * sum( (x2-mean(x2)) * (mean(x2)-a) )  + sum((mean(x2) -a)^2 )

sum( (x2-mean(x2))^2) + 2*(mean(x2)-a) *sum( x2-mean(x2) ) + 10* ((mean(x2) -a)^2 ) 


sum((x2-a)^2)
sum( (x2-mean(x2))^2)+10* ((mean(x2) -a)^2 )

#平均値の問題点
xa<- c(25,32,28,200,30)
xb<- c(35,42,38,50,40)

xa_mean <- mean(xa)
xa_mean
xb_mean <- mean(xb)
xb_mean

#異常値がわかったり
xa_median <- median(xa)
xa_median
xb_median <- median(xb)
xb_median

#medianの性質(奇数偶数)
xa1_median <- median(2*xa + 3)
xa1_median
xa2_median <- 2* median(xa) +3
xa2_median

sum(abs(xa - 30))
sum(xa-30)
sum(abs(xa - 80))
sum(abs(xa - median(xa)))

#mode(names関数)
xc <- c(1,1,1,2,2,2,2,2,2,4,3,5,3,3,3)

statmode <- function(x){
  names(which.max(table(x)))
}
mode<- statmode(xc)
mode

#刈り込み平均
xa<- c(25,32,28,200,30)
xb<- c(35,42,38,50,40)
mean(sort(xa)[2:4] )
mean(sort(xb)[2:4] )

#幾何平均
kika<-1
for(i in 1:(length(xa))){
  kika <- kika*xa[i]
}
kika^(1/length(xa))

