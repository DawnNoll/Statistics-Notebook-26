library(tidyverse)

set.seed(121)


N <- 50000
beta0 <- 3
beta1 <- 2.5

sigma <- 1.2

X <- rnorm(N, 30, 5)
Y <- beta0 + beta1*X + rnorm(N, 0, sigma)

plot(Y ~ X, col=rgb(.1,.1,.1,.01), pch=16)

n <- 5
mysample1 <- sample(N, n)

points(Y[mysample1] ~ X[mysample1], col="hotpink", pch=16)
mylm1 <- lm(Y[mysample1] ~ X[mysample1])
summary(mylm1)
abline(mylm1, col="hotpink")
abline(beta0, beta1, col="green1")



n <- 5
mysample2 <- sample(N, n)

points(Y[mysample2] ~ X[mysample2], col="lemonchiffon", pch=16)
mylm2 <- lm(Y[mysample2] ~ X[mysample2])
summary(mylm2)
abline(mylm2, col="lemonchiffon")

mydata <- data.frame(Y=Y[mysample2],X=X[mysample2])

ggplot(mydata, aes(x=X, y=Y)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y~x) + 
  geom_abline(aes(intercept=beta0, slope=beta1), color="green1")





N <- 512  
storage <- rep(NA, N)
storage

for (i in 1:N){
  storage[i] <- 2*i
}

storage


N <- 5000
storage_int <- storage_slope <- rep(NA, N)

for (i in 1:N){

  #Hint 1

  n <- 40
  Xi <- rep(seq(20, 120, length.out=n/2), each=2) #n must be even.
  Yi <- 2.5 + 3*Xi + rnorm(n, 0, 3)

  #Hint 2

  mylm <- lm(Yi ~ Xi)
  #coef(mylm)
  storage_int[i] <- coef(mylm)[1] #intercept only
  storage_slope[i] <- coef(mylm)[2] #slope only

}

#Hint 3

hist(storage_int)
hist(storage_slope)
mean(storage_int)
mean(storage_slope)
sd(storage_int)
sd(storage_int)
sd(storage_slope)
