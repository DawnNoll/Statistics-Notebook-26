library(mosaic)
library(tidyverse)

u.lm <- lm(gasbill ~ temp, data=Utilities)
summary(u.lm)

predict(u.lm, data.frame(temp=30), 
        interval="prediction")

ggplot(Utilities, aes(x=temp, y=gasbill)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T, formula=y~x) + 
  geom_segment(x=30, xend=30, y=83.87, yend=189.46,
               color="red", alpha=0.01, lwd=4) + 
  theme_classic()

plot(gasbill ~ temp, data=Utilities)
abline(u.lm)
lines(c(30,30), c(83.87, 189.46), lwd=4, 
      col=rgb(1,0,0, .5))


library(car)
boxCox(u.lm)

u.lm.t <- lm(sqrt(sqrt(gasbill)) ~ temp, data=Utilities)
b <- coef(u.lm.t)

myx <- 70
mypreds <- predict(u.lm.t, data.frame(temp=myx), 
        interval="prediction")^4


ggplot(Utilities, aes(x=temp, y=gasbill)) + 
  geom_point() + 
  #geom_smooth(method="lm", se=T, formula=y~x) + 
  stat_function(fun=function(x) (b[1] + b[2]*x)^4) + 
  geom_segment(x=myx, xend=myx, y=mypreds[2], yend=mypreds[3],
               color="red", alpha=0.01, lwd=4) + 
  theme_classic()

plot(gasbill ~ temp, data=Utilities)
curve((b[1] + b[2]*x)^4, add=TRUE)
lines(c(myx,myx), c(mypreds[2], mypreds[3]), lwd=4, 
      col=rgb(1,0,0, .5))
