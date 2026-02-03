library(mosaicData)
View(Utilities)
?Utilities

plot(gasbill ~ temp, data=Utilities)

u.lm <- lm(gasbill ~ temp, data=Utilities)

plot(u.lm, which=1)

library(car)
qqPlot(u.lm$residuals)
plot(u.lm$residuals)


boxCox(u.lm)

for (lambda in c(-2, -1, -.5, -.4, -.3, -.2, -.1, .1, .25, .5, 1, 2)) {
  plot(gasbill^lambda ~ temp, data=Utilities, main = paste("lambda = ", lambda))
}


for (lambda in seq(0.5, 0.005, length.out=10)) {
  plot(gasbill^lambda ~ temp, data=Utilities, main = paste("lambda = ", lambda))
}

plot(log(gasbill) ~ temp, data=Utilities)
u.lm.t <- lm(log(gasbill) ~ temp, data=Utilities)
abline(u.lm.t)

summary(u.lm)$coef
summary(u.lm.t)$coef

exp(predict(u.lm.t, data.frame(temp=90)))
predict(u.lm, data.frame(temp=90))


plot(gasbill ~ temp, data=Utilities)
curve(exp(6 - 0.04*x), add=TRUE, col="skyblue")


ggplot(Utilities, aes(x=temp, y=gasbill)) + 
  geom_point() + 
  stat_function(fun=function(x) exp(6 - 0.04*x))


# What does Box-Cox do when no transformation is needed?
x <- runif(30, 10, 20)
y <- 2 + 3.5*x + rnorm(30, 0, 1)
plot(y ~ x)

mylm <- lm(y ~ x)
boxCox(mylm)








## Graphing Transformations Class Activity

# Base Graphic

plot(circumference ~ age, data=Orange, pch=16, col="orangered", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")

# ggplot Graphic

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( )

lm.log <- lm(log(circumference) ~ age, data=Orange)
b.log <- coef(lm.log)
lm.sqrt <- lm(sqrt(circumference) ~ age, data=Orange) 
b.sqrt <- coef(lm.sqrt)
lm.1oy <- lm(1/circumference ~ age, data=Orange) 
b.1oy <- coef(lm.1oy)
lm.y <- lm(circumference ~ age, data=Orange) 
b <- coef(lm.y)
lm.y2 <- lm(circumference^2 ~ age, data=Orange) 
b.y2 <- coef(lm.y2)
lm.ss <- lm(sqrt(sqrt(circumference)) ~ age, data=Orange) 
b.ss <- coef(lm.ss)



ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x),
                aes(color="log")) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2,
                aes(color="sqrt")) + 
  stat_function(fun=function(x) 1/(b.1oy[1] + b.1oy[2]*x),
                aes(color="1/y")) + 
  stat_function(fun=function(x) b[1] + b[2]*x,
                aes(color="y")) + 
  stat_function(fun=function(x) sqrt(b.y2[1] + b.y2[2]*x),
                aes(color="y2")) + 
  stat_function(fun=function(x) (b.ss[1] + b.ss[2]*x)^4,
                aes(color="sqrt-sqrt")) + 
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( )


YoungOrange <- Orange %>%
  filter(age < 1200)

lm.log <- lm(log(circumference) ~ age, data=YoungOrange)
b.log <- coef(lm.log)
lm.sqrt <- lm(sqrt(circumference) ~ age, data=YoungOrange) 
b.sqrt <- coef(lm.sqrt)
lm.1oy <- lm(1/circumference ~ age, data=YoungOrange) 
b.1oy <- coef(lm.1oy)
lm.y <- lm(circumference ~ age, data=YoungOrange) 
b <- coef(lm.y)
lm.y2 <- lm(circumference^2 ~ age, data=YoungOrange) 
b.y2 <- coef(lm.y2)
lm.ss <- lm(sqrt(sqrt(circumference)) ~ age, data=YoungOrange) 
b.ss <- coef(lm.ss)


ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x),
                aes(color="log")) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2,
                aes(color="sqrt")) + 
  #stat_function(fun=function(x) 1/(b.1oy[1] + b.1oy[2]*x),
   #             aes(color="1/y")) + 
  stat_function(fun=function(x) b[1] + b[2]*x,
                aes(color="y")) + 
  stat_function(fun=function(x) sqrt(b.y2[1] + b.y2[2]*x),
                aes(color="y2")) + 
  stat_function(fun=function(x) (b.ss[1] + b.ss[2]*x)^4,
                aes(color="sqrt-sqrt")) + 
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( )
