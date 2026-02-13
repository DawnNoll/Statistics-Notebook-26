library(mosaic)
library(tidyverse)
library(car)

# 1 Perform a regression using the airquality data set in R where the response variable is Ozone and the explanatory variable is Wind.
#   What is the value of the MSE of this regression rounded to the nearest whole number?
View(airquality)
aqlm <- lm(data=airquality, Ozone~Wind)
summary(aqlm)
sum( aqlm$res^2 ) / 114

# 2 Use the RailTrail data set in R from library(mosaic) to fit a regression to the following scatterplot. Compute the value of the residual for the solid dot labeled in the plot.
View(RailTrail)
RailTrail[25,]
rtlm <- lm(hightemp ~ lowtemp, data=RailTrail)
summary(rtlm)
61-predict(rtlm, data.frame(lowtemp=35))

# 3 accurate written equation

# 4 What is the value of the MSE for the simple linear regression depicted in the plot?
sum(6.6^2, 13.8^2, 9.8^2, 4.6^2, 2^2)/3

# 5 For the Residuals vs Fitted plot shown, what is the value of for the solid orange dot?

# 6 Suppose a researcher developed the theory that each one degree increase in daily average temperatures reduced average daily wind speed by 1/6 a mile per hour.
aq2lm <- lm(data=airquality, Wind~Temp)
summary(aq2lm)
calculate_t_value <- function(estimate, std_error, number=0){
  # number is the value from hypotheses eg. H0:B0=3)
  (estimate - number)/std_error
}
t_value <- calculate_t_value(-0.17046, 0.02693, -0.167)
2*pt(-abs(t_value), 151)

# 7 Suppose the impossible happened and May 1st of a certain year ended up being 0 degrees Fahrenheit in New York City. 
#   What daily average wind speed (in miles per hour) does your regression model expect for such a day?
?airquality
predict(aq2lm, data.frame(Temp=0))


# 8 A regression is performed, and the following output obtained.
#   Use this output to create a 95% confidence interval for the true y-intercept 
#   (B0) corresponding to this regression.
calculate_conf <- function(estimate, min=0.025, max=0.975,
                           dof, std_error){
  # min and max for qt function)
  estimate + qt(c(min, max), dof)*std_error
}
calculate_conf(1.9845, dof=37, std_error=.3521)

??# 9 Use the mtcars data set in R to perform a log(Y) transformation regression of mpg predicted by wt. 
# log transform is lamba of 0
lambda <- 0
loglm <-lm(log(mpg) ~ wt, data=mtcars)
summary(loglm)
predict(loglm, data.frame(wt=2))
exp(3.288344 )


# 10 see image
plot(high_temp ~ high_dewpt, data=Weather)
wlm <- lm(high_temp ~ high_dewpt, data=Weather)
predict(wlm, data.frame(high_dewpt=80), interval="prediction")

# 11 definition

# 12 kids feet regress girs width~ length
# Select the interval below that, according to your regression, we are 95% confident that it contains 95% of the actual widths of fourth-grade girl's feet that are 22 centimeters long.
?KidsFeet
GirlsFeet <- filter(KidsFeet, sex == "G")
gflm <- lm(data=GirlsFeet, width~length)
predict(gflm, data.frame(length=22), interval = "confidence")

#13 The solid orange dot shown in the scatterplot below corresponds with which point on the residuals vs. fitted plot?
The Chrysler has a wt of 5.345 and a weight of 14.7 or nothing because I think the Chrysler pt shoud be near 15 on the fitted (x)
crlm <- lm(mpg~wt, data=mtcars)
predict(crlm, data.frame(wt=5.345))

??#14 Consider the following scatterplot. Apply an appropriate Y transformation to the data shown in the plot.
# State the R-squared value of the regression on the transformed data.
aqtrlm <- lm(sqrt(sqrt(Ozone)) ~ Temp, data=airquality)
boxCox(aqtrlm) # 0.25
lm(sqrt(sqrt(Y)) ~ X)
loglm <-lm(log(mpg) ~ wt, data=mtcars)
summary(aqtrlm)
predict(loglm, data.frame(wt=2))
exp(3.288344 )

#15 Perform a linear regression that predicts the weight of chickens according to how old the chick is in days.
# Which of the following correctly diagnoses the appropriateness of this regression?
?ChickWeight
cwlm <- lm(data=ChickWeight, weight~Time)
summary(cwlm)
windows(width = 10, height = 5)
par(mfrow=c(1,3))
plot(cwlm, which = 1:2) #1,3 & 2
plot(cwlm$residuals) #5


??17 rse 
1.875*22
?mpg
 
?mpg
View(mpg)
plot(hwy ~ cty, data = mpg)
mpg.lm <- lm(hwy ~ cty, data=mpg)
summary(mpg.lm)

19 
t_value <- calculate_t_value(-109.736, 374.182)
t_value
2*pt(-abs(-0.293), 26)

23
?cars
View(cars)


24
?Loblolly
view(Loblolly)
plot(height ~ age, data=Loblolly)
lmlob <- lm(height ~ age, data=Loblolly)
abline(lmlob)
plot(lmlob, which=1)
