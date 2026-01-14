View(airquality)
?airquality
library(tidyverse)

plot(Temp ~ Wind, data=airquality)

# histogram
ggplot(airquality, aes(x=Temp)) + 
   geom_histogram(fill="skyblue", color="steelblue4", bins=9) +
   labs(title = "Maximum daily htemperature in degrees Fahrenheit at LaGuardia Airport, NY, USA",
                  subtitle ="May to September 1973",
                  x="Number of Days in Temperature Range",
                  y="Temperature in degrees F")

# boxplot
 ggplot(airquality, aes(x=factor(Month), y=Temp)) + 
   geom_boxplot(fill="skyblue", color="steelblue4") +
   labs(title = "Maximum daily htemperature in degrees Fahrenheit at LaGuardia Airport, NY, USA",
                  subtitle ="May to September 1973",
                  x="Month of the Year",
                  y="Temperature in degrees F")

# mean and sd
mean(airquality$Temp)

sd(airquality$Temp)

# scatterplot
ggplot(airquality, aes(x=Wind, y=Temp)) + 
  geom_point(pch=21, bg="skyblue", color="steelblue4") + 
  geom_smooth(method="lm", se=F, formula=y~x,
              color="steelblue4")

# lm and predict
mylm <- lm(Temp ~ Wind, data=airquality)
summary(mylm)

predict(mylm, data.frame(Wind=19))
# 90-1.23*19

# filter by Month
 airquality %>%
   group_by(Month) %>%
              summarize(aveTemp = mean(Temp))

airquality %>%
   group_by(Month) %>%
   summarize(SD = sd(Temp))