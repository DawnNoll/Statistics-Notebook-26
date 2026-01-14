library(tidyverse)


ggplot(airquality, aes(x=Wind, y=Temp)) + 
  geom_point(fill="skyblue", pch=21, color="skyblue4") + 
  geom_smooth(method="lm", se=F, formula=y~x, color="skyblue4") + 
  labs(title="Max...", subtitle="May...", 
       y="Temp...", x="Average...")

mylm <- lm(Temp ~ Wind, data=airquality)
summary(mylm)


May <- filter(airquality, Month==5)
ggplot(May, aes(x=Wind, y=Temp)) + 
  geom_point(fill="skyblue", pch=21, color="skyblue4") + 
  geom_smooth(method="lm", se=F, formula=y~x, color="skyblue4") + 
  labs(title="Max...", subtitle="May...", 
       y="Temp...", x="Average...")

mylm <- lm(Temp ~ Wind, data=May)
summary(mylm)
install.packages("car")
library(car)
View(Davis)
?Davis
males <- filter(Davis, sex=='M')
mylm <- lm(weight ~ height, data=males)
summary(mylm)
predict(mylm, data.frame(height=180))


#2
View(USArrests)
?USArrests
mylm <- lm(Murder~Assault, data=USArrests)
summary(mylm)
