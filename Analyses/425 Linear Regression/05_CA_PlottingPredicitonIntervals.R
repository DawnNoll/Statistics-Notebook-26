View(faithful)
plot(waiting ~ eruptions, data=faithful)
faith.lm <- lm(waiting ~ eruptions, data=faithful)
abline(faith.lm)
grid()
?faithful
predict(faith.lm, data.frame(eruptions=2))
summary(faith.lm)


predict(faith.lm, data.frame(eruptions=2),
        interval="prediction")

predict(faith.lm, data.frame(eruptions=2),
        interval="confidence")

library(tidyverse)
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point( ) + 
  geom_smooth(method="lm", se=T, formula=y~x) + 
  geom_segment(x=2, xend=2, y=43.23, yend=66.63, color="orange", alpha=0.01, lwd=3)



library(mosaic)
ggplot(KidsFeet, aes(x=width, length)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T, formula=y~x)
