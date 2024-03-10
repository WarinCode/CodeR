### การวิเคราะห์การถดถอย และ สหพันธ์อย่างง่าย

library(tidyverse)
library(broom)
library(psych)
library(modelr)

data <- read.csv(file="data.csv")
noodle <- data$NumberOfTimesEatingNoodlePerAWeek
pay <- data$Pay

df <- data.frame(noodle,pay)
describe(df)

df %>% ggplot(aes(x=noodle, y=pay)) + geom_point() + stat_smooth(method="lm", se=FALSE)

model <- lm(noodle ~ pay , data=df)
model

summary(model)