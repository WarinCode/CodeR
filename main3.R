data <- read.csv(file="C:\\Users\\ACER USER5949486\\Desktop\\CodeR\\data.csv")

noodle <- data$EatNoodleTimesAWeek
pay <- data$Pay

combined_data <- cbind(noodle, rice)
combined_data

library(tidyverse)
library(broom)
library(psych)
library(modelr)

# describe(combined_data)

data %>% ggplot(aes(x=noodle, y=pay)) + geom_point() + stat_smooth(method ='lm', se=FALSE)

model <- lm(noodle ~ pay, data=data)
summary(model)