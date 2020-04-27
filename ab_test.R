library(dplyr)
library(broom)

setwd("C://Users/Andy/Desktop/DS/Learning Journal")
data_viz <- read.csv("data_viz_website_2018_04.csv")

library(dplyr)
library(ggplot2)

data_viz$condition <- relevel(data_viz$condition, "tools")

# explore difference between conditions
data_viz_test <- data_viz %>% group_by(condition) %>%
  summarise(
    avg_time_spent = mean(time_spent_homepage_sec),
    clicked_article = mean(clicked_article),
    clicked_like = mean(clicked_like)
  )

# plot results
plot1 <-
  ggplot(data_viz_test, aes(x = condition, y = avg_time_spent)) +
  geom_bar(stat = "identity") + theme(axis.title.x = element_blank())
plot2 <-
  ggplot(data_viz_test, aes(x = condition, y = clicked_article)) +
  geom_bar(stat = "identity") + theme(axis.title.x = element_blank())
plot3 <-
  ggplot(data_viz_test, aes(x = condition, y = clicked_like)) +
  geom_bar(stat = "identity") + theme(axis.title.x = element_blank())
gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 3)

# logistic regression----------------------------------------------
# power analysis
library(pwr)
SSizeLogisticBin(
  p1 = 0.07,
  p2 = 0.17,
  alpha = 0.05,
  power = 0.8,
  B = 0.5
)
# estimate required observations
pwr.p.test(
  n = 1000,
  h = ES.h(p1 = 0.1, p2 = 0.12),
  sig.level = 0.05,
  alternative = "two.sided"
)


library(broom)

# binary (categorical) dependent variables
glm(clicked_article ~ condition, family = "binomial", data = data_viz) %>% tidy()
glm(clicked_like ~ condition, family = "binomial", data = data_viz) %>% tidy()

# continuous dependent variables
t.test(time_spent_homepage_sec ~ condition, alternative = "two.sided", data = data_viz) %>% tidy()
lm(time_spent_homepage_sec ~ condition, data = data_viz) %>% tidy()


# lm & t-test------------------------------------------------------


# estimate required observations for logistic regression
library(powerMediation)
SSizeLogisticBin(p1 = 0.07, p2 = 0.17, alpha = 0.05, power = 0.8, B = 0.5)

# estimate required observations for t-test
library(pwr)
pwr.t.test(power = 0.8, sig.level = 0.05, d = 0.2) %>% tidy() # get sample size
pwr.t.test(n = 400, sig.level = 0.05, d = 0.2) %>% tidy() # get power
