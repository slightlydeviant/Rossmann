# erik_model.R
# Purpose: Erik's file to model the data in combo.rds

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

source("scripts/evaluate_function.R")

dat <- readRDS("data/combo.rds") %>%
  filter(Open == 1,
         Sales > 0)

dat %>%
  filter(Store == 1) %>%
  mutate(yr = format(Date, "%Y"),
         doy = as.numeric(format(Date, "%j"))) %>%
  # filter(yr == 2014) %>%
  ggplot(aes(x = doy, y = Sales, color = yr)) +
  geom_line() +
  geom_point(aes(x = doy, y = Sales, 
                 color = Promo)) +
  scale_x_continuous(breaks = seq(1, 365, 30))

inTrain <- createDataPartition(y = dat$Sales, p = .7)$Resample1

simpleFit <- lm(Sales ~ Store + Promo*DayOfWeek2, data = dat, subset = inTrain)
summary(simpleFit)

predSF <- predict(simpleFit, dat[-inTrain, ])

evaluate(dat[-inTrain, "Sales"], predSF)
head(cbind(predSF, dat$Sales[-inTrain]))
