# erik_model.R
# Purpose: Erik's file to model the data in combo.rds

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(forecast)

source("scripts/evaluate_function.R")

dat <- readRDS("data/combo.rds")

dat2 <- dat %>%
  filter(DayOfWeek2 != "Sunday") %>%
  arrange(Store, Date)

dat2 %>%
  filter(Sales == 0) %>%
  dim

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


# Time series -----
# annual seasonality only
store1 <- dat2[which(dat2$Store == 1), ]
s <- ts(store1$Sales, frequency = 313.25)
plot(s)

fit <- tbats(s)
plot(fit)
fc <- forecast(fit)
plot(fc)
plot(s, type = "l", col = "black", lwd = 1.5)
lines(fc$fitted, col = "red", lwd = 3)
plot(s - fc$fitted * store1$Open)
store1$DSSales <- s - fc$fitted * store1$Open

# annual and weekly seasonality
s2 <- msts(store1$Sales, seasonal.periods=c(12, 313.25))
plot(s2)

fit2 <- tbats(s2)
plot(fit2)
fc2 <- forecast(fit2)
plot(fc2)
plot(s2, type = "l", col = "black", lwd = 1.5)
lines(fc$fitted, col = "red", lwd = 3)
plot(s2 - fc2$fitted * store1$Open)
# store1$DSSales <- s - fc$fitted * store1$Open


# Model deseasonalized sales -----
lmfit <- lm(DSSales ~ Promo + DayOfWeek2 + SchoolHoliday + StateHoliday, data = store1)
plot(lmfit)
store1$fitted <- lmfit$fitted.values + (store1$Sales - store1$DSSales)
with(store1[which(store1$Sales > 0), ], evaluate(Sales, fitted))


# All stores ts to lm -----
storelist <- split()
store1 %>%
  mutate(DSSales = as.numeric(Sales - forecast(tbats(ts(Sales, frequency = 313.25)))$fitted * Open))

dssalesfunc <- function(data){
  return(with(data, as.numeric(Sales - forecast(tbats(ts(Sales, frequency = 313.25)))$fitted * Open)))
}
dssalesfunc(store1)

dat3 <- dat2 %>%
  group_by(Store) %>%
  mutate(DSSales = dssalesfunc(.))



# Linear model (dumb) -----
inTrain <- createDataPartition(y = dat$Sales, p = .7)$Resample1

simpleFit <- lm(Sales ~ Store + Promo*DayOfWeek2, data = dat, subset = inTrain)
summary(simpleFit)

predSF <- predict(simpleFit, dat[-inTrain, ])

evaluate(dat[-inTrain, "Sales"], predSF)
head(cbind(predSF, dat$Sales[-inTrain]))



# Linear model, like the LH model -----
lmodel <- dat %>%
  filter(Open == 1) %>%
  mutate(moy = months(Date)) %>%
  select(Store, Sales, StateHoliday, SchoolHoliday, moy, DayOfWeek2)

lfit <- lm(Sales ~ factor(Store) + factor(StateHoliday) + factor(SchoolHoliday) + moy + DayOfWeek2,
           data = lmodel)
