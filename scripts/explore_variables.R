# explore_variables.R
# Purpose: Clean data, create new variables, plot and explore, transformations

library(dplyr)
library(tidyr)
library(ggplot2)

store <- read.csv("data/store.csv")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")


# Store Data -----
names(store)
head(store)
str(store)

# Training Data -----
names(train)
head(train)
str(train)

# change variable classes
train$Store <- factor(train$Store)
train$Date <- as.Date(train$Date, "%Y-%m-%d")
train$Weekend <- factor(ifelse(train$DayOfWeek %in% c(6, 7), "Weekend", "Weekday"))
train$DayOfWeek2 <- factor(train$DayOfWeek, levels = 1:7, 
                           labels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday", "Sunday"))
train$Open <- factor(train$Open)
train$Promo <- factor(train$Promo)
train$SchoolHoliday <- factor(train$SchoolHoliday)
str(train)

summary(train)
table(train$Store)  # Every store has 942 days, ie. no missing data


