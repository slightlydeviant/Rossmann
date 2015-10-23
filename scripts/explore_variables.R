# explore_variables.R

library(dplyr)
library(tidyr)
library(ggplot2)

store <- read.csv("data/store.csv")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")


# Store Data -----
summary(store$StoreType) # a,b,c,d: 602, 17, 148, 348 

# Training Data -----
train$Date <- as.Date(train$Date, "%Y-%m-%d")
