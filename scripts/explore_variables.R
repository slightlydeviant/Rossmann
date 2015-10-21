# explore_variables.R

library(dplyr)
library(tidyr)
library(ggplot2)

store <- read.csv("data/store.csv")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")


# Store Data -----


# Training Data -----
train$Date <- as.Date(train$Date, "%Y-%m-%d")
