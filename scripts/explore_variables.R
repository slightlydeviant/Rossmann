# explore_variables.R
# Purpose: Clean data, create new variables, plot and explore, transformations

library(dplyr)
library(tidyr)
library(ggplot2)

store <- read.csv("data/store.csv")
train <- read.csv("data/train.csv")
# test <- read.csv("data/test.csv")  # don't need this yet

addMonths <- function(date,n) {
  if(is.na(date)){
    x <- NA
  } else {
    x <- seq(date, by = paste (n, "months"), length = 2)[2]
  }
  return(x)
}
  

# Store Data -----
str(store)

store2 <- store %>%
  mutate(Store = factor(Store),
         CompetitionOpenSinceDate = as.Date(paste("01", 
                                                   CompetitionOpenSinceMonth, 
                                                   CompetitionOpenSinceYear, sep = "-"),
                                            format = "%d-%m-%Y"),
         Promo2Started = as.Date(paste("1", Promo2SinceWeek,
                                              Promo2SinceYear, sep = "-"),
                                       format = "%w-%W-%Y"),
         Promo2 = factor(Promo2),
         PromoInterval = ifelse(PromoInterval == "", NA, as.character(PromoInterval)),
         CompetitionDistance = ifelse(is.na(CompetitionDistance), 
                                      mean(store$CompetitionDistance, na.rm = TRUE), 
                                      CompetitionDistance),
         CompetitionOpenSinceDate = ifelse(is.na(CompetitionOpenSinceDate),
                                                 as.Date("1900-01-01"),
                                                 CompetitionOpenSinceDate)) %>%
  separate(col = PromoInterval, into = c("m1", "m2", "m3", "m4"), sep = ",") %>%
  select(-CompetitionOpenSinceMonth,
         -CompetitionOpenSinceYear,
         -Promo2SinceWeek,
         -Promo2SinceYear)

# store$Store <- factor(store$Store)
# store$CompetitionOpenSinceDate <- as.Date(paste('01', 
#                                                 store$CompetitionOpenSinceMonth, 
#                                                 store$CompetitionOpenSinceYear, sep = "-"),
#                                           format = "%d-%m-%Y")
# store$Promo2Started <- as.Date(paste("1", store$Promo2SinceWeek,
#                                      store$Promo2SinceYear, sep = "-"),
#                                format = "%w-%W-%Y")
# store$Promo2 <- factor(store$Promo2)

str(store2)

# Training Data -----
str(train)

train2 <- train %>%
  mutate(Store = factor(Store),
         Date = as.Date(Date, "%Y-%m-%d"),
         Weekend = factor(ifelse(DayOfWeek %in% c(6, 7), "Weekend", "Weekday")),
         DayOfWeek2 = factor(train$DayOfWeek, levels = 1:7, 
                             labels = c("Monday", "Tuesday", "Wednesday", 
                                        "Thursday", "Friday", "Saturday", "Sunday")),
         Open = factor(Open),
         Promo = factor(Promo),
         SchoolHoliday = factor(SchoolHoliday)) %>%
  select(-DayOfWeek)
  
# train$Store <- factor(train$Store)
# train$Date <- as.Date(train$Date, "%Y-%m-%d")
# train$Weekend <- factor(ifelse(train$DayOfWeek %in% c(6, 7), "Weekend", "Weekday"))
# train$DayOfWeek2 <- factor(train$DayOfWeek, levels = 1:7, 
#                            labels = c("Monday", "Tuesday", "Wednesday", 
#                                       "Thursday", "Friday", "Saturday", "Sunday"))
# train$Open <- factor(train$Open)
# train$Promo <- factor(train$Promo)
# train$SchoolHoliday <- factor(train$SchoolHoliday)

str(train2)


# Combine data -----
combo <- store2 %>%
  inner_join(train2, by = "Store") %>%
  mutate(CompetitionOpen = factor(ifelse(Date >= CompetitionOpenSinceDate, 1, 0)),
         Promo2Open = factor(ifelse(Date >= Promo2Started, 1, 0)),
         month = format(Date, "%b"),
         Promo2StartMonth = factor(ifelse(Promo2 == 0, NA,
                                          ifelse(month == m1 | month == m2 | month == m3 | month == m4,
                                                 1, 0)))#,
#          Promo2EndMonth = factor(ifelse(Promo2 == 0, NA,
#                                         ifelse(format(as.Date(addMonths(Promo2Started, 2),
#                                                               origin = "01-01-1970"), "%b") %in% c(m1, m2, m3, m4),
#                                                1, 0)))
         ) %>%
  select(-(m1:m4), -month)

saveRDS(combo, file = "data/combo.rds")
