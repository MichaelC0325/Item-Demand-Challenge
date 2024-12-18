### Forecasting using XGBoost
library(readr)
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(dplyr)
library(caret)
library(xgboost)
library(Ecdat)
library(plyr) 
library(dbscan)
library(tidyverse)
library(data.table)
library(Matrix)
library(psych)
library(gbm)

### Read in the datasets
store.train <- read_csv('/kaggle/input/demand-forecasting-kernels-only/train.csv')
store.test <- read_csv('/kaggle/input/demand-forecasting-kernels-only/test.csv')

store <- bind_rows(store.train, store.test, .id = 'me') %>% 
  select(-me)

# Pull out weekdays, months and year
store$dayofweek <- store$date %>% weekdays()
store$month <- store$date %>% month()
store$year <- store$date %>% year()
store <- store %>% select(-date)

#################
## Predictions ##
#################

#split
store.train <- store %>% filter(!is.na(sales))
store.test <- store %>% filter(is.na(sales))
plot_missing(store.train)


#10 folds repeat 3 times
control <- trainControl(method='repeatedcv',
                        number=3,
                        repeats=2)
grid_default <- expand.grid(
  nrounds = 250,
  max_depth = 10,
  eta = 0.3,
  gamma = 15,
  colsample_bytree = .5,
  min_child_weight = 25,
  subsample = c(1:2))

#Metric compare model is Accuracy
metric <- "Accuracy"
xgb_default <- train(sales ~ .,
                     data=store.train %>% select(-id),
                     method='xgbTree',
                     trControl=control,
                     tuneGrid = grid_default)

saveRDS(xgb_default, 'xgb_default.RDS')

names(xgb_default)
plot(xgb_default)
xgb_default$bestTune


predictions <- data.frame(id=store.test$id, sales = (predict(xgb_default, newdata=store.test)))

## write to a csv
write.csv(predictions,"/kaggle/working/submission.csv", row.names = FALSE)