
# Libraries / loading data ---------------------------------------------------------------

library(vroom)
library(tidymodels)
library(modeltime)
library(timetk)

trainData_full <- vroom("train.csv")
testData_full <- vroom("test.csv")

#filter to a single store item
trainData <- trainData_full |> 
  filter(store == 3, item == 6) |> 
  select(date, sales)

testData <- testData_full |> 
  filter(store == 3, item == 6) |> 
  select(date)

# code --------------------------------------------------------------------

## Create the CV split for time series
cv_split <- time_series_split(data = trainData, date_var = date, assess = "1 month", cumulative = TRUE)

## Create a recipe for the linear model part
arima_recipe <- recipe(sales ~ date, data = trainData) %>%
  step_date(date, features = c("dow", "month", "year", "doy"))

## Define the ARIMA Model
arima_model <- arima_reg(seasonal_period=30,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
set_engine("auto_arima")

## Merge into a single workflow and fit to the training data
arima_wf <- workflow() %>%
add_recipe(arima_recipe) %>%
add_model(arima_model) %>%
fit(data=training(cv_split))

## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize results
cv_plot2 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = training(cv_split)
) %>%
plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
modeltime_refit(data=trainData)

## Predict for all the observations in storeItemTest
forecast_plot2 <- fullfit %>%
modeltime_forecast(
                   new_data = testData,
                   actual_data = trainData
) %>%
plot_modeltime_forecast(.interactive=FALSE)

# plots -------------------------------------------------------------------

plotly::subplot(cv_plot1,cv_plot2,forecast_plot1,forecast_plot2, nrows=2)
