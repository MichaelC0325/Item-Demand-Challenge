library(prophet)
library(vroom)
library(tidymodels)
library(modeltime)
library(timetk)

# libraries and loading ---------------------------------------------------

trainData_full <- vroom("train.csv")
testData_full <- vroom("test.csv")

#filter to a single store item
trainData <- trainData_full |> 
  filter(store == 5, item == 35) |> 
  select(date, sales)

testData <- testData_full |> 
  filter(store == 5, item == 35) |> 
  select(date)

# code --------------------------------------------------------------------

cv_split <- time_series_split(data = trainData, date_var = date, assess = "3 month", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
set_engine(engine = "prophet") %>%
fit(sales ~ date, data = training(cv_split))

## Calibrate (i.e. tune) workflow


## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(prophet_model,
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
