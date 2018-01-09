#### Ece's data cleaning base
## row 99 in Known.r file, retail$return2 <- retail$return 
## I need 0,1 so I add row 99 because ece switched return class like retail$return <- ("keep","return")

### Random forest
library(ranger)
library(dplyr)
library(caret)
# From Parameter tuning file
set.seed(124)
n <- nrow(retail) 
sample.size <- ceiling(n*0.8)
idx.train <- createDataPartition(y =retail$return2, p = 0.75, list = FALSE) 
tr <- retail[idx.train, ]  # training set
ts <- retail[-idx.train, ] # test set 
# The return column
return <- "return2"
# The input variables
vars <- c("user_dob", "delivery_duration", "user_maturity",
          "user_state", "item_price", "item_color", "item_size", "month_of_delivery")
# Create the formula string for returns as a function of the inputs
fmla <- paste(return, "~", paste(vars, collapse = " + "))
# Fit and print the random forest model
(retail_model_rf <- ranger(fmla, # formula 
                           tr,   # data
                           num.trees = 500, 
                           respect.unordered.factors = "order"))
# Make predictions on the ts data
ts$pred <- predict(retail_model_rf, ts)$predictions
# Calculate the RMSE of the predictions
ts %>% 
  mutate(residual = return2 - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2)))    # calculate rmse
# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(ts, aes(x = pred, y = return2)) + 
  geom_point() + 
  stat_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) # Smoothed curve w/o standard errors
#Prediction for retail
retail$pred <- predict(retail_model_rf, retail)$predictions
retail1 <- subset(retail, select = c(order_item_id, pred))
csv <- write_csv(retail1, 'randomforest_probability_retail.csv')


#### Prediction
library(readr)
sales$pred <- predict(retail_model_rf, sales)$predictions
sales$pred_return <- with(sales, ifelse(pred < 0.5, 0, 1))
table(sales$pred_return)
# Compare the histograms between the predicted values(Probabilities) of ts(known) and sales(class) 
par(mfrow = c(1, 2)) # 1 row, 2 columns
hist(ts$pred, main = 'Distribution of Predicted Probabilities for Known Data', xlab = 'Predicted Probabilities (Known Data)')
hist(sales$pred, main = 'Distribution of Predicted Probabilities for Class Data', xlab = 'Predicted Probabilities (Class Data)')
#Select only 2 columns for csv files(probability, class)
sales1 <- subset(sales, select = c(order_item_id, pred))
csv <- write_csv(sales1, 'randomforest_probability.csv')
sales2 <- subset(sales, select = c(order_item_id, pred_return))
csv <- write_csv(sales2, 'randomforest_class.csv')





