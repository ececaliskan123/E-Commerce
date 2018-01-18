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
# Hyperparameters with CV
(retail_model_rf <- ranger(fmla, # formula
                           tr,  #data
                           num.trees = 200,
                           mtry = 6,
                           num.threads = 1,
                           verbose = FALSE,
                           respect.unordered.factors = TRUE))
# Hyperparameters with bootstrap 632, 10 folds CV
(retail_model_rf2 <- ranger(fmla, # formula
                           tr,  #data
                           num.trees = 800,
                           mtry = 6,
                           num.threads = 1,
                           verbose = FALSE,
                           respect.unordered.factors = TRUE))
# Make predictions on the ts data
ts$pred <- predict(retail_model_rf, ts)$predictions
ts$pred2 <- predict(retail_model_rf2, ts)$predictions
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
retail$pred2 <- predict(retail_model_rf2, retail)$predictions
retail1 <- subset(retail, select = c(order_item_id, pred))
retail2 <- subset(retail, select = c(order_item_id, pred2))
csv <- write_csv(retail1, 'randomforest_probability_retail_5CV.csv')
csv1 <- write_csv(retail2, 'randomforest_probability_retail_bootstrap632.csv')

# Accuracy
prob.pred_known  = as.vector(retail$pred)
class.pred_known  = ifelse(prob.pred_known > 0.5, "1", "0")
confusionMatrix(data = class.pred_known, reference = retail$return2, positive = "1")

prob.pred_known  = as.vector(retail$pred2)
class.pred_known  = ifelse(prob.pred_known > 0.5, "1", "0")
confusionMatrix(data = class.pred_known, reference = retail$return2, positive = "1")


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

#### Parameter tuning with 'mlr'
# Cross validation with 'mlr'
library(mlr)
library(tidyr)
library(ranger)

retail2 <- retail
retail2 <- subset(retail2, select = -c(delivery_date))
retail2 <- separate(retail2, col = order_date , into = c("order_year", "order_month", "order_day"), sep="-") 
retail2 <- separate(retail2, col = user_reg_date, into = c("user_reg_year", "user_reg_month", "user_reg_day"), sep="-")

head(retail2)

retail2$order_year <- as.numeric(retail2$order_year)
retail2$order_month <- as.numeric(retail2$order_month)
retail2$order_day <- as.numeric(retail2$order_day)
retail2$user_reg_year <- as.numeric(retail2$user_reg_year)
retail2$user_reg_month <- as.numeric(retail2$user_reg_month)
retail2$user_reg_day <- as.numeric(retail2$user_reg_day)
retail2$month_of_delivery <- as.numeric(retail2$month_of_delivery)

retail2 <- createDummyFeatures(retail2, target = "return2")

# Splitting the data into a test and a training set 
set.seed(124)
n <- nrow(retail) 
sample.size <- ceiling(n*0.8)
idx.train2 <- createDataPartition(y =retail2$return2, p = 0.75, list = FALSE) 
tr1 <- retail3[idx.train, ]  # training set
ts1 <- retail3[-idx.train, ] # test set 

# Define task and learner
task <- makeClassifTask(data = tr1, target = "return2", positive = "1")
# Set up a structure to save the expected results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()

rf <- makeLearner("classif.ranger", predict.type="prob", predict.threshold = 0.6)
rf
## Tuning
# Hyperparameter setting
# Set the scale/range for your parameters
rf.parms <- makeParamSet(
  # The recommendation for mtry by Breiman is squareroot number of columns
  makeIntegerParam("mtry", lower = 2, upper = 6), # Number of features selected at each node, smaller -> faster
  makeIntegerParam("num.trees", 200, 800) # Number of tree, smaller -> faster
) 
# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
# Sampling strategy
# Given our small dataset, we do 10-fold cross-validation
rdesc <- makeResampleDesc(method = "CV", iters = 10, stratify = TRUE)
rdesc_632 <- makeResampleDesc(method = "Bootstrap", iters = 10, stratify = TRUE, predict = "both")
# Start tuning with the defined options
timing <- list()
timing[["simple"]] <- system.time(
  tuning <- tuneParams(rf, task = task, resampling = rdesc,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
)
timing[["simple"]] <- system.time(
  tuning <- tuneParams(rf, task = task, resampling = rdesc_632,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
)
# Extract optimal parameter values after tuning 
tuning$x
# Investigate the results in detail
tuning_results <- generateHyperParsEffectData(tuning, partial.dep = TRUE)
# Get the performance for each parameter combination
tuning_results$data
tapply(tuning_results$data$auc.test.mean, INDEX = c(tuning_results$data$mtry), mean)
# Update the learner to the optimal hyperparameters
rf
rf_tuned <- setHyperPars(rf, par.vals = tuning$x)
rf_tuned #Hyperparameters: num.threads=1,verbose=FALSE,respect.unordered.factors=TRUE,mtry=6,num.trees=200
# Train the model on the full training data (not only a CV-fold)
modelLib[["rf"]] <- mlr::train(rf_tuned, task = task)
print(modelLib[["rf"]])
# Make prediction on test data
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = ts1)
str(yhat[["rf"]])
# Calculate AUC performance on test set 
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)
auc[["rf"]] 




