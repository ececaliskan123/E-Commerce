source('load_data.R')

### Random forest
library(ranger)
library(dplyr)
library(caret)
# From Parameter tuning file
set.seed(124)
n <- nrow(df_known) 
sample.size <- ceiling(n*0.8)
idx.train <- createDataPartition(y =df_known$return, p = 0.8, list = FALSE) 
tr <- df_known[idx.train, ]  # training set
ts <- df_known[-idx.train, ] # test set 
# The return column
return <- "return"
# The input variables
vars <- c("user_id", "order_item_id", "order_date","month_of_delivery", "price_and_age",
          "item_id", "item_size", "brand_id", "item_price", "user_dob", "user_reg_date", "delivery_duration")
# Create the formula string for returns as a function of the inputs
fmla <- paste(return, "~", paste(vars, collapse = " + "))
# Fit and print the random forest model
# Hyperparameters with 5 folds CV
(df_known_rfmodel_CV <- ranger(fmla, # formula
                           tr,  #data
                           num.trees = 800,
                           mtry = 6,
                           num.threads = 1,
                           verbose = FALSE,
                           respect.unordered.factors = TRUE))
# Make predictions on the ts data
ts$pred <- predict(df_known_rfmodel_CV, ts)$predictions
# Calculate the RMSE of the predictions
ts %>% 
  mutate(residual = return - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2)))    # calculate rmse
# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(ts, aes(x = pred, y = return)) + 
  geom_point() + 
  stat_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) # Smoothed curve w/o standard errors
#Prediction for df_known
df_known$pred <- predict(df_known_rfmodel_CV, df_known)$predictions
df_known1 <- subset(df_known, select = c(order_item_id, pred))
csv <- write_csv(df_known1, 'randomforest_known.csv')

# Saving optimal rf model
saveRDS(df_known_rfmodel_CV, file = "models/RF_Model_Par.R") 

# Accuracy : 0.9256  
prob.pred_known  = as.vector(df_known$pred)
class.pred_known  = ifelse(prob.pred_known > 0.5, "1", "0")
confusionMatrix(data = class.pred_known, reference = df_known$return, positive = "1")




#### Prediction
library(readr)
df_class$pred <- predict(df_known_rfmodel_CV, df_class)$predictions
df_class$pred_return <- with(df_class, ifelse(pred < 0.5, 0, 1))
table(df_class$pred_return)

# Compare the histograms between the predicted values(Probabilities) of ts(known) and df_class(class) 
par(mfrow = c(1, 2)) # 1 row, 2 columns
hist(ts$pred, main = 'Distribution of Predicted Probabilities for Known Data', xlab = 'Predicted Probabilities (Known Data)')
hist(df_class$pred, main = 'Distribution of Predicted Probabilities for Class Data', xlab = 'Predicted Probabilities (Class Data)')

#Select only 2 columns for csv files(probability, class)
df_class1 <- subset(df_class, select = c(order_item_id, pred))
csv1 <- write_csv(df_class1, 'randomforest_class_probability.csv')
df_class2 <- subset(df_class, select = c(order_item_id, pred_return))
csv2 <- write_csv(df_class2, 'randomforest_class.csv')




#### Parameter tuning for ranger(randomforest) with mlr 
library(mlr)
library(tidyr)
library(ranger)

amend_features = function(dd){
  dd = subset(dd, select = -c(delivery_date))
  dd = subset(dd, select = -c(user_dob, user_maturity, user_title, user_state, item_color))
  
  dd$order_year  = as.numeric(format(dd$order_date, "%Y"))
  dd$order_month = as.numeric(format(dd$order_date, "%m"))
  dd$order_day   = as.numeric(format(dd$order_date, "%d"))
  dd             = subset(dd, select=-order_date)
  
  dd$reg_year  = as.numeric(format(dd$user_reg_date, "%Y"))
  dd$reg_month = as.numeric(format(dd$user_reg_date, "%m"))
  dd$reg_day   = as.numeric(format(dd$user_reg_date, "%d"))
  dd           = subset(dd, select=-user_reg_date)
  
  if("return" %in% colnames(dd)) {
    dd = normalizeFeatures(dd, target="return")
    dd = createDummyFeatures(dd, target="return", cols=c("item_size"))
  } else {
    dd = createDummyFeatures(dd, cols=c("item_size"))
  }
  
  return(dd)
}

#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
#d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
#classdata = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')

### TODO AMEND BLOCK AFTER FEATURE ENGINEERING
retail2 = amend_features(df_known)

# Splitting the data into a test and a training set 
set.seed(124)
n <- nrow(retail2) 
sample.size <- ceiling(n*0.8)
idx.train <- createDataPartition(y =retail2$return, p = 0.8, list = FALSE) 
tr1 <- retail2[idx.train, ]  # training set
ts1 <- retail2[-idx.train, ] # test set 

# Define task and learner
task <- makeClassifTask(data = tr1, target = "return", positive = "1")
# Set up a structure to save the expected results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()

rf <- makeLearner("classif.ranger", predict.type="prob")
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
# Given our small dataset, we do 5-fold cross-validation
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
# Start tuning with the defined options
timing <- list()
timing[["simple"]] <- system.time(
  tuning <- tuneParams(rf, task = task, resampling = rdesc,
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
auc[["rf"]] #0.9118319 







