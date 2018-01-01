# Neural network with package nnet

getwd()

if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("pROC")) install.packages("pROC"); library("pROC")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("foreach")) install.packages("foreach"); library("foreach")

source("load_data.R")

retail<-read_and_preprocess_data_file("data/BADS_WS1718_known.csv")

# Convert target variable into a factor
retail$return<-factor(retail$return, levels = c(0,1), labels = c("No Return","Return"))

set.seed(123)

# Creating training and test data set
idx.train<-createDataPartition(y = retail$return, p = 0.8, list = FALSE)
tr<-retail[idx.train,] # is the unstandardized training set
ts<-retail[-idx.train,] # is the unstandardized test set

normalizer<-caret::preProcess(tr, method = c("center", "scale"))
tr<-predict(normalizer, newdata = tr) # is the standardized training set
ts<-predict(normalizer, newdata = ts) # is the standardized test set

model.control<- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  returnData = FALSE
)

# Define a search grid of values to test
nnet.param <- expand.grid("decay" = seq(0.001, 0.01, 0.001), 
                        "size" = seq(3, 30, 1)) # "3" is theoretically sufficient to approximate any arbitrarily complex function

#Train neural network nn with 5-fold cv
nn_tuned_v.2 <- caret::train(return ~ user_dob + user_maturity + delivery_duration + user_state + item_price + item_color + item_size + month_of_delivery, 
                         data = tr,  
                         method = "nnet",
                         maxit = 200, # choice based on http://cowlet.org/2014/01/12/understanding-data-science-classification-with-neural-networks-in-r.html
                         MaxNWts = 10000, # manual setting as weights exceed standard threshold of 1,000
                         trace = TRUE, 
                         tuneGrid = nnet.param, 
                         metric = "AUC", trControl = model.control)

# Make probability prediction on test set with package caret automatically retraining the model on the whole training set
yhat[["nn_tuned_v.2"]] <- predict(nn_tuned_v.2, newdata = ts, type = "prob")[,2]
h <- HMeasure(true.class = as.numeric(ts$return)-1, scores = data.frame(yhat))
h$metrics["AUC"]

# Saving optimal nnet model
saveRDS(nn_tuned_v.2, file = "models/Nnet_Model_v.2.R")

# Predictions of tuned model for unknown data
retail_class<-read_and_preprocess_data_file("BADS_WS1718_class.csv")
retail_class<-predict(normalizer, newdata = retail_class)
pred_nnet_prob_v.2<-predict(nn_tuned_v.2, newdata = retail_class, type = "prob")

# Saving nnet probability predictions
write.csv(pred_nnet_prob_v.2, file = "data/Nnet_Predictions_Prob_v.2.csv")

# Making and saving nnet class predictions
pred_nnet_class_v.2<-predict(nn_tuned_v.2, newdata = retail_class, type = "class")
write.csv(pred_nnet_class_v.2, file = "data/Nnet_Predictions_Class_v.2.csv")