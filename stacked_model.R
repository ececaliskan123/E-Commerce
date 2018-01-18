if(!require("caret")) install.packages("caret"); library("caret")
#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')

#svm_known = read.csv("data/.csv", stringsAsFactors = FALSE)
#svm_class = read.csv("data/.csv", stringsAsFactors = FALSE)
xgboost_known = read.csv("data/xgboost_known.csv", stringsAsFactors = FALSE)
xgboost_class = read.csv("data/xgboost_class.csv", stringsAsFactors = FALSE)
rf_known = read.csv("data/randomforest_probability_retail.csv", stringsAsFactors = FALSE)
rf_class = read.csv("data/randomforest_probability.csv", stringsAsFactors = FALSE)
nnet_known = read.csv("data/nnet_known.csv", stringsAsFactors = FALSE)
nnet_class = read.csv("data/nnet_class.csv", stringsAsFactors = FALSE)
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')

# sort everything first
xgboost_known = xgboost_known[order(xgboost_known$order_item_id),]
xgboost_class = xgboost_class[order(xgboost_class$order_item_id),]
rf_known = rf_known[order(rf_known$order_item_id),]
rf_class = rf_class[order(rf_class$order_item_id),]
nnet_known = nnet_known[order(nnet_known$order_item_id),]
nnet_class = nnet_class[order(nnet_class$order_item_id),]

# assert if we are missing labels
known_labels_complete = 
  xgboost_known$order_item_id == rf_known$order_item_id &&
  rf_known$order_item_id == nnet_known$order_item_id

class_labels_complete = 
  xgboost_class$order_item_id == rf_class$order_item_id &&
  rf_class$order_item_id == nnet_class$order_item_id

stopifnot(known_labels_complete && class_labels_complete)

# create learning and prediction dataframes
df_known = data.frame(xgboost_known$order_item_id,
                      xgboost_known$return,
                      rf_known$pred,
                      nnet_known$return,
                      d$return)
df_class = data.frame(xgboost_known$order_item_id,
                      xgboost_known$return,
                      rf_known$pred,
                      nnet_known$return)

colnames(df_known) = c("order_item_id",
                       "xgboost_return",
                       "rf_return",
                       "nnet_return",
                       "return")
colnames(df_class) = c("order_item_id",
                       "xgboost_return",
                       "rf_return",
                       "nnet_return")

# create cost matrix
cost = data.frame(ifelse(d$return == 0, 0, 2.5*(3+0.1*d$item_price)),
                  ifelse(d$return == 1, 0, 0.5 * d$item_price))
colnames(cost) = make.names(c("X0","X1"))

# check correlations of models
#results <- resamples(list(mod1 = mod.rf, mod2 = mod.svm)) 
#modelCor(results) 

# create test and training sets
set.seed(1)
#df_known$return[df_known$return == 0] = "X0"
#df_known$return[df_known$return == 1] = "X1"
df_known$return = factor(df_known$return)
idx.train = caret::createDataPartition(y = df_known$return, p = 0.75, list = FALSE) 
tr = df_known[idx.train, ]
ts = df_known[-idx.train, ]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

stack_model <- caret::train(return ~ xgboost_return + rf_return + nnet_return,
                        data = tr, 
                        method = "glm", 
                        trControl = fitControl)
# create final predictions
predicted_classes = predict(stack_model, newdata = df_known)
#predicted_class   = predict(stack_model, newdata = df_class)

# assess performance
d.result = data.frame(d$order_item_id, predicted_classes)
names(d.result) = c("order_item_id", "return")
accuracy = mean(d.result[-idx.train,]$return == ts$return)
total_accuracy = mean(d$return == d.result$return)

#classdata.result = data.frame(classdata$order_item_id, predicted_class$data$response)
#names(classdata.result) = c("order_item_id", "return")

# put in NA column hack
d.result[is.na(d$delivery_date), "return"] = 0
#classdata.result[is.na(classdata$delivery_date), "return"] = 0

# and write out final predictions
