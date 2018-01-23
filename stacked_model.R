if(!require("mlr")) install.packages("mlr"); library("mlr")
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
c = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')

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
df_class = data.frame(c$order_item_id,
                      xgboost_class$return,
                      rf_class$pred,
                      nnet_class$return)

colnames(df_known) = c("order_item_id",
                       "xgboost_return",
                       "rf_return",
                       "nnet_return",
                       "return")
colnames(df_class) = c("order_item_id",
                       "xgboost_return",
                       "rf_return",
                       "nnet_return")
levels(df_known$return) = c("return0", "return1")

# create cost matrix
cost = data.frame(ifelse(df_known$return == 0, 0, 0.5 * d$item_price),
                  ifelse(df_known$return == 1, 0, 2.5*(3+0.1*d$item_price)))
colnames(cost) = levels(df_known$return)
rownames(cost) = rownames(d)

# check correlations of models
#results <- resamples(list(mod1 = mod.rf, mod2 = mod.svm)) 
#modelCor(results) 

# create test and training sets
set.seed(1)

idx.train = caret::createDataPartition(y = df_known$return, p = 0.8, list = FALSE) 
df_known$return = NULL
tr = df_known[idx.train, ]
ts = df_known[-idx.train, ]

# train the regression
trainTask = makeCostSensTask(data = tr,
                             cost = cost[idx.train,])
resample_desc = makeResampleDesc("CV", iters = 50)

stack_learner = makeLearner(
  "classif.logreg",
  predict.type = "response"
)
stack_learner = makeCostSensClassifWrapper(stack_learner)
stack_train = resample(
                      learner    = stack_learner,
                      task       = trainTask,
                      resampling = resample_desc
                      )
stack_model = mlr::train(stack_learner, trainTask)

# create final predictions
predicted_classes = predict(stack_model, newdata = df_known)
predicted_class   = predict(stack_model, newdata = df_class)

# assess performance
d.result = data.frame(d$order_item_id, ifelse(predicted_classes$data$response == "return0",0,1))
names(d.result) = c("order_item_id", "return")
accuracy = mean(d.result[-idx.train,]$return == d[-idx.train,]$return)
total_accuracy = mean(d$return == d.result$return)

classdata.result = data.frame(df_class$order_item_id, ifelse(predicted_class$data$response == "return0",0,1))
names(classdata.result) = c("order_item_id", "return")

# put in NA column hack
d.result[is.na(d$delivery_date), "return"] = 0
classdata.result[is.na(c$delivery_date), "return"] = 0

# and write out final predictions
write.csv(d.result, "data/stacked_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/stacked_class.csv", row.names = FALSE)