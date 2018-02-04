if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")

#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
#source('load_data.R')

#svm_known = read.csv("data/.csv", stringsAsFactors = FALSE)
#svm_class = read.csv("data/.csv", stringsAsFactors = FALSE)
xgboost_known = read.csv("data/xgboost_known.csv", stringsAsFactors = FALSE)
xgboost_class = read.csv("data/xgboost_class.csv", stringsAsFactors = FALSE)
rf_known = read.csv("data/rf_known.csv", stringsAsFactors = FALSE)
rf_class = read.csv("data/rf_class.csv", stringsAsFactors = FALSE)
nnet_known = read.csv("data/nnet_known.csv", stringsAsFactors = FALSE)
nnet_class = read.csv("data/nnet_class.csv", stringsAsFactors = FALSE)
h2o_known = read.csv("data/h2o_known.csv", stringsAsFactors = FALSE)
h2o_class = read.csv("data/h2o_class.csv", stringsAsFactors = FALSE)
d = read.csv('data/BADS_WS1718_known.csv')
c = read.csv('data/BADS_WS1718_class.csv')

# sort everything first
xgboost_known = xgboost_known[order(xgboost_known$order_item_id),]
xgboost_class = xgboost_class[order(xgboost_class$order_item_id),]
rf_known = rf_known[order(rf_known$order_item_id),]
rf_class = rf_class[order(rf_class$order_item_id),]
nnet_known = nnet_known[order(nnet_known$order_item_id),]
nnet_class = nnet_class[order(nnet_class$order_item_id),]
h2o_known = h2o_known[order(h2o_known$order_item_id),]
h2o_class = h2o_class[order(h2o_class$order_item_id),]

# assert if we are missing labels
known_labels_complete = 
  xgboost_known$order_item_id == rf_known$order_item_id &&
  rf_known$order_item_id == nnet_known$order_item_id &&
  nnet_known$order_item_id == h2o_known$order_item_id

class_labels_complete = 
  xgboost_class$order_item_id == rf_class$order_item_id &&
  rf_class$order_item_id == nnet_class$order_item_id &&
  nnet_class$order_item_id == h2o_class$order_item_id

stopifnot(known_labels_complete && class_labels_complete)

# create learning and prediction dataframes
df_known = data.frame(xgboost_known$order_item_id,
                      xgboost_known$return,
                      rf_known$return,
                      nnet_known$return,
                      h2o_known$return,
                      d$return)
df_class = data.frame(c$order_item_id,
                      xgboost_class$return,
                      rf_class$return,
                      nnet_class$return,
                      h2o_class$return)

colnames(df_known) = c("order_item_id",
                       "xgboost_return",
                       "rf_return",
                       "nnet_return",
                       "h2o_return",
                       "return")
colnames(df_class) = c("order_item_id",
                       "xgboost_return",
                       "rf_return",
                       "nnet_return",
                       "h2o_return")
levels(df_known$return) = c("return0", "return1")

# create cost matrix
cost = data.frame(ifelse(df_known$return == 0, 0, 2.5 * (3+0.1*d$item_price)),
                  ifelse(df_known$return == 1, 0, 0.5 * d$item_price))
colnames(cost) = levels(df_known$return)
rownames(cost) = rownames(d)

# create test and training sets
set.seed(1)
idx.train = caret::createDataPartition(y = df_known$return, p = 0.8, list = FALSE) 
df_known$return = NULL
tr = df_known[idx.train, ]
ts = df_known[-idx.train, ]

# train the regression
trainTask = makeCostSensTask(data = tr, cost = cost[idx.train,])
#trainTask = makeClassifTask(data = tr, target = "return", positive = 1)
resample_desc = makeResampleDesc("CV", iters = 5)
param_set = makeParamSet(
  makeIntegerParam("mtry",          lower = 2,   upper = 4),
  makeIntegerParam("num.trees",     lower = 200, upper = 450),
  makeIntegerParam("min.node.size", lower = 1,   upper = 200)
)
rscontrol = makeTuneControlRandom(maxit = 50L)

stack_learner = makeLearner(
  "classif.ranger",
  predict.type = "response",
  par.vals = list(
    num.threads = 4
  )
)
stack_learner = makeCostSensClassifWrapper(stack_learner)

tuning <- tuneParams(
  learner = stack_learner, 
  resampling = resample_desc, 
  task = trainTask, 
  par.set = param_set,
  control = rscontrol)
stack_learner.tuned = setHyperPars(stack_learner, par.vals = tuning$x)

stack_model = mlr::train(stack_learner.tuned, trainTask)

# create final predictions
predicted_classes = predict(stack_model, newdata = df_known)
predicted_class   = predict(stack_model, newdata = df_class)

# assess performance
d.result = data.frame(d$order_item_id, ifelse(predicted_classes$data$response == "return0",0,1))
#d.result = data.frame(d$order_item_id, predicted_classes$data$response)
names(d.result) = c("order_item_id", "return")
ts_accuracy     = mean(d.result[-idx.train,"return"] == d[-idx.train,"return"])
tr_accuracy     = mean(d.result[idx.train, "return"] == d[idx.train,"return"])

classdata.result = data.frame(df_class$order_item_id, ifelse(predicted_class$data$response == "return0",0,1))
names(classdata.result) = c("order_item_id", "return")

# put in NA column hack
d.result[is.na(d$delivery_date), "return"] = 0
classdata.result[is.na(c$delivery_date), "return"] = 0

# and write out final predictions
write.csv(d.result, "data/stacked_known_ranger.csv", row.names = FALSE)
write.csv(classdata.result, "data/stacked_class_ranger.csv", row.names = FALSE)

# $mtry [1] 3
# $num.trees [1] 397
# $min.node.size [1] 184

if (FALSE) {
  d.result$truth = d$return
  d.result$cost  = NA
  
  d.result[(d.result$truth == 1) == (d.result$return == 0),"cost"] = (d[(d.result$truth == 1) == (d.result$return == 0),"item_price"]*0.1+3)*2.5
  d.result[(d.result$truth == 0) == (d.result$return == 1),"cost"] =  d[(d.result$truth == 0) == (d.result$return == 1),"item_price"]*0.5
  d.result[is.na(d.result$cost),"cost"] = 0
  sum(d.result$cost)
}