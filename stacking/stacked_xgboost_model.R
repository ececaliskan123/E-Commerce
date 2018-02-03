if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")

# IMPORTANT! working directory needs to be project root!

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
#trainTask = makeClassifTask(data = tr, target = "return", positive = 1)
trainTask = makeCostSensTask(data = tr,
                             cost = cost[idx.train,])
resample_desc = makeResampleDesc("CV", iters = 5)
tune_control = makeTuneControlRandom(maxit = 40)

xgb_params = makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 300, upper = 600),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 6, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .01, upper = .3),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x),
  makeNumericParam("gamma",  lower = 0,  upper = 0.3),
  makeNumericParam("subsample", lower = 0.6, upper = 0.9)
)

stack_learner = makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 200,
    base_score = mean(d$return == 1)
  )
)
stack_learner = makeCostSensClassifWrapper(stack_learner)

tuned_params = tuneParams(
  learner = stack_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = tune_control
)

stack_tuned_learner = setHyperPars(
  learner = stack_learner,
  par.vals = tuned_params$x
)

stack_model = mlr::train(stack_tuned_learner, trainTask)
# create final predictions
predicted_classes = predict(stack_model, newdata = df_known)
predicted_class   = predict(stack_model, newdata = df_class)

# assess performance
d.result = data.frame(d$order_item_id, ifelse(predicted_classes$data$response == "return0",0,1))
names(d.result) = c("order_item_id", "return")
ts_accuracy     = mean(d.result[-idx.train,"return"] == d[-idx.train,"return"])
tr_accuracy     = mean(d.result[idx.train, "return"] == d[idx.train,"return"])

classdata.result = data.frame(df_class$order_item_id, ifelse(predicted_class$data$response == "return0",0,1))
names(classdata.result) = c("order_item_id", "return")

# put in NA column hack
d.result[is.na(d$delivery_date), "return"] = 0
classdata.result[is.na(c$delivery_date), "return"] = 0

# and write out final predictions
write.csv(d.result, "data/stacked_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/stacked_class.csv", row.names = FALSE)

if (FALSE) {
  d.result$truth = d$return
  d.result$cost  = NA
  
  d.result[(d.result$truth == 1) == (d.result$return == 0),"cost"] = (d[(d.result$truth == 1) == (d.result$return == 0),"item_price"]*0.1+3)*2.5
  d.result[(d.result$truth == 0) == (d.result$return == 1),"cost"] =  d[(d.result$truth == 0) == (d.result$return == 1),"item_price"]*0.5
  d.result[is.na(d.result$cost),"cost"] = 0
  sum(d.result$cost)
}