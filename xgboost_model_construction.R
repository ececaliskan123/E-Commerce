if(!require("prettyR")) install.packages("prettyR"); library("prettyR")
if(!require("sortinghat")) install.packages("sortinghat"); library("sortinghat")
if(!require("Matrix")) install.packages("Matrix"); library("Matrix")
if(!require("xgboost")) install.packages("xgboost"); library("xgboost")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")

amend_features = function(dd){
  dd = subset(dd, select = -c(delivery_date))
  dd = subset(dd, select = -c(order_item_id, item_color, item_size))
  
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
    dd = createDummyFeatures(dd, target="return", cols=c("user_state", "user_title"))
  }

  return(dd)
}

#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
classdata = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')

### TODO AMEND BLOCK AFTER FEATURE ENGINEERING
dn = amend_features(d)
classdatan = amend_features(classdata)
###############################################
set.seed(1)

idx.train = caret::createDataPartition(y = dn$return, p = 0.75, list = FALSE) 
tr = dn[idx.train, ]
ts = dn[-idx.train, ]

# e columns correspond to classes and their 
# names are the class labels (if unnamed we
# use y1 to yk as labels). Each entry (i,j) of the matrix
# specifies the cost of predicting class j for observation i.
# TODO set d$return labels and colnames accordingly

xgb_params = makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 400),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 6, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .01, upper = .3),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x),
  makeNumericParam("gamma",  lower = 0,  upper = 0.3),
  makeNumericParam("subsample", lower = 0.6, upper = 0.9)
)

trainTask = makeClassifTask(data = tr, target = "return", positive = 1)

# Create an xgboost learner that is classification based and outputs
# labels (as opposed to probabilities)
xgb_learner = makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 200,
    base_score = mean(tr$return)
  )
)

control = makeTuneControlRandom(maxit = 50)
resample_desc = makeResampleDesc("CV", iters = 5)

tuned_params = tuneParams(
  learner = xgb_learner,
  task = trainTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)

# Create a new model using tuned hyperparameters
xgb_tuned_learner = setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)

# Re-train parameters using tuned hyperparameters (and full training set)
xgb_model = mlr::train(xgb_tuned_learner, trainTask)
predicted_classes = predict(xgb_model, newdata = dn)
predicted_class   = predict(xgb_model, newdata = classdata)

d.result = data.frame(d$order_item_id, predicted_classes$data$prob.1)
names(d.result) = c("order_item_id", "return")
accuracy = mean(predicted_classes$data[-idx.train,]$response == ts$return)
total_accuracy = mean(ifelse(d.result$return > 0.5, 1,0) == d$return)

classdata.result = data.frame(classdata$order_item_id, predicted_class$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(d$delivery_date), "return"] = 0
classdata.result[is.na(classdata$delivery_date), "return"] = 0

save(xgb_model, file = "models/xgboost.model")
write.csv(d.result, "data/xgboost_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/xgboost_class.csv", row.names = FALSE)
# 0.72059 accuracy before eces changes
