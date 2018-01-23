if(!require("e1071")) install.packages("e1071"); library("e1071")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("caret")) install.packages("caret"); library("caret")

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
  
  if ("return" %in% colnames (dd)) {
    dd = normalizeFeatures(dd, target="return")
    dd = createDummyFeatures(dd, target="return", cols=c("user_state", "user_title"))
  } else {
    dd = createDummyFeatures(dd, cols=c("user_state", "user_title"))
  }
  
  return(dd)
}

# good arguments agains pure svm approach: https://datascience.stackexchange.com/questions/989/svm-using-scikit-learn-runs-endlessly-and-never-completes-execution
#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
classdata = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')

### TODO AMEND BLOCK AFTER FEATURE ENGINEERING
dn = amend_features(d)
classdatan = amend_features(classdata)
###############################################
dn$return = factor(d$return)

set.seed(1)

idx.train = createDataPartition(y = dn$return, p = 0.8, list = FALSE) 
tr = dn[idx.train, ]
ts = dn[-idx.train, ]

trainTask  = makeClassifTask(data = tr, target = "return", positive = 1)
svmLearner = makeLearner(
  "classif.lssvm",
  predict.type = "response",
  par.vals = list(
    kernel = "rbfdot"
  )
)

svmParams = makeParamSet(
  makeDiscreteParam("sigma", values = 2^c(1))
)

control = makeTuneControlRandom(maxit = 1)
resample_desc = makeResampleDesc("CV", iters = 5)

tuned_params = tuneParams(
  learner = svmLearner,
  task = trainTask,
  resampling = resample_desc,
  par.set = svmParams,
  control = control
)

########################################

predictions = predict(radsvm, newdata = d)
d.result = data.frame(d$order_item_id, predictions)
names(d.result) = c("order_item_id", "return")


save(radsvm, file = "models/svm.model")
write.csv(d.result, "data/svm_predictions_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/xgboost_class.csv", row.names = FALSE)
