if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("h2o")) install.packages("h2o"); library("h2o")


amend_features = function(dd){
  dd = subset(dd, select = -c(user_dob,
                              user_maturity,
                              user_title,
                              user_state,
                              item_color,
                              delivery_date,
                              item_size))
  
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
  }
  
  return(dd)
}

source('load_data.R')

dn         = amend_features(df_known)
classdatan = amend_features(df_class)

set.seed(1)
idx.train = createDataPartition(y =dn$return, p = 0.8, list = FALSE) 
tr = dn[idx.train, ]  # training set
ts = dn[-idx.train, ] # test set 
##
localH2O = h2o.init(ip='localhost',
                    nthreads=-1,
                    min_mem_size='4G',
                    max_mem_size='5G') # with system memory of 8GB
##
rf_params = makeParamSet(
  makeIntegerParam("ntrees", lower = 50, upper = 500),
  makeIntegerParam("mtries", lower=7, upper=9),
  makeIntegerParam("min_rows", lower=2, upper=20)
)

train_task  = makeClassifTask(data = tr, target = "return", positive = 1)
rf_control = makeTuneControlRandom(maxit = 5)
rs_desc    = makeResampleDesc("CV", iters = 5)

rf_learner = makeLearner(
  "classif.h2o.randomForest",
  predict.type = "prob",
  par.vals = list(
  )
)

rf_learner=makeTuneWrapper(learner    = rf_learner,
                           resampling = rs_desc,
                           par.set    = rf_params,
                           control    = rf_control)
#rf_tuned_learner = setHyperPars(
#  learner = rf_learner,
#  par.vals = tuned_params$x
#)

rf_model = mlr::train(rf_learner, train_task)
predicted_classes = predict(rf_model, newdata = dn)
predicted_class   = predict(rf_model, newdata = classdatan)

d.result = data.frame(df_known$order_item_id, predicted_classes$data$prob.1)
names(d.result) = c("order_item_id", "return")
ts_accuracy     = mean(ifelse(d.result[-idx.train,"return"] > 0.5, 1,0) == ts$return)
tr_accuracy     = mean(ifelse(d.result[idx.train, "return"] > 0.5, 1,0) == df_known[idx.train,"return"])

classdata.result = data.frame(df_class$order_item_id, predicted_class$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

save(rf_model, file = "models/rf_mlr.model")
write.csv(d.result, "data/rf_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/rf_class.csv", row.names = FALSE)

if (FALSE) {
  importance = h2o.varimp(rf_mode$learner.model)
  pd.xgboost = generatePartialDependenceData(xgb_model, trainTask, "item_price")
  plotPartialDependence(pd.xgboost)
}