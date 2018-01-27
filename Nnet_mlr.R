# mlr with neuralnet, 5 fold CV

if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
if(!require("data.table")) install.packages("data.table"); library("data.table")

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

# TODO WHAT THE F IS HAPPENING HERE? Error in data.table(sales) : could not find function "data.table"
# setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
#d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
#classdata = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')

### TODO AMEND BLOCK AFTER FEATURE ENGINEERING
dn = amend_features(df_known)
classdatan = amend_features(df_class)
###############################################
set.seed(1)

idx.train = caret::createDataPartition(y = dn$return, p = 0.8, list = FALSE) 
tr = dn[idx.train, ]
ts = dn[-idx.train, ]

# Define tasks
trainTask <- makeClassifTask(data = tr, target = "return", positive = "1")
testTask <- makeClassifTask(data = ts, target = "return", positive = "1")
# Learner
makeannet <- makeLearner("classif.nnet", predict.type="prob")
# Set up a structure to save the expected results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()
# ACC performance for each model
acc <- list()

# Activate parallel computing
parallelStartSocket(3)

# Choose 5 fold CV with stratified sampling
set_cv <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

# Hyperparamter tuning
# TODO implement final parameter ranges
getParamSet("classif.nnet")
gs <- makeParamSet(
  makeIntegerParam("size", lower = 3, upper = 30),
  # makeNumericParam("MaxNWts", lower = 10000, upper = 10000),
  # makeNumericParam("maxit", lower = 200, upper = 200),
  makeDiscreteParam("MaxNWts", values = 10000),
  makeDiscreteParam("maxit", values = c(100, 200, 300)),
  makeNumericParam("decay", lower = 1e-08, upper = 0.01)
)
# Perform grid search
gscontrol <- makeTuneControlGrid(tune.threshold = TRUE)
tuning <- tuneParams(
  learner = makeannet, 
  resampling = set_cv, 
  task = trainTask, 
  par.set = gs, 
  control = gscontrol,
  measures = mlr::auc)

# Stop parallelization
parallelStop()

# View optimal hyperparameters
tuning$x

# Select best parameters and retrain model on whole training set
tuned.nnet<-setHyperPars(makeannet, par.vals = tuning$x)
modelLib[["nnet"]]<-mlr::train(tuned.nnet, trainTask)

# Save model
save(modelLib[["nnet"]], file = "models/nnet_mlr.model")

# Predict on test set and assess performance based on auc and acc
# TODO adjust command for optimal threshold
yhat[["nnet_prob_test"]]<-predict(modelLib[["nnet"]], testTask)
yhat[["nnet_class_test"]]<-ifelse(yhat[["nnet_prob_test"]]> tuning$x[["tune.threshold"]], 1, 0)
auc[["nnet_test"]]<-mlr::performance(yhat[["nnet_class_test"]], measures = mlr::auc)
acc[["nnet_test"]]<-mlr::performance(yhat[["nnet_class_test"]], measures = mlr::acc)

# Use tuned model to predict whole known dataset
yhat[["nnet_known"]]<-predict(modelLib[["nnet"]], newdata = dn)

# Use tuned model to predict whole class dataset
yhat[["nnet_class"]]<-predict(modelLib[["nnet"]], newdata = classdatan)

# Create an object containing order_item_id and predicted probabilities for known dataset
d.result<-data.frame(df_known$order_item_id, yhat[["nnet_known"]]$data$prob.1)
names(d.result)<-c("order_item_id", "return")

# Assess total accuracy on known dataset 
# TODO adjust command for optimal threshold
acc[["nnet_total"]]<-mean(ifelse(d.result$return > tuning$x[["tune.threshold"]], 1,0) == df_known$return)

# Create an object containing order_item_id and predicted probabilities for class dataset
classdata.result = data.frame(df_class$order_item_id, yhat[["nnet_class"]]$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

write.csv(d.result, "data/nnet_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/nnet_class.csv", row.names = FALSE)

