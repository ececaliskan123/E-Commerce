# mlr with neuralnet, 5 fold CV

if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
# if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("data.table")) install.packages("data.table"); library("data.table")

amend_features = function(dd){
  dd = subset(dd, select = -c(delivery_date))
  dd = subset(dd, select = -c(user_dob, user_maturity, user_title, user_state, item_color, item_price, item_size))
# Nicolai added the feature item_price and item_size to the previous line due to low RF variable importance scores
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
    # dd = createDummyFeatures(dd, target="return", cols=c("item_size"))
  } else {
    # dd = createDummyFeatures(dd, cols=c("item_size"))
  }
# Nicolai deselected createDummyFeatures due to the deletion of fetures item_size and item_price (see above)
  return(dd)
}

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

# Activate parallel computing with all cores
parallelStartSocket(parallel::detectCores())

# Choose 5 fold CV with stratified sampling
set_cv <- makeResampleDesc("CV", iters = 5L, stratify = TRUE)

# Hyperparamter tuning
# TODO implement final parameter ranges
getParamSet("classif.nnet")
rs <- makeParamSet(
  makeIntegerParam("size", lower = 3L, upper = 30L),
  makeDiscreteParam("MaxNWts", values = 10000),
  makeDiscreteParam("maxit", values = 50),
  makeNumericParam("decay", lower = 1e-06, upper = 1e-01)
)
# Perform grid search
rscontrol <- makeTuneControlRandom(maxit = 50L, tune.threshold = TRUE)
tuning <- tuneParams(
  learner = makeannet, 
  resampling = set_cv, 
  task = trainTask, 
  par.set = rs, 
  control = rscontrol,
  measures = mlr::auc)

# Stop parallelization
parallelStop()

# View optimal hyperparameters
tuning$x

# Select best parameters and retrain model on whole training set
tuned.nnet<-setHyperPars(makeannet, par.vals = tuning$x)
nnet_model<-mlr::train(tuned.nnet, trainTask)

# Save model
save(nnet_model, file = "models/nnet_mlr.model")
yhat[["nnet_prob_test"]]$data$truth

# Predict on test set and assess performance based on auc and acc
yhat[["nnet_prob_test"]]<-predict(nnet_model, testTask)
yhat[["nnet_class_test"]]<-yhat[["nnet_prob_test"]]$data$response
auc[["nnet_test"]]<-mlr::performance(yhat[["nnet_prob_test"]], measures = mlr::auc)
auc[["nnet_test"]]
acc[["nnet_test"]]<-mean(yhat[["nnet_class_test"]] == yhat[["nnet_prob_test"]]$data$truth)
acc[["nnet_test"]]

# Use tuned model to predict whole known dataset
yhat[["nnet_known"]]<-predict(nnet_model, newdata = dn)

# Use tuned model to predict whole class dataset
yhat[["nnet_class"]]<-predict(nnet_model, newdata = classdatan)

# Create an object containing order_item_id and predicted probabilities for known dataset
d.result<-data.frame(df_known$order_item_id, yhat[["nnet_known"]]$data$prob.1)
names(d.result)<-c("order_item_id", "return")

# Assess total accuracy on known dataset 
acc[["nnet_total"]]<-mean(ifelse(d.result$return > 0.5, 1, 0) == df_known$return)
acc[["nnet_total"]]

# Create an object containing order_item_id and predicted probabilities for class dataset
classdata.result = data.frame(df_class$order_item_id, yhat[["nnet_class"]]$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

write.csv(d.result, "data/nnet_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/nnet_class.csv", row.names = FALSE)

