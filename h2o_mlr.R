# h2o with neuralnet, 5 fold CV

if(!require("h2o")) install.packages("h2o"); library("h2o")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
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
makeh2o <- makeLearner("classif.h2o.deeplearning", predict.type="prob")
# Set up a structure to save the expected results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()
# ACC performance for each model
acc <- list()

# TODO Activate parallel computing with all cores
parallelStartSocket(parallel::detectCores()-1)

# Choose 5 fold CV with stratified sampling
set_cv <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

# Hyperparamter tuning
getParamSet("classif.h2o.deeplearning")
gs <- makeParamSet(
  makeDiscreteParam("activation", values = c("RectifierWithDropout", "TanhWithDropout")),
  # Three layers are theoretically enough to approximate any arbitrarily complex function
  makeIntegerVectorParam("hidden", len = 3, lower = c(32, 32, 32), upper = c(32, 32, 32)),
  # Default equals 10, outcome is sensitive to number of iterations (epochs) according to the h2o.ai handbook
  # makeDiscreteParam("epochs", values = c(10, 20, 30)),
  # Adaptive learning speeds up the learning process and minimizes error on both training and test dataset according to Srivastava, N. et al. (2014)
  makeNumericParam("rho", lower = 0.9, upper = 0.99),
  makeNumericParam("epsilon", lower = 1e-10, upper = 10^-04),
  # Dropout regulaization outperforms traditional regularization methods according to Srivastava, N. et al. (2014)
  # As a rule of thumb, choose either 0.1 or 0.2 according to the h2o handbook from h2o.ai
  makeNumericParam("input_dropout_ratio", lower = 0, upper = 0.2),
  # Handbook and Srivastava, N. et al. (2014) differ in terms of parameter choice. Hence, we tune over the union of both parameter spaces
  makeNumericVectorParam("hidden_dropout_ratios", len = 3, lower = c(0.1, 0.1, 0.1), upper = c(0.8, 0.8, 0.8)),
  # max-norm regularization increases the performance when dropout regularization is used. Typical values lie between 3 and 4 according to Srivastava, N. et al. (2014)
  makeNumericParam("max_w2", lower = 3, upper = 4)
)
?makeNumericParam
# Perform grid search
gscontrol <- makeTuneControlGrid(tune.threshold = TRUE)
tuning <- tuneParams(
  learner = makeh2o, 
  resampling = set_cv, 
  task = trainTask, 
  par.set = gs, 
  control = gscontrol,
  measures = mlr::auc
)

# Stop parallelization
parallelStop()

# View optimal hyperparameters
tuning$x

# Select best parameters and retrain model on whole training set
tuned.h2o<-setHyperPars(makeh2o, par.vals = tuning$x)
modelLib[["h2o"]]<-mlr::train(tuned.h2o, trainTask)

# Save model
save(modelLib[["h2o"]], file = "models/h2o_mlr.model")

# Predict on test set and assess performance based on auc and acc
# TODO adjust command for optimal threshold
yhat[["h2o_prob_test"]]<-predict(modelLib[["h2o"]], testTask)
yhat[["h2o_class_test"]]<-ifelse(yhat[["h2o_prob_test"]]> tuning$x[["tune.threshold"]], 1, 0)
auc[["h2o_test"]]<-mlr::performance(yhat[["h2o_class_test"]], measures = mlr::auc)
acc[["h2o_test"]]<-mlr::performance(yhat[["h2o_class_test"]], measures = mlr::acc)

# Use tuned model to predict whole known dataset
yhat[["h2o_known"]]<-predict(modelLib[["h2o"]], newdata = dn)

# Use tuned model to predict whole class dataset
yhat[["h2o_class"]]<-predict(modelLib[["h2o"]], newdata = classdatan)

# Create an object containing order_item_id and predicted probabilities for known dataset
d.result<-data.frame(df_known$order_item_id, yhat[["h2o_known"]]$data$prob.1)
names(d.result)<-c("order_item_id", "return")

# Assess total accuracy on known dataset 
# TODO adjust command for optimal threshold
acc[["h2o_total"]]<-mean(ifelse(d.result$return > tuning$x[["tune.threshold"]], 1,0) == df_known$return)

# Create an object containing order_item_id and predicted probabilities for class dataset
classdata.result = data.frame(df_class$order_item_id, yhat[["h2o_class"]]$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

write.csv(d.result, "data/h2o_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/h2o_class.csv", row.names = FALSE)
