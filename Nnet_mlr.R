# mlr with neuralnet, 5 fold CV

if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("nnet")) install.packages("nnet"); library("nnet")

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

# Define tasks and learner
trainTask <- makeClassifTask(data = tr, target = "return", positive = "1")
testTask <- makeClassifTask(data = ts, target = "return", positive = "1")
# Set up a structure to save the expected results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
# auc <- list()
# Learner
makeannet <- makeLearner("classif.nnet", predict.type="prob")
# Choose 5 fold CV with stratified sampling
set_cv <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

# Hyperparamter tuning
getParamSet("classif.nnet")
gs <- makeParamSet(
  makeIntegerParam("size", lower = 3, upper = 5),
  makeNumericParam("MaxNWts", lower = 10000, upper = 10000),
  makeNumericParam("maxit", lower = 200, upper = 200),
  #makeDiscreteParam("MaxNWts", values = "10000"),
  #makeDiscreteParam("maxit", values = c("100", "200", "300")),
  makeNumericParam("decay", lower = 1e-08, upper = 0.01)
)
# Perform grid search
gscontrol <- makeTuneControlGrid()
tuning <- tuneParams(
  learner = makeannet, 
  resampling = set_cv, 
  task = trainTask, 
  par.set = gs, 
  control = gscontrol,
  measures = auc
)

# Select best parameters and retrain model on whole training set
tuned.nnet<-setHyperPars(makeannet, par.vals = tuning$x)
trained.nnet<-train(tuned.nnet, trainTask)
getLearnerModel(trained.nnet)

# Predict on test set
pred.nnet<-predict(trained.nnet, testTask)


