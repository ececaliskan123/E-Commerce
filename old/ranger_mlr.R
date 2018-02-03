### Parameter tuning for ranger (random forest) with mlr 

if(!require("ranger")) install.packages("ranger"); library("ranger")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")

# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
# load data
source('load_data.R')
dn = amend_features(df_known)
dn[is.na(dn)] = 0
classdatan = amend_features(df_class)

###############################################
set.seed(1)

idx.train = caret::createDataPartition(y = dn$return, p = 0.8, list = FALSE) 
tr = dn[idx.train, ]
ts = dn[-idx.train, ]

# Define task and learner
trainTask <- makeClassifTask(data = tr, target = "return", positive = "1")
testTask <- makeClassifTask(data = ts, target = "return", positive = "1")
# Set up a structure to save the expected results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()

rf <- makeLearner("classif.ranger", predict.type="prob")
rf
## Tuning
# Hyperparameter setting
# Set the scale/range for your parameters
rf.parms <- makeParamSet(
  # The recommendation for mtry by Breiman is squareroot number of columns
  makeIntegerParam("mtry", lower = 2, upper = 6), # Number of features selected at each node, smaller -> faster
  makeIntegerParam("num.trees", lower = 200, upper = 1000) # Number of tree, smaller -> faster
) 
# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlRandom(maxit = 50L, tune.threshold = TRUE)
# Sampling strategy
# Given our small dataset, we do 5-fold cross-validation
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
# Activate parallel computing with all cores
parallelStartSocket(parallel::detectCores())
# Start tuning with the defined options
timing <- list()
timing[["simple"]] <- system.time(
  tuning <- tuneParams(learner = rf, task = trainTask, resampling = rdesc,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
)
# Stop parallelization
parallelStop()
# Extract optimal parameter values after tuning 
tuning$x
# Investigate the results in detail
tuning_results <- generateHyperParsEffectData(tuning, partial.dep = TRUE)
# Get the performance for each parameter combination
tuning_results$data
tapply(tuning_results$data$auc.test.mean, INDEX = c(tuning_results$data$mtry), mean)
# Update the learner to the optimal hyperparameters
rf
rf_tuned <- setHyperPars(rf, par.vals = tuning$x)
rf_tuned #Hyperparameters: num.threads=1,verbose=FALSE,respect.unordered.factors=TRUE,mtry=6,num.trees=200
# Train the model on the full training data (not only a CV-fold)
ranger_model <- mlr::train(rf_tuned, task = trainTask)
# Save model
save(ranger_model, file = "old/ranger_mlr.model")

# Predict on test set and assess performance based on auc and acc
yhat[["ranger_prob_test"]]<-predict(ranger_model, testTask)
yhat[["ranger_class_test"]]<-yhat[["ranger_prob_test"]]$data$response
auc[["ranger_test"]]<-mlr::performance(yhat[["ranger_prob_test"]], measures = mlr::auc)
auc[["ranger_test"]]
acc[["ranger_test"]]<-mean(yhat[["ranger_class_test"]] == yhat[["ranger_prob_test"]]$data$truth)
acc[["ranger_test"]]

# Use tuned model to predict whole known dataset
yhat[["ranger_known"]]<-predict(ranger_model, newdata = dn)

# Use tuned model to predict whole class dataset
yhat[["ranger_class"]]<-predict(ranger_model, newdata = classdatan)

# Create an object containing order_item_id and predicted probabilities for known dataset
d.result<-data.frame(df_known$order_item_id, yhat[["ranger_known"]]$data$prob.1)
names(d.result)<-c("order_item_id", "return")

# Assess total accuracy on known dataset 
acc[["ranger_total"]]<-mean(ifelse(d.result$return > yhat[["ranger_known"]]$threshold[1], 1, 0) == df_known$return)
acc[["ranger_total"]]

# Create an object containing order_item_id and predicted probabilities for class dataset
classdata.result = data.frame(df_class$order_item_id, yhat[["ranger_class"]]$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

write.csv(d.result, "old/ranger_known.csv", row.names = FALSE)
write.csv(classdata.result, "old/ranger_class.csv", row.names = FALSE)