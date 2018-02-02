### Parameter tuning for ranger(randomforest) with mlr 
library(mlr)
library(tidyr)
library(ranger)

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
task <- makeClassifTask(data = tr, target = "return", positive = "1")
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
  makeIntegerParam("num.trees", 200, 800) # Number of tree, smaller -> faster
) 
# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = TRUE)
# Sampling strategy
# Given our small dataset, we do 5-fold cross-validation
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
# Start tuning with the defined options
timing <- list()
timing[["simple"]] <- system.time(
  tuning <- tuneParams(rf, task = task, resampling = rdesc,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
)
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
modelLib[["rf"]] <- mlr::train(rf_tuned, task = task)
print(modelLib[["rf"]])
# Make prediction on test data
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = ts1)
str(yhat[["rf"]])
# Calculate AUC performance on test set 
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)
auc[["rf"]] #0.91037 