### mlr with neuralnet, 5 fold CV

# load required packages
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("NeuralNetTools")) install.packages("NeuralNetTools"); library("NeuralNetTools")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("pdp")) install.packages("pdp"); library("pdp")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
# load data
source('load_data.R')

dn = amend_features(df_known)
# Replace 'NA' with 0. This is not going to affect predictive performance due to the fact that we will later assign all those customer that show 'NA' for delivery date a "no return", since we discovered fro the known dataset that if delivery_date = NA, then return = 0.
dn[is.na(dn)] = 0
is.na(dn)
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
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()
# ACC performance for each model
acc <- list()

# Activate parallel computing with all cores
parallelStartSocket(parallel::detectCores())

# Choose 5 fold CV with stratified sampling
set_cv <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

# Hyperparamter tuning
getParamSet("classif.nnet")
rs <- makeParamSet(
  makeIntegerParam("size", lower = 3L, upper = 30L),
  makeDiscreteParam("MaxNWts", values = 10000),
  makeDiscreteParam("maxit", values = 200),
  makeNumericParam("decay", lower = 1e-08, upper = 1e-02)
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
acc[["nnet_total"]]<-mean(ifelse(d.result$return > yhat[["nnet_known"]]$threshold[1], 1, 0) == df_known$return)
acc[["nnet_total"]]

# Create an object containing order_item_id and predicted probabilities for class dataset
classdata.result = data.frame(df_class$order_item_id, yhat[["nnet_class"]]$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

write.csv(d.result, "data/nnet_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/nnet_class.csv", row.names = FALSE)

## Variable importance using the olden method
nnet_weights  <- neuralweights(nnet_model$learner.model)$wts
feature_names <- nnet_model$features
target_name   <- "return"
nnet_imp_df <- olden(mod_in = nnet_weights,
              y_names = target_name,
              x_names = feature_names,
              bar_plot = FALSE)
save(nnet_imp_df, file = "data/nnet_imp_df")

nnet_imp_plot <- olden(mod_in = nnet_weights,
                      y_names = target_name,
                      x_names = feature_names,
                      bar_plot = TRUE)
save(nnet_imp_plot, file = "data/nnet_imp_plot")

## Calculate and plot PDPs for all variables
# PDP_avg_return<-generatePartialDependenceData(nnet_model, trainTask, "avg_return")
# plot(x = PDP_avg_return$data$avg_return, y = PDP_avg_return$data$Probability)

partialPlots <- list()
# For each of the variables, calculate the partial dependence object for further use
for (var in feature_names) {
  message("Now calculating for variable ", var)
  partialPlots[[var]][["nnet"]] <- do.call(partial, 
                                                list(nnet_model$learner.model, 
                                                train = trainTask$env$data, 
                                                pred.var = var, 
                                                which.class = 1, 
                                                type = "classification", 
                                                plot = FALSE, prob = TRUE))
  }

save(partialPlots, file = "data/h2o_PDPs_df")
par(mfrow=c(17, 1))
for(var in names(partialPlots)){
  plot(partialPlots[[var]][["nnet"]], type = "l", xlab = paste(var, " - nnet"), ylab = 'Pred. prob. return', ylim = c(0.1, 0.1), xlim = c(0.1, 0.1))
}
###