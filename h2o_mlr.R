## mlr with h2o, 5 fold CV

if(!require("h2o")) install.packages("h2o"); library("h2o")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("pdp")) install.packages("pdp"); library("pdp")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
if(!require("data.table")) install.packages("data.table"); library("data.table")
# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
# load data
source('load_data.R')
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
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()
# ACC performance for each model
acc <- list()

# Choose 5 fold CV with stratified sampling (no significant performance loss compared with 10 fold CV or 632 bootstrapping)
set_cv <- makeResampleDesc("CV", iters = 5L, stratify = TRUE)

# Hyperparamter tuning
getParamSet("classif.h2o.deeplearning")
rs <- makeParamSet(
  makeDiscreteParam("activation", values = c("RectifierWithDropout", "TanhWithDropout")),
  # Three layers are theoretically enough to approximate any arbitrarily complex function
  makeIntegerVectorParam("hidden", len = 3L, lower = c(32, 32, 32), upper = c(64, 64, 64)), # TODO 64, 64, 64
  # Default equals 10, outcome is sensitive to number of iterations (epochs) according to the h2o.ai handbook
  makeDiscreteParam("epochs", values = 10L),
  # Adaptive learning speeds up the learning process and minimizes error on both training and test dataset according to Srivastava, N. et al. (2014)
  makeNumericParam("rho", lower = 0.9, upper = 0.99), # TODO 0.9 to 0.99
  makeNumericParam("epsilon", lower = 1e-10, upper = 1e-4), # TODO 1e-10 to 1e-4
  # Dropout regulaization outperforms traditional regularization methods according to Srivastava, N. et al. (2014)
  # As a rule of thumb, choose either 0.1 or 0.2 according to the h2o handbook from h2o.ai
  makeNumericParam("input_dropout_ratio", lower = 0, upper = 0.2), # TODO 0 to 0.2
  # Handbook and Srivastava, N. et al. (2014) differ in terms of parameter choice. Hence, we tune over the union of both parameter spaces 
  makeNumericVectorParam("hidden_dropout_ratios", len = 3L, lower = c(0, 0, 0), upper = c(0.8, 0.8, 0.8)), # TODO 0 to 0.8
  # max-norm regularization increases the performance when dropout regularization is used. Typical values lie between 3 and 4 according to Srivastava, N. et al. (2014)
  makeNumericParam("max_w2", lower = 3L, upper = 4L)
)

# Perform grid search
rancontrol <- makeTuneControlRandom(maxit = 50L, tune.threshold = TRUE)
tuning <- tuneParams(
  learner = makeh2o, 
  resampling = set_cv, 
  task = trainTask, 
  par.set = rs, 
  control = rancontrol,
  measures = mlr::auc
)

# View optimal hyperparameters
tuning$x

# Select best parameters and retrain model on whole training set
tuned.h2o<-setHyperPars(makeh2o, par.vals = tuning$x)
h2o_model<-mlr::train(tuned.h2o, trainTask)

# Save model
save(h2o_model, file = "models/h2o_mlr.model")

# Predict on test set and assess performance based on auc and acc
yhat[["h2o_prob_test"]]<-predict(h2o_model, testTask)
yhat[["h2o_class_test"]]<-yhat[["h2o_prob_test"]]$data$response
auc[["h2o_test"]]<-mlr::performance(yhat[["h2o_prob_test"]], measures = mlr::auc)
auc[["h2o_test"]]
acc[["h2o_test"]]<-mean(yhat[["h2o_class_test"]] == yhat[["h2o_prob_test"]]$data$truth)
acc[["h2o_test"]]

# Use tuned model to predict whole known dataset
yhat[["h2o_known"]]<-predict(h2o_model, newdata = dn)

# Use tuned model to predict whole class dataset
yhat[["h2o_class"]]<-predict(h2o_model, newdata = classdatan)

# Create an object containing order_item_id and predicted probabilities for known dataset
d.result<-data.frame(df_known$order_item_id, yhat[["h2o_known"]]$data$prob.1)
names(d.result)<-c("order_item_id", "return")

# Assess total accuracy on known dataset 
acc[["h2o_total"]]<-mean(ifelse(d.result$return > yhat[["h2o_known"]]$threshold[1], 1,0) == df_known$return)
acc[["h2o_total"]]

# Create an object containing order_item_id and predicted probabilities for class dataset
classdata.result = data.frame(df_class$order_item_id, yhat[["h2o_class"]]$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

# TODO put in known class hack
d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

write.csv(d.result, "data/h2o_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/h2o_class.csv", row.names = FALSE)

## Variable importance
h2o_imp_df <- h2o.varimp(h2o_model$learner.model)
h2o_imp_df
save(h2o_imp_df, file = "data/h2o_imp_df")

h2o_imp_plot <- h2o.varimp_plot(h2o_model$learner.model, num_of_features = 17)
h2o_imp_plot
save(h2o_imp_plot, file = "data/h2o_imp_plot")

feature_names <- h2o_model$features
pd = generatePartialDependenceData(h2o_model, trainTask)
class(h2o_model)
class(h2o_model$learner.model)

localH2O = h2o.init(ip='localhost',
                    nthreads=-1,
                    min_mem_size='4G',
                    max_mem_size='5G') # with system memory of 8GB

h2o_pdp <- generatePartialDependenceData(h2o_model, trainTask, "user_id")
###