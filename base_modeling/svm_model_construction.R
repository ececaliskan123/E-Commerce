if(!require("e1071")) install.packages("e1071"); library("e1071")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("caret")) install.packages("caret"); library("caret")

# good arguments agains pure svm approach: https://datascience.stackexchange.com/questions/989/svm-using-scikit-learn-runs-endlessly-and-never-completes-execution
source('load_data.R')
source('helpers/amend_features.R')
dn = amend_features(df_known)
classdatan = amend_features(df_class)
###############################################
dn[is.na(dn)] = 0
classdatan[is.na(classdatan)] = 0
###############################################
set.seed(1)
idx.train = createDataPartition(y = dn$return, p = 0.8, list = FALSE) 
tr = dn[idx.train, ]
ts = dn[-idx.train, ]

trainTask  = makeClassifTask(data = tr, target = "return", positive = 1)
svmLearner = makeLearner(
  "classif.svm",
  predict.type = "prob",
  par.vals = list(
    kernel = "linear"
  )
)

svmParams = makeParamSet(
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x)
  #makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 2^x)
)

control = makeTuneControlRandom(maxit = 40)
resample_desc = makeResampleDesc("CV", iters = 5)

tuned_params = tuneParams(
  learner = svmLearner,
  task = trainTask,
  resampling = resample_desc,
  par.set = svmParams,
  control = control
)

# Create a new model using tuned hyperparameters
svm_tuned_learner = setHyperPars(
  learner = svmLearner,
  par.vals = tuned_params$x
)

# Re-train parameters using tuned hyperparameters (and full training set)
svm_model = mlr::train(svm_tuned_learner, trainTask)
predicted_classes = predict(svm_model, newdata = dn)
predicted_class   = predict(svm_model, newdata = classdatan)

d.result        = data.frame(df_known$order_item_id, predicted_classes$data$prob.1)
names(d.result) = c("order_item_id", "return")
ts_accuracy     = mean(ifelse(d.result[-idx.train,"return"] > 0.5, 1,0) == ts$return)
tr_accuracy     = mean(ifelse(d.result[idx.train, "return"] > 0.5, 1,0) == tr$return)

classdata.result = data.frame(df_class$order_item_id, predicted_class$data$prob.1)
names(classdata.result) = c("order_item_id", "return")

d.result[is.na(df_known$delivery_date), "return"] = 0
classdata.result[is.na(df_class$delivery_date), "return"] = 0

save(svm_model, file = "models/svm_mlr.model")
write.csv(d.result, "data/svm_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/svm_class.csv", row.names = FALSE)

#importance = svm.importance(feature_names = colnames(tr), model = svm_model$learner.model)
#s_cols = importance[,c("Gain","Cover","Frequency")]
#importance = data.frame(importance[,"Feature"], s_cols)
#write.csv(importance, "data/svm_importance.csv", row.names = F)
