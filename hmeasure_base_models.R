### base modeling performance measures

if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("mlr")) install.packages("mlr"); library("mlr")

# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
# load data
source('load_data.R')
# process data
dn = amend_features(df_known)
## calculate test set indices from prediction by setting the same seed
set.seed(1)
idx.train = caret::createDataPartition(y = dn$return, p = 0.8, list = FALSE) 
ts = dn[-idx.train, ]
## Setup the true return values for the known dataset as a whole and the test set
true_returns_known = subset(dn, select = c('order_item_id', 'return'))
true_returns_test = subset(ts, select = c('order_item_id', 'return'))
## setup a list to save performance measure results for individual base models
metrics <- list()

## h2o.deeplearning predictions
h2o_known <- read.csv('data/h2o_known.csv')
h2o_known$return <- ifelse(h2o_known$return > 0.5, 1, 0)
h2o_test = h2o_known[-idx.train, ]
# known dataset
metrics[["h2o_known"]] = HMeasure(true_returns_known$return, h2o_known$return)
# test dataset
metrics[["h2o_test"]] = HMeasure(true_returns_test$return, h2o_test$return)

## nnet predictions
nnet_known <- read.csv('data/nnet_known.csv')
nnet_known$return <- ifelse(nnet_known$return > 0.5, 1, 0)
nnet_test = nnet_known[-idx.train, ]
# known dataset
metrics[["nnet_known"]] = HMeasure(true_returns_known$return, nnet_known$return)
# test dataset
metrics[["nnet_test"]] = HMeasure(true_returns_test$return, nnet_test$return)

## rf predictions
rf_known <- read.csv('data/rf_known.csv')
rf_known$return <- ifelse(rf_known$return > 0.5, 1, 0)
rf_test = rf_known[-idx.train, ]
# known dataset
metrics[["rf_known"]] = HMeasure(true_returns_known$return, rf_known$return)
# test dataset
metrics[["rf_test"]] = HMeasure(true_returns_test$return, rf_test$return)

## xgboost predictions
xgboost_known <- read.csv('data/xgboost_known.csv')
xgboost_known$return <- ifelse(xgboost_known$return > 0.5, 1, 0)
xgboost_test = xgboost_known[-idx.train, ]
# known dataset
metrics[["xgboost_known"]] = HMeasure(true_returns_known$return, xgboost_known$return)
# test dataset
metrics[["xgboost_test"]] = HMeasure(true_returns_test$return, xgboost_test$return)

## ranger predictions
ranger_known <- read.csv('old/ranger_known.csv')
ranger_known$return <- ifelse(xgboost_known$return > 0.5, 1, 0)
ranger_test = ranger_known[-idx.train, ]
# known dataset
metrics[["ranger_known"]] = HMeasure(true_returns_known$return, ranger_known$return)
# test dataset
metrics[["ranger_test"]] = HMeasure(true_returns_test$return, ranger_test$return)

## Data frame with ACC and AUC for base models
base.model_performance <- rbind(PCC = c(1-metrics[["h2o_known"]]$metrics$ER, 
                                        1-metrics[["h2o_test"]]$metrics$ER, 
                                        1-metrics[["nnet_known"]]$metrics$ER, 
                                        1-metrics[["nnet_test"]]$metrics$ER, 
                                        1-metrics[["rf_known"]]$metrics$ER, 
                                        1-metrics[["rf_test"]]$metrics$ER, 
                                        1-metrics[["xgboost_known"]]$metrics$ER, 
                                        1-metrics[["xgboost_test"]]$metrics$ER,
                                        1-metrics[["ranger_known"]]$metrics$ER,
                                        1-metrics[["ranger_test"]]$metrics$ER), 
                                AUC = c(metrics[["h2o_known"]]$metrics$AUC, 
                                        metrics[["h2o_test"]]$metrics$AUC, 
                                        metrics[["nnet_known"]]$metrics$AUC, 
                                        metrics[["nnet_test"]]$metrics$AUC, 
                                        metrics[["rf_known"]]$metrics$AUC, 
                                        metrics[["rf_test"]]$metrics$AUC, 
                                        metrics[["xgboost_known"]]$metrics$AUC, 
                                        metrics[["xgboost_test"]]$metrics$AUC,
                                        metrics[["ranger_known"]]$metrics$AUC,
                                        metrics[["ranger_test"]]$metrics$AUC
                                      ))

colnames(base.model_performance) <- c('h2o_known', 
                                      'h2o_test', 
                                      'nnet_known', 
                                      'nnet_test', 
                                      'rf_known', 
                                      'rf_test', 
                                      'xgboost_known', 
                                      'xgboost_test',
                                      'ranger_known',
                                      'ranger_test'
                                    )
print(base.model_performance)
save(base.model_performance, file = "data/acc_auc_base_models")
###