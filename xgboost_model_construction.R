if(!require("caret")) install.packages("caret"); library("caret")
if(!require("prettyR")) install.packages("prettyR"); library("prettyR")
if(!require("Matrix")) install.packages("Matrix"); library("Matrix")
if(!require("xgboost")) install.packages("xgboost"); library("xgboost")

setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')

# prevent NA omission in sparse matrix creation
# https://stackoverflow.com/questions/29732720/sparse-model-matrix-loses-rows-in-r
previous_na_action <- options('na.action')
options(na.action='na.pass')
sparse_matrix = sparse.model.matrix(return~.-1, d)
output_vector = d$return == 1
options(na.action=previous_na_action$na.action)

# jackpot: https://stackoverflow.com/questions/35050846/xgboost-in-r-how-does-xgb-cv-pass-the-optimal-parameters-into-xgb-train
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0

for (iter in 1:100) {
  # parameter recommendations: https://www.slideshare.net/odsc/owen-zhangopen-sourcetoolsanddscompetitions1
  param = list(objective = "binary:logistic",
    eval_metric = "error",
    max_depth = sample(6:10, 1),
    eta = runif(1, .01, .3),
    gamma = 0,#runif(1, 0.0, 0.2), 
    subsample = runif(1, .6, .9),
    colsample_bytree = runif(1, .3, .5), 
    min_child_weight = sample(1:40, 1),
    max_delta_step = sample(1:10, 1),
    base_score = 0.48174 # mean(d$return)
  )
  
  cv.nround = 1000
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  
  set.seed(seed.number)
  mdcv = xgb.cv(data = sparse_matrix,
                label = output_vector,
                params = param,
                nfold=cv.nfold,
                nrounds=cv.nround,
                verbose = T,
                early.stop.round=8,
                maximize=FALSE)
  
  min_logloss = min(mdcv$evaluation_log[, test_error_mean])
  min_logloss_index = mdcv$best_iteration #which.min(mdcv[, test.error.mean])
  
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}

xgb.train.data = xgb.DMatrix(sparse_matrix, label = output_vector)
nround = best_logloss_index
set.seed(best_seednumber)
bst = xgb.train(
  xgb.train.data,
  params=best_param,
  nrounds=nround)

xgb.save(bst, 'models/xgboost.model')

predicted_classes = predict(bst, newdata = sparse_matrix) 
accuracy = mean((predicted_classes > .5) == output_vector)
