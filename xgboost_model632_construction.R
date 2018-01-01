if(!require("sortinghat")) install.packages("sortinghat"); library("sortinghat")
if(!require("Matrix")) install.packages("Matrix"); library("Matrix")
if(!require("xgboost")) install.packages("xgboost"); library("xgboost")
if(!require("data.table")) install.packages("data.table"); library("data.table")

setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')

# prevent NA omission in sparse matrix creation
# https://stackoverflow.com/questions/29732720/sparse-model-matrix-loses-rows-in-r
set.seed(123)

# TODO: optimize parameter selection
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

probs = c()
accs  = c()
nround = 1
N = 1000
bst = NULL

previous_na_action <- options('na.action')
options(na.action='na.pass')

for (iter in 1:400) {
  sampled_order_ids = sample(nrow(d), replace = TRUE)
  sampled_order_ids = unique(sampled_order_ids)
  dds = d[sampled_order_ids,]
  probs = append(probs, nrow(dds)/nrow(d))
  
  sparse_matrix = sparse.model.matrix(return~.-1, dds)
  output_vector = dds$return == 1
  xgb.train.data = xgb.DMatrix(sparse_matrix, label = output_vector)
  
  bst = xgb.train(
    xgb.train.data,
    params=param,
    xgb_model = bst,
    nrounds=nround)
  
  predicted_classes = predict(bst, newdata = sparse_matrix) 
  accuracy = mean((predicted_classes > .5) == output_vector)
  accs = append(accs, accuracy)
}

options(na.action=previous_na_action$na.action)
plot(x=1:length(accs), y=accs, type='p')