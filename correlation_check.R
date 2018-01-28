#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')

#svm_known = read.csv("data/.csv", stringsAsFactors = FALSE)
#svm_class = read.csv("data/.csv", stringsAsFactors = FALSE)
xgboost_known = read.csv("data/xgboost_known.csv", stringsAsFactors = FALSE)
rf_known = read.csv("data/randomforest_probability_retail.csv", stringsAsFactors = FALSE)
nnet_known = read.csv("data/nnet_known.csv", stringsAsFactors = FALSE)

# sort everything first
xgboost_known = xgboost_known[order(xgboost_known$order_item_id),]
xgboost_class = xgboost_class[order(xgboost_class$order_item_id),]
rf_known = rf_known[order(rf_known$order_item_id),]
rf_class = rf_class[order(rf_class$order_item_id),]
nnet_known = nnet_known[order(nnet_known$order_item_id),]
nnet_class = nnet_class[order(nnet_class$order_item_id),]

# assert if we are missing labels
known_labels_complete = 
  xgboost_known$order_item_id == rf_known$order_item_id &&
  rf_known$order_item_id == nnet_known$order_item_id

stopifnot(known_labels_complete)

# create learning and prediction dataframes
df_known = data.frame(xgboost_known$return,
                      rf_known$pred,
                      nnet_known$return)

colnames(df_known) = c("xgboost_return",
                       "rf_return",
                       "nnet_return")

cor(df_known)