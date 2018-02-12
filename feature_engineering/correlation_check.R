#svm_known = read.csv("data/.csv", stringsAsFactors = FALSE)
#svm_class = read.csv("data/.csv", stringsAsFactors = FALSE)
xgboost_known = read.csv("data/xgboost_known.csv", stringsAsFactors = FALSE)
rf_known = read.csv("data/rf_known.csv", stringsAsFactors = FALSE)
nnet_known = read.csv("data/nnet_known.csv", stringsAsFactors = FALSE)
h2o_known = read.csv("data/h2o_known.csv", stringsAsFactors = FALSE)


# sort everything first
xgboost_known = xgboost_known[order(xgboost_known$order_item_id),]
rf_known = rf_known[order(rf_known$order_item_id),]
nnet_known = nnet_known[order(nnet_known$order_item_id),]
h2o_known = h2o_known[order(h2o_known$order_item_id),]

# assert if we are missing labels
known_labels_complete = 
  xgboost_known$order_item_id == rf_known$order_item_id &&
  rf_known$order_item_id == nnet_known$order_item_id &&
  nnet_known$order_item_id == h2o_known$order_item_id

stopifnot(known_labels_complete)

# create learning and prediction dataframes
df_known = data.frame(xgboost_known$return,
                      rf_known$return,
                      nnet_known$return,
                      h2o_known$return)

colnames(df_known) = c("xgboost_return",
                       "rf_return",
                       "nnet_return",
                       "h2o_known")

cor(df_known)
