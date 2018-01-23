#svm_known = read.csv("data/.csv", stringsAsFactors = FALSE)
xgboost_known = read.csv("data/xgboost_known.csv", stringsAsFactors = FALSE)
rf_known = read.csv("data/randomforest_probability_retail.csv", stringsAsFactors = FALSE)
nnet_known = read.csv("data/nnet_known.csv", stringsAsFactors = FALSE)
d = read.csv('data/BADS_WS1718_known.csv', stringsAsFactors = FALSE)

cost = data.frame(ifelse(d$return == 0, 0, 2.5*(3+0.1*d$item_price)),
                  ifelse(d$return == 1, 0, 0.5 * d$item_price))

xgboost_cost = 0
rf_cost = 0
nnet_cost = 0

calculate_item_loss = function(real,pred,c) {
  if (real == 1 && pred == 0) {
    return(2.5*(3+0.1*c))
  } else if (real == 0 && pred == 1) {
    return(0.5 * c)
  } else {
    return(0)
  }
}



for (row in 1:nrow(d)) {
  xgboost_cost = xgboost_cost + calculate_item_loss(d[row,"return"],
                                                    ifelse(xgboost_known[row,"return"] > 0.5,1,0),
                                                    d[row,"item_price"])
  rf_cost = rf_cost + calculate_item_loss(d[row,"return"],
                                          ifelse(rf_known[row,"pred"] > 0.5,1,0),
                                          d[row,"item_price"])
  nnet_cost = nnet_cost + calculate_item_loss(d[row,"return"],
                                              ifelse(nnet_known[row,"return"] > 0.5,1,0),
                                              d[row,"item_price"])
}

print(paste("xgboost cost: ",as.character(xgboost_cost)))
print(paste("random forest cost: ", as.character(rf_cost)))
print(paste("nnet cost: ", as.character(nnet_cost)))
