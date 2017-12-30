# Neural network with package nnet

getwd()

if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("pROC")) install.packages("pROC"); library("pROC")

source("load_data.R")

retail<-read_and_preprocess_data_file("data/BADS_WS1718_known.csv")

set.seed(123)

# Creating training and test data set
idx.train<-createDataPartition(y = retail$return, p = 0.8, list = FALSE)
tr<-retail[idx.train,] # is the unstandardized training set
ts<-retail[-idx.train,] # is the unstandardized test set

normalizer<-caret::preProcess(tr, method = c("center", "scale"))
tr<-predict(normalizer, newdata = tr) # is the standardized training set
ts<-predict(normalizer, newdata = ts) # is the standardized test set

# Cross validation and parameter tuning
k<-5
set.seed(321)
folds <- cut(1:nrow(tr), breaks = k, labels = FALSE)
folds <- sample(folds)
nnet.param <- expand.grid("size" = seq(from = 3, to = 15, by = 3), "decay" = c(0.001, 0.01, 0.1, 1))
results<-as.data.frame(matrix(NA, ncol = nrow(nnet.param), nrow = k))
for(n in 1:nrow(nnet.param)){
  for(i in 1:k){
    idx.val<-which(folds == i, arr.ind = TRUE)
    cv.train<-tr[-idx.val,]
    cv.val<-tr[idx.val,]
    nn<-nnet(return ~ user_dob + user_maturity + delivery_duration + user_state + item_price + item_color + item_size + month_of_delivery,
             data = cv.train,
             trace = FALSE,
             maxit = 200, # choice based on http://cowlet.org/2014/01/12/understanding-data-science-classification-with-neural-networks-in-r.html
             MaxNWts = 10000, # manual setting as weights exceed standard threshold of 1,000
             size = nnet.param$size[n],
             decay = nnet.param$decay[n])
    yhat.val<-predict(nn, newdata = cv.val, type = "raw")
    results[i, n]<-auc(as.numeric(cv.val$return)-1, as.numeric(as.vector(yhat.val)))
  }
}

head(results)
nnet.param$avg_auc<-apply(results, 2, mean)
nnet.param
opt.auc<-which.max(nnet.param$avg_auc)
nnet.param[opt.auc,]

# Optimal model using nnet
nn_tuned<-nnet(return ~ user_dob + user_maturity + delivery_duration + user_state + item_price + item_color + item_size + month_of_delivery,
               data = tr,
               trace = FALSE,
               maxit = 200, # choice based on http://cowlet.org/2014/01/12/understanding-data-science-classification-with-neural-networks-in-r.html
               MaxNWts = 10000, # manual setting as weights exceed standard threshold of 1,000
               size = nnet.param$size[opt.auc],
               decay = nnet.param$decay[opt.auc])

# Predictions and performance of tuned model for test data set
yhat<-list()
yhat[["nn_tuned"]]<-predict(nn_tuned, newdata = ts, type = "prob")
h <- HMeasure(true.class = as.numeric(ts$return)-1, scores = data.frame(yhat))
h

# Saving optimal nnet model
saveRDS(nn_tuned, file = "models/Nnet_Model.R") 

# Predictions of tuned model for unknown data
retail_class<-read_and_preprocess_data_file("BADS_WS1718_class.csv")
retail_class<-predict(normalizer, newdata = retail_class)
pred_nnet_prob<-predict(nn_tuned, newdata = retail_class, type = "prob")

# Saving nnet class predictions
write.csv(pred_nnet_prob, file = "data/Nnet_Predictions_Prob.csv")

# Making and saving nnet class predictions
pred_nnet_class<-predict(nn_tuned, newdata = retail_class, type = "class")
write.csv(pred_nnet_class, file = "data/Nnet_Predictions_Class.csv")

