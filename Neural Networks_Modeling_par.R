# Neural network with parallel computing
getwd()

if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("pROC")) install.packages("pROC"); library("pROC")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("foreach")) install.packages("foreach"); library("foreach")

source("load_data.R")

retail<-read_and_preprocess_data_file("data/BADS_WS1718_known.csv")

# Convert target variable into a factor
retail$return<-factor(retail$return, levels = c(0,1), labels = c("No Return","Return"))

set.seed(123)

# Creating training and test data set
idx.train<-createDataPartition(y = retail$return, p = 0.8, list = FALSE)
tr<-retail[idx.train,] # is the unstandardized training set
ts<-retail[-idx.train,] # is the unstandardized test set

normalizer<-caret::preProcess(tr, method = c("center", "scale"))
tr<-predict(normalizer, newdata = tr) # is the standardized training set
ts<-predict(normalizer, newdata = ts) # is the standardized test set

# The number of folds and the specific folds for cv
k <- 5
train.rnd <- tr[sample(nrow(tr)),]
folds <- cut(1:nrow(train.rnd), breaks = k, labels = FALSE)

# The number of nodes to try for the model
nnet.param <- expand.grid("size" = seq(from = 3, to = 15, by = 3), "decay" = c(0.001, 0.01, 0.1, 1))

# Setup up parallel backend
par_cluster <- makeCluster(max(1, detectCores()-1))
registerDoParallel(par_cluster)

# Parallelization with foreach
results.par <- foreach(n = 1:length(nnet.param$size), .combine = cbind, .packages = c("caret", "nnet", "pROC")) %:%
    foreach(i = 1:k, .combine = c, .packages = c("caret","nnet", "pROC")) %dopar%{
      idx.val <- which(folds == i, arr.ind = TRUE)
      cv.train <- train.rnd[-idx.val,]
      cv.val <- train.rnd[idx.val,]
      neuralnet <- nnet(return ~ user_dob + user_maturity + delivery_duration + user_state + item_price + item_color + item_size + month_of_delivery,
                        data = cv.train, 
                        trace = FALSE, 
                        maxit = 1000,
                        MaxNWts = 10000, # manual setting as weights exceed standard threshold of 1,000
                        size = nnet.param$size[n],
                        decay = nnet.param$decay[n])
      yhat <- predict(neuralnet, newdata = cv.val, type = "raw")
      auc(cv.val$return, as.vector(yhat))
    }

# Close the cluster
stopCluster(par_cluster)

# View results and identify maximum auc
head(results.par)
nnet.param$avg_auc<-apply(results.par, 2, mean)
nnet.param
opt.auc<-which.max(nnet.param$avg_auc)
nnet.param[opt.auc,]

# Optimal model using nnet
nn_tuned_par<-nnet(return ~ user_dob + user_maturity + delivery_duration + user_state + item_price + item_color + item_size + month_of_delivery,
               data = tr,
               trace = FALSE,
               maxit = 200, # choice based on http://cowlet.org/2014/01/12/understanding-data-science-classification-with-neural-networks-in-r.html
               MaxNWts = 10000, # manual setting as weights exceed standard threshold of 1,000
               size = nnet.param$size[opt.auc],
               decay = nnet.param$decay[opt.auc])

# Predictions and performance of tuned model for test data set
yhat<-list()
yhat[["nn_tuned_par"]]<-predict(nn_tuned_par, newdata = ts, type = "raw")
h <- HMeasure(true.class = as.numeric(ts$return)-1, scores = data.frame(yhat))
h

# Saving optimal nnet model
saveRDS(nn_tuned_par, file = "models/Nnet_Model_Par.R") 

# Predictions of tuned model for unknown data
retail_class<-read_and_preprocess_data_file("data/BADS_WS1718_class.csv")
retail_class<-predict(normalizer, newdata = retail_class)
retail_class$item_color[which(as.character.factor(retail_class$item_color)=="cortina mocca")]<-NA
retail_class$item_size[which(as.character.factor(retail_class$item_size)=="3132")]<-NA
retail_class$item_size[which(as.character.factor(retail_class$item_size)=="4034")]<-NA
retail_class$item_size[which(as.character.factor(retail_class$item_size)=="49")]<-NA

pred_nnet_prob_par<-predict(nn_tuned_par, newdata = retail_class, type = "raw")
pred_nnet_class_par<-predict(nn_tuned_par, newdata = retail_class, type = "class")

# Saving nnet probability predictions
write.csv(pred_nnet_prob_par, file = "data/Nnet_Predictions_Prob_Par.csv")

# Saving nnet class predictions
write.csv(pred_nnet_class_par, file = "data/Nnet_Predictions_Class_Par.csv")

