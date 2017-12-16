# Split-sample testing

# Model Selection: Picking the model
if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("rpart")) install.packages("rpart"); library("rpart") 
if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")
set.seed(124)
n <- nrow(retail) 
sample.size <- ceiling(n*0.8)
idx.train <- createDataPartition(y =retail$return, p = 0.75, list = FALSE) 
tr <- retail[idx.train, ] # training set
ts <-  retail[-idx.train, ] # test set 
#x_tr <- model.matrix(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size,data = retail -1, tr)
#y_tr <- tr$return
#x_ts <- model.matrix(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size,data = retail -1, ts)
#y_ts <- ts$return
prob_yhat <- list()
# Logistic regression
# lr <- glm(return~., data = tr, family = binomial(link = "logit"))
# Regularized logistic regression
# lasso <- glmnet(x = x_tr, y = y_tr, family = "binomial", standardize = TRUE,
                # alpha = 1, nlambda = 100)
# Decision tree
dt <- rpart(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size, data = tr, method = "class", control = rpart.control(cp = 0.001, minsplit = 20)) # create decision tree classifier
# yhat[["lr"]] <- predict(lr, newdata = ts, type = "response")
# yhat[["lasso"]] <- as.vector( predict(lasso, newx = x_ts, s = 0.001, type = "response") )
prob_yhat[["dt"]] <- predict(dt, newdata = ts, type = "prob")[, 2] 
prob_yhat.df <- data.frame(prob_yhat)
h <- HMeasure( as.numeric(ts$return)-1, prob_yhat.df, severity.ratio = 0.1) 
auc_splitsampling <- h$metrics['AUC']


auc_splitsampling




# Model Selection: Picking the metaparameters

set.seed(321)
idx_validation <- createDataPartition(y = tr$return, p = 0.25, list = FALSE) 
val <- tr[idx_validation, ] # training set
tr_val <-  tr[-idx_validation, ] # test set 
#x_tr_val <- model.matrix(return~.-1, tr_val)
#y_tr_val <- tr_val$return
#x_val <- model.matrix(return~.-1, val)
#y_val <- val$return
# Lambda tuning for Lasso
# lasso_val <- glmnet(x = x, y = y, family = "binomial", standardize = TRUE,
                    # alpha = 1, nlambda = 100)

# lambda_candidates <- data.frame("lambda" = lasso$lambda, "AUC" = NA)
# for(i in 1:nrow(lambda_candidates)){
  # yhat_val <- as.vector( predict(lasso_val, newx = x_val, s = lambda_candidates$lambda[i], type = "response") )
  # lambda_candidates[i, "AUC"] <- ModelMetrics::auc( as.numeric(val$return)-1, yhat_val) 
# }
# Decision tree
cp_candidates <- data.frame("cp" = seq(0, 0.025, length.out = 50),
                            "AUC" = NA)
for(i in 1:nrow(cp_candidates)){
  dt_val <- rpart(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size, data = tr_val, method = "class", control = rpart.control(cp = cp_candidates$cp[i], minsplit = 20)) # create decision tree classifier
  prob_yhat_val <- as.vector( predict(dt_val, newdata = val, type = "prob")[,2] )
  cp_candidates[i, "AUC"] <- ModelMetrics::auc( as.numeric(val$return)-1, prob_yhat_val) }
# Training the optimal model on the full training data 
# Best lamda candidate
# lambda_candidates[which.max(lambda_candidates$AUC),]
# Decision tree
cp_candidates[which.max(cp_candidates$AUC),]
dt <- rpart(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size, data = tr, method = "class", control = rpart.control(cp = 0.003, minsplit = 20)) # create decision tree classifier
prp(dt, extra = 104, border.col = 0, box.palette="auto")
# Calculate the predictions 
# yhat[["lr"]] <- predict(lr, newdata = ts, type = "response")
# yhat[["lasso"]] <- as.vector( predict(lasso, newx = x_ts, s = 0.0099, type = "response") )
prob_yhat[["dt"]] <- predict(dt, newdata = ts, type = "prob")[, 2] 
yhat_dt <- predict(dt, newdata = ts, type = "class")
# Assess performance on the test data 
prob_yhat.df <- data.frame(prob_yhat)  
h_parameters_tuned <- HMeasure( as.numeric(ts$return)-1, prob_yhat.df, severity.ratio = 0.1) 
auc_parameters_tuned <- h_parameters_tuned$metrics['AUC']

auc_naive
auc_splitsampling
auc_parameters_tuned

#  5 Fold Cross-validation for metaparameter selection

k <- 5
folds <- cut(1:nrow(tr), breaks = k, labels = FALSE)
set.seed(123)
folds <- sample(folds)
head(folds, 20)
?cut
# CV loop
cp_candidates <- seq(0, 0.01, length.out = 20)
cv_results <- matrix(nrow = length(cp_candidates), ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- tr[-idx_val,]
  cv_val <- tr[idx_val,]
  # Build and evaluate models using the partitions
  for(i in 1:length(cp_candidates)){
    cv_dt <- rpart(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size, data = cv_train, method = "class", control = rpart.control(cp = cp_candidates[i], minsplit = 20)) # create decision tree classifier
    prob_cv_yhat_dt <- predict(cv_dt, newdata = cv_val, type = "prob")[,2]
    cv_results[i, j] <- ModelMetrics::auc( as.numeric(cv_val$return)-1, prob_cv_yhat_dt) }
}
dt.helper <- function(Train = NULL, Val = NULL, Tau = 0.5, control) {
  dt <- rpart(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size, data = Train, method = "class", control = control) # create decision   tree classifier
  prob_yhat <- predict(dt, newdata = Val, type = "prob")[,2]
  auc <- ModelMetrics::auc(as.numeric(Val$return)-1, predicted = prob_yhat)
  return(auc) }
dt.helper(cv_train, cv_val, control = rpart.control(cp = 0.01, minsplit = 20))

# Average performance
cv_perf_mean <- apply(cv_results, 1, mean)
cv_perf_sd <- apply(cv_results, 1, sd)

# Plot the results
library(ggplot2)
qplot(x = 1:length(cv_perf_mean), y = cv_perf_mean) + geom_errorbar(aes(ymin = cv_perf_mean - cv_perf_sd, ymax = cv_perf_mean + cv_perf_sd))

# The best performing candidate  
cp_optimal <- which.max(cv_perf_mean)
cp_candidates[cp_optimal]

# Evaluate the models on the test data again
dt_cv <- rpart(return~., data = tr, method = "class", control = rpart.control(cp = cp_candidates[cp_optimal], minsplit = 20)) # create decision tree classifier
prob_yhat[["dt_cv"]] <- predict(dt_cv, newdata = ts, type = "prob")[, 2] # calculate predictions (in-sample)
prob_yhat.df <- data.frame(prob_yhat)
h <- HMeasure( as.numeric(ts$return)-1, prob_yhat.df, severity.ratio = 0.1) 
auc_testset <- h$metrics['AUC']

auc_testset

# 5-Fold CV with more than one metaparameter

# CV loop
candidates <- expand.grid("cp" = seq(0, 0.01, length.out = 20), "minsplit" = seq(0, 20, 5))

cv_results <- matrix(nrow = nrow(candidates), ncol = k)

for (j in 1:k) {
  
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- tr[-idx_val,]
  cv_val <- tr[idx_val,]
  
  for(i in 1:nrow(candidates)){
    cv_results[i,j] <- dt.helper(Train = cv_train, Val = cv_val, 
                                 control = rpart.control(cp = candidates$cp[i], minsplit = candidates$minsplit[i]))}}
# Average performance 
cv_perf_mean2 <- apply(cv_results, 1, mean)
cv_perf_sd2 <- apply(cv_results, 1, sd)
cv_cand_perf <- cbind(candidates, cv_perf_mean2)
round(tapply(cv_perf_mean2, INDEX = candidates, FUN = mean), 4)

# The best performing candidate  
param_optimal <- which.max(cv_perf_mean2)
candidates[param_optimal,]

# Evaluate the models on the test data 
dt_cv <- rpart(return ~  user_dob + user_maturity + user_state + item_price + item_color +item_size, data = tr, method = "class", 
               control = rpart.control(cp = candidates$cp[param_optimal], minsplit = candidates$minsplit[param_optimal])) # create decision tree classifier
prob_yhat[["dt_cv_2"]] <- predict(dt_cv, newdata = ts, type = "prob")[, 2] # calculate predictions (in-sample)
prob_yhat.df <- data.frame(prob_yhat)
h <- HMeasure( as.numeric(ts$return)-1, prob_yhat.df, severity.ratio = 0.1) 
auc_testset <- h$metrics['AUC']

auc_testset
