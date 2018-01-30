if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")

source('load_data.R')
amend_features = function(dd){
  dd = subset(dd, select = -c(user_dob,
                              user_maturity,
                              user_title,
                              user_state,
                              item_color,
                              delivery_date,
                              item_size))
  
  dd$order_year  = as.numeric(format(dd$order_date, "%Y"))
  dd$order_month = as.numeric(format(dd$order_date, "%m"))
  dd$order_day   = as.numeric(format(dd$order_date, "%d"))
  dd             = subset(dd, select=-order_date)
  
  dd$reg_year  = as.numeric(format(dd$user_reg_date, "%Y"))
  dd$reg_month = as.numeric(format(dd$user_reg_date, "%m"))
  dd$reg_day   = as.numeric(format(dd$user_reg_date, "%d"))
  dd           = subset(dd, select=-user_reg_date)
  
  if("return" %in% colnames(dd)) {
    dd = normalizeFeatures(dd, target="return")
    #dd = createDummyFeatures(dd, target="return", cols=c("item_size"))
  } else {
    #dd = createDummyFeatures(dd, cols=c("item_size"))
  }
  
  return(dd)
}

n = nrow(df_known)
test.results <- data.frame(matrix(ncol = 3, nrow = 0))
rn <- c("tr_size", "ts_acc", "tr_acc")
colnames(test.results) = rn

for (part in seq(0.05,0.8,0.05)) {
  set.seed(1)
  idx.train <- createDataPartition(y=df_known$return, p=part, list = FALSE) 
  tr <- df_known[idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  tr = amend_features(tr)
  ts = amend_features(ts)
  
  train_task = makeClassifTask(data = tr, target = "return", positive = 1)
  resample_desc = makeResampleDesc("CV", iters = 5)
  xgb_learner = makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    par.vals = list(
      objective = "binary:logistic",
      eval_metric = "error",
      nrounds = 200,
      base_score = mean(tr$return)
    )
  )
  #cv.ranger = crossval(learner = xgb_learner,
  #                    task = train_task,
  #                   iters = 5,
  #                  stratify = TRUE,
  #                 measures = acc,
  #                show.info = T)
  txgb_model = mlr::train(ranger_learner, train_task)
  
  ts$pred <- ifelse(predict(txgb_model, newdata=ts)$data$prob.1 > 0.5,1,0)
  tr$pred <- ifelse(predict(txgb_model, newdata=tr)$data$prob.1 > 0.5,1,0)
  results =  data.frame(part*n, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
