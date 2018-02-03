### Random forest with h2o overfitting test
library(mlr)
library(dplyr)
library(caret)

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
  dd$return    = as.factor(dd$return)
  
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

localH2O = h2o.init(ip='localhost',
                    nthreads=-1,
                    min_mem_size='4G',
                    max_mem_size='5G') # with system memory of 8GB

for (part in seq(0.05,0.8,0.05)) {

  
  set.seed(1)
  idx.train <- createDataPartition(y=df_known$return, p=part, list = FALSE) 
  
  tr <- df_known[idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  tr = amend_features(tr)
  ts = amend_features(ts)
  
  train_task  = makeClassifTask(data = tr, target = "return", positive = 1)
  rf_learner = makeLearner(
    "classif.h2o.randomForest",
    predict.type = "prob",
    par.vals = list(
      ntrees   = 241, # values taken from hyperparameter tuning
      mtries   = 8,
      min_rows = 47
    )
  )
  cv.h2o = crossval(learner   = rf_learner,
                       task      = train_task,
                       iters     = 5,
                       stratify  = TRUE,
                       measures  = acc,
                       show.info = T)
  rf_model = mlr::train(rf_learner, train_task)
  
  ts$pred <- predict(rf_model, newdata=ts)$data$response
  tr$pred <- predict(rf_model, newdata=tr)$data$response
  results =  data.frame(part*n, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

h2o.rf_overfit_plot <- ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
h2o.rf_overfit_plot
save(h2o.rf_overfit_plot, file = "data/h2o.rf_overfit_plot")
