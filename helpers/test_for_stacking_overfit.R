if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")

# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
source('load_data.R')

df_known = amend_features(df_known)
df_known[is.na(df_known)] = 0
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
  tr <- df_known[ idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  train_task = makeClassifTask(data = tr, target = "return", positive = 1)
  resample_desc = makeResampleDesc("CV", iters = 5)
  rf_learner = makeLearner(
    "classif.h2o.randomForest",
    predict.type = "prob",
    par.vals = list(
      ntrees=395,
      mtries=7,
      min_rows=59
    )
  )
  cv.ranger = crossval(learner = rf_learner,
                       task = train_task,
                       iters = 5,
                       stratify = TRUE,
                       measures = mlr::acc,
                       show.info = T)
  
  trf_model = mlr::train(rf_learner, train_task)
  ts$pred <- ifelse(predict(trf_model, newdata=ts)$data$prob.1 > 0.5,1,0)
  tr$pred <- ifelse(predict(trf_model, newdata=tr)$data$prob.1 > 0.5,1,0)
  results =  data.frame(part*n, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

xgboost_overfit_plot <- ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
xgboost_overfit_plot
save(xgboost_overfit_plot, file = "data/xgboost_overfit_plot")
