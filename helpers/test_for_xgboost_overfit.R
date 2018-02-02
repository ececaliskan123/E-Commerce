if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")

# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
# load data
source('load_data.R')

df_known = amend_features(df_known)
df_known[is.na(df_known)] = 0
n = nrow(df_known)

test.results <- data.frame(matrix(ncol = 3, nrow = 0))
rn <- c("tr_size", "ts_acc", "tr_acc")
colnames(test.results) = rn

parallelStartSocket(parallel::detectCores())

for (part in seq(0.05,0.8,0.05)) {
  set.seed(1)
  idx.train <- createDataPartition(y=df_known$return, p=part, list = FALSE) 
  tr <- df_known[idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  train_task = makeClassifTask(data = tr, target = "return", positive = 1)
  resample_desc = makeResampleDesc("CV", iters = 5)
  xgb_learner = makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    par.vals = list(
      objective = "binary:logistic",
      eval_metric = "error",
      nrounds = 200,
      base_score = mean(as.numeric(as.character(tr$return)))
    )
  )
  cv.ranger = crossval(learner = xgb_learner,
                       task = train_task,
                       iters = 5,
                       stratify = TRUE,
                       measures = mlr::acc,
                       show.info = T)
  txgb_model = mlr::train(xgb_learner, train_task)
  
  ts$pred <- ifelse(predict(txgb_model, newdata=ts)$data$prob.1 > 0.5,1,0)
  tr$pred <- ifelse(predict(txgb_model, newdata=tr)$data$prob.1 > 0.5,1,0)
  results =  data.frame(part*n, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

parallelStop()

xgboost_overfit_plot <- ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
xgboost_overfit_plot
save(xgboost_overfit_plot, file = "data/xgboost_overfit_plot")
