### Random forest

if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")

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

return <- "return"
vars <- c("user_id", 
          "order_item_id", 
          "item_id", 
          "item_size", 
          "item_color", 
          "brand_id", 
          "user_dob", 
          "delivery_duration", 
          "user_maturity", 
          "price_and_age", 
          "avg_return", 
          "nr_obs", 
          "order_month", 
          "order_day", 
          "del_year", 
          "del_month", 
          "del_day")
fmla <- as.formula(paste(return, "~", paste(vars, collapse = " + ")))

## Take tuned parameters
for (part in seq(0.05,0.8,0.05)) {
  set.seed(1)
  idx.train <- createDataPartition(y = df_known$return, p = part, list = FALSE) 
  tr <- df_known[idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  train_task = makeClassifTask(data = tr, target = "return", positive = 1)
  resample_desc = makeResampleDesc("CV", iters = 5)
  ranger_learner = makeLearner(
    "classif.ranger",
    predict.type = "prob",
    par.vals = list(
      num.trees = 800,
      mtry = 4,
      num.threads = 4,
      verbose = T)
  )
  cv.ranger = crossval(learner = ranger_learner,
                       task = train_task,
                       iters = 5,
                       stratify = TRUE,
                       measures = acc,
                       show.info = T)
  ranger_model = mlr::train(ranger_learner, train_task)
  
  ts$pred <- predict(ranger_model, newdata=ts)$data$response
  tr$pred <- predict(ranger_model, newdata=tr)$data$response
  results =  data.frame(part, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

ranger_overfit_plot <- ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 

save(ranger_overfit_plot, file = "old/ranger_overfit_plot")