### h2o.deeplearning overfitting test

if(!require("h2o")) install.packages("h2o"); library("h2o")
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")
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
  h2o_learner = makeLearner(
    "classif.h2o.deeplearning",
    predict.type = "prob",
    par.vals = list(
      activation = "RectifierWithDropout",
      hidden = c(61, 38, 58),
      epochs = 10,
      rho = 0.9058344,
      epsilon = 8.905493e-06,
      input_dropout_ratio = 0.06722173,
      hidden_dropout_ratios = c(0.00323078, 0.68089378, 0.42031610),
      max_w2 = 3.338197
      # num.threads = 4,
      # verbose = T)
    ))
  cv.h2o = crossval(learner = h2o_learner,
                     task = train_task,
                     iters = 5,
                     stratify = TRUE,
                     measures = mlr::acc,
                     show.info = T)
  h2o_model = mlr::train(h2o_learner, train_task)
  
  ts$pred <- predict(h2o_model, newdata=ts)$data$response
  tr$pred <- predict(h2o_model, newdata=tr)$data$response
  results =  data.frame(part, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

h2o_overfit_plot <- ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
h2o_overfit_plot
save(h2o_overfit_plot, file = "data/h2o_overfit_plot")
