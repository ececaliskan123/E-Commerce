### Random forest
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

return <- "return"
vars <- c("user_id", "order_item_id", "order_year", "order_month", "order_day",
          "month_of_delivery", "price_and_age",
          "item_id", "brand_id", "item_price",
          "reg_year","reg_month","reg_day", "delivery_duration")
fmla <- as.formula(paste(return, "~", paste(vars, collapse = " + ")))

for (part in seq(0.05,0.8,0.05)) {
  set.seed(1)
  idx.train <- createDataPartition(y=df_known$return, p=part, list = FALSE) 
  tr <- df_known[idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  tr = amend_features(tr)
  ts = amend_features(ts)
  
  train_task = makeClassifTask(data = tr, target = "return", positive = 1)
  resample_desc = makeResampleDesc("CV", iters = 5)
  ranger_learner = makeLearner(
    "classif.ranger",
    predict.type = "prob",
    par.vals = list(
      num.trees = 500,
      mtry = 6,
      num.threads = 4,
      verbose = T
    )
  )
  #cv.ranger = crossval(learner = ranger_learner,
   #                    task = train_task,
    #                   iters = 5,
     #                  stratify = TRUE,
      #                 measures = acc,
       #                show.info = T)
  #ranger_model = mlr::train(ranger_learner, train_task)
  trc = trainControl(method="cv",number=5)
  ranger_model = caret::train(fmla,
                       data=tr,
                       method="ranger",
                       trControl=trc,
                       num.trees=600,
                       tuneGrid = data.frame(.mtry = 6,
                                             .splitrule = "gini",
                                             .min.node.size = 10))
  
  ts$pred <- predict(ranger_model, newdata=ts)
  tr$pred <- predict(ranger_model, newdata=tr)
  results =  data.frame(part, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
