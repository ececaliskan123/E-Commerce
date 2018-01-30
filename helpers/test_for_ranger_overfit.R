### Random forest
library(ranger)
library(dplyr)
library(caret)

source('load_data.R')

n = nrow(df_known)

test.results <- data.frame(matrix(ncol = 3, nrow = 0))
rn <- c("tr_size", "ts_acc", "tr_acc")
colnames(test.results) = rn

for (part in seq(0.05,0.8,0.05)) {
  set.seed(1)
  idx.train <- createDataPartition(y=df_known$return, p=part, list = FALSE) 
  tr <- df_known[idx.train, ]  # training set
  ts <- df_known[-idx.train, ] # test set 
  
  return <- "return"
  vars <- c("user_id", "order_item_id", "order_date","month_of_delivery", "price_and_age",
            "item_id", "item_size", "brand_id", "item_price", "user_dob", "user_reg_date", "delivery_duration")
  fmla <- paste(return, "~", paste(vars, collapse = " + "))
  
  df_known_rfmodel_CV <- ranger(fmla, # formula
                                tr,  #data
                                num.trees = 500,
                                mtry = 6,
                                num.threads = 1,
                                verbose = FALSE,
                                respect.unordered.factors = TRUE)
  
  ts$pred <- ifelse(predict(df_known_rfmodel_CV, ts)$predictions > 0.5, 1, 0)
  tr$pred <- ifelse(predict(df_known_rfmodel_CV, tr)$predictions > 0.5, 1, 0)
  results =  data.frame(part, mean(ts$pred == ts$return), mean(tr$pred == tr$return))
  colnames(results) = rn
  test.results = rbind(test.results,results)
}

ggplot(test.results, aes(tr_size)) +                    # basic graphical object
  geom_line(aes(y=tr_acc), colour="red") +  # first layer
  geom_line(aes(y=ts_acc), colour="green")  # 
