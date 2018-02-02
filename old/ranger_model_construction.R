#### random forest with ranger and caret

if(!require("ranger")) install.packages("ranger"); library("ranger")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
# load data
source('load_data.R')
dn = amend_features(df_known)
dn[is.na(dn)] = 0
classdatan = amend_features(df_class)

###############################################
set.seed(1)

idx.train = caret::createDataPartition(y = dn$return, p = 0.8, list = FALSE) 
tr = dn[idx.train, ]
ts = dn[-idx.train, ]

# From Parameter tuning file
n <- nrow(dn) 
# The return column
return <- "return"
# The input variables
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
# Create the formula string for returns as a function of the inputs
fmla <- paste(return, "~", paste(vars, collapse = " + "))
# Fit and print the random forest model
# Hyperparameters with 5 folds CV (done by mlr)
(df_known_rfmodel_CV <- ranger(fmla, # formula
                           tr,  #data
                           num.trees = 500,
                           mtry = 6,
                           num.threads = 1,
                           verbose = FALSE,
                           respect.unordered.factors = TRUE
                          ))
# Make predictions on the ts data
ts$pred <- predict(df_known_rfmodel_CV, ts)$predictions
#Prediction for dn1
dn$pred <- predict(df_known_rfmodel_CV, dn)$predictions
a <- which(is.na(dn$delivery_date))
dn$pred[a] <- 0 
dn <- subset(dn, select = c(order_item_id, pred))
dn$order_item_id <- df_known$order_item_id
csv <- write.csv(dn, 'old/new_ranger_known.csv', row.names = FALSE)
# Saving optimal rf model
save(df_known_rfmodel_CV, file = "old/rf_ranger.model") 

# Accuracy : 96.38%
prob.pred_known  = as.vector(dn$pred)
class.pred_known  = ifelse(prob.pred_known > 0.5, "1", "0")
confusionMatrix(data = class.pred_known, reference = df_known$return, positive = "1")

#### Prediction
library(readr)
df_class_amended<-amend_features(df_class)
df_class_amended[is.na(df_class_amended)] = 0
df_class_amended$pred <- predict(df_known_rfmodel_CV, df_class_amended)$predictions
b <- which(is.na(df_class$delivery_date))
df_class_amended$pred[b] <- 0
df_class_amended$pred <- as.numeric(as.character(df_class_amended$pred))
df_class_amended$return <- ifelse(df_class_amended$pred > 0.5, 1, 0)
table(df_class_amended$return)
# Select only 2 columns for csv files(probability, class)
df_class_amended1 <- subset(df_class_amended, select = c(order_item_id, pred))
csv1 <- write.csv(df_class_amended1, "old/ranger_class_probability.csv")
df_class_amended2 <- subset(df_class_amended, select = c(order_item_id, return))
csv2 <- write.csv(df_class_amended2, "old/ranger_class.csv")
###








