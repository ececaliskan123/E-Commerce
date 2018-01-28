# mlr with h2o, 5 fold CV

if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("car")) install.packages("car"); library("car")

amend_features = function(dd){
  dd = subset(dd, select = -c(delivery_date))
  dd = subset(dd, select = -c(user_dob, user_maturity, user_title, user_state, item_color, item_price, item_size))
  # Nicolai added the feature item_price and item_size to the previous line due to low RF variable importance scores
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
    # dd = createDummyFeatures(dd, target="return", cols=c("item_size"))
  } else {
    # dd = createDummyFeatures(dd, cols=c("item_size"))
  }
  # Nicolai deselected createDummyFeatures due to the deletion of fetures item_size and item_price (see above)
  return(dd)
}

# setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
#d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
#classdata = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')

### TODO AMEND BLOCK AFTER FEATURE ENGINEERING
dn = amend_features(df_known)
###############################################
set.seed(1)
n = nrow(dn)
ratio = sum(dn$return)/n
dn$random_class <- rbinom(n, 1, 0.5)

### TODO Robustness check wrt feature "avg_return" with VIF test
model <- glm (randorm_class ~ ., data = dn, family = binomial)

