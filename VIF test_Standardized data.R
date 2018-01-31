## VIF test for multicollinearity with standardized data

if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("car")) install.packages("car"); library("car")

amend_features = function(dd){
  # dd = subset(dd, select = -c(delivery_date))
  # dd = subset(dd, select = -c(user_dob, user_maturity, user_title, user_state, item_color, item_price, item_size))
  # Added the features item_price and item_size to the previous line due to low RF variable importance scores
  dd$order_year  = as.numeric(format(dd$order_date, "%Y"))
  dd$order_month = as.numeric(format(dd$order_date, "%m"))
  dd$order_day   = as.numeric(format(dd$order_date, "%d"))
  dd             = subset(dd, select=-order_date)
  
  dd$reg_year  = as.numeric(format(dd$user_reg_date, "%Y"))
  dd$reg_month = as.numeric(format(dd$user_reg_date, "%m"))
  dd$reg_day   = as.numeric(format(dd$user_reg_date, "%d"))
  dd           = subset(dd, select=-user_reg_date)
  
  dd$del_year  = as.numeric(format(dd$delivery_date, "%Y"))
  dd$del_month = as.numeric(format(dd$delivery_date, "%m"))
  dd$del_day   = as.numeric(format(dd$delivery_date, "%d"))
  dd           = subset(dd, select=-c(delivery_date, month_of_delivery))
  
  dd$return = as.numeric(as.character(dd$return))
  
    if("return" %in% colnames(dd)) {
    dd = normalizeFeatures(dd, target="return")
    # dd = createDummyFeatures(dd, target="return", cols=c("item_size"))
  } else {
    # dd = createDummyFeatures(dd, cols=c("item_size"))
  }
  # Deselected createDummyFeatures due to the deletion of features item_size and item_price (see above)
  return(dd)
}

source('load_data.R')

### Process known dataset
dn = amend_features(df_known)
##############################################
### Look at correlation to see whether multicollinearity could be present
round(cor(dn), 2)
# As some variables are highly correlated, proceed with VIF test
dn$return <- as.factor(dn$return)
### Create regression model that follows our specification for prediction to test for multicollinearity
model_raw <- glm (return ~ ., data = dn, family = binomial)

### Calculate VIF for model1
vif_test_raw <- vif(model_raw)
vif_test_raw
save(vif_test_raw, file = "data/vif_test_raw")
# Multiple instances of multicollinearity

## Repeat test iteratively, reducing the collection of features starting with the combination of VIF in descending and variable importance score in ascending order
dn<-subset(dn, select = -c(reg_day, reg_month, reg_year))
model_reg_date <- glm (return ~ ., data = dn, family = binomial)
vif(model_reg_date)
# Two instances of multicollinearity and one of near multicollinearity (order_year). The latter also has the second lowest variable importance score of all features

## Repeat test after deleting item_price, which has a lower variable importance than price_and_age, and order_year
dn<-subset(dn, select = -c(item_price, order_year))
model_reg_price_order_year <- glm (return ~ ., data = dn, family = binomial)
vif(model_reg_price_order_year)

## For purposes of final variable selection, we also delte user_title and user_state due to low variable importance
dn<-subset(dn, select = -c(user_title, user_state))
model_final <- glm (return ~ ., data = dn, family = binomial)
vif(model_final)
save(vif_test_raw, file = "data/vif_test_final")
## Save list of final features
final_feature_list <- list()
final_feature_list <- colnames(dn)
final_feature_list
save(final_feature_list, file = "data/final_feature_list")
