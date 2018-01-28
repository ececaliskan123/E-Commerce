# VIF test for multicollinearity between "return" and "avg_return"

if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("car")) install.packages("car"); library("car")

amend_features = function(dd){
  dd = subset(dd, select = -c(delivery_date))
  dd = subset(dd, select = -c(user_dob, user_maturity, user_title, user_state, item_color, item_price, item_size))
  # Added the features item_price and item_size to the previous line due to low RF variable importance scores
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
  # Deselected createDummyFeatures due to the deletion of features item_size and item_price (see above)
  return(dd)
}

source('load_data.R')

### Process known dataset
dn = amend_features(df_known)
###############################################

### Assign random target variable
set.seed(1)
n = nrow(dn)
ratio = sum(dn$return)/n
dn$random_class <- rbinom(n, 1, ratio)
table(dn$random_class)

### Look at correlation to see whether multicollinearity could be present
round(cor(dn), 2)
# As some variables are highly correlated, proceed with VIF test

### Create regression model with random target and all other variables as features
model <- glm (random_class ~ ., data = dn, family = binomial)

### Calculate VIF
vif(model)
# No VIF > 10
## Decision: Keep all variables