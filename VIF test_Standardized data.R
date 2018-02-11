## VIF test for multicollinearity with standardized data
 
if(!require("mlr")) install.packages("mlr"); library("mlr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("car")) install.packages("car"); library("car")

# load helper to preprocess data and select fetures
source('helpers/amend_features.R')
source('helpers/amend_features_beta.R')
# load data
source('load_data.R')

### Process known dataset
dn = amend_features(df_known)
dn$return = as.numeric(as.character(dn$return))
dn2 = amend_features_beta(df_known)
##############################################
### Look at correlation to see whether multicollinearity could be present
cor_raw <- round(cor(dn), 3)
cor_raw
write.csv(cor_raw, file = "data/correlation_table.csv")

dn_na <- na.exclude(dn2)
dn_na$return <- as.numeric(as.character(dn_na$return))
dn_na <- subset(dn_na, select = -c(order_date, delivery_date, user_reg_date))

cor_raw2 <- round(cor(dn_na), 3)
cor_raw2
write.csv(cor_raw2, file = "data/correlation_table_raw.csv")


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
