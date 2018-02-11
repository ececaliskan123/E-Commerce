#
source('load_data.R')
install.packages("Information")
library(Information)
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("klaR")) install.packages("klaR"); library("klaR")
sales <- read_and_preprocess_data_file('data/BADS_WS1718_known.csv')

# Reducing dimensionality for sizes and colors

str(sales)
set.seed(123)
idx.sales <- createDataPartition(y = sales$return, p = 0.75, list = FALSE) # Draw a random, stratified sample including p percent of the data
test <-  sales[-idx.sales, ] # test set
sales <- sales[idx.sales, ] # salesing set

tapply(sales$item_color, sales$return, summary)

sales$item_color <- factor(sales$item_color)
sales$item_size <- factor(sales$item_size)
sales$return <- factor(sales$return)
sales$user_title <- factor(sales$user_title)
sales$user_state <- factor(sales$user_state)
sales$month_of_delivery <- factor(sales$month_of_delivery)

#WOE

# Use function woe() to calculate the weight of evidence for each factor level
woe.object <- woe(return ~ item_size + item_color, data = sales, zeroadj = 0.5)
# Note that you will see a warning message telling you about empty cells 
# It is safe to ignore these messages as we set the parameter  

# The weights for each factor level are saved in list element 'woe'
woe.object$woe
summary(woe.object$xnew)

test$return <- factor(test$return)
# Need to drop the new levels in the test data
missingLevelsToNA<-function(object,data){
  
  #Obtaining factors in the model and their levels
  
  factors<-(gsub("[-^0-9]|as.factor|\\(|\\)", "",names(unlist(object$xlevels))))
  factorLevels<-unname(unlist(object$xlevels))
  modelFactors<-as.data.frame(cbind(factors,factorLevels))
  
  #Select column names of the factors
  
  predictors<-names(data[names(data) %in% factors])
  
  #For each factor level that doesn't exist in the original data set; set the value to NA
  
  for (i in 1:length(predictors)){
    found<-data[,predictors[i]] %in% modelFactors[modelFactors$factors==predictors[i],]$factorLevels
    if (any(!found)) data[!found,predictors[i]]<-NA
  }
  
  data
  
}
# Use the Level drop code above and make sure sales and test are data frames.

test <- cbind(test[,"item_size" ],test [,"item_color"])
test$item_color <- factor(test$item_color)
test$item_size <- factor(test$item_size)
test <- data.frame(test)
sales <- data.frame(sales)
pred_model <- glm(return ~ item_size + item_color ,data = sales,family = binomial(link = "logit"))
test <- missingLevelsToNA(pred_model,test)
test$item_color [is.na(test$item_color) ]  <- "black"
test$item_size [is.na(test$item_size)] <- "m"
test <- data.frame(test)

test.woe <- predict(woe.object, newdata = test, replace = TRUE)
#summary(test.woe)

# Check for plausibility by plotting the weights against their levels
# Remember that the direction is switched
barplot(-1 * woe.object$woe$item_color)

barplot(-1 * woe.object$woe$item_size)

#Information Value

# using create_infotables from information package.
# return should be binary integer
IV <- create_infotables(data=sales, y="return", bins=10, parallel=TRUE)
IV_Value = data.frame(IV$Summary)
print(IV$Tables$item_color, row.names=TRUE)
print(IV$Tables$item_size, row.names=TRUE)
plot_infotables(IV, "item_color")
plot_infotables(IV, "item_size")

# for multiple charts
#plot_infotables(IV, IV$Summary$Variable[2:3], same_scale=FALSE)

# As a rule of thumb: <0.02: not predictive, 0.02-0.1: weak, 0.1-0.3: medium, >0.3: strong
