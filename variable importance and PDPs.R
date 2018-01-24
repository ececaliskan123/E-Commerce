if(!require("caret")) install.packages("caret"); library("caret")
if(!require("mlr")) install.packages("mmlr"); library("mlr")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("pdp")) install.packages("pdp"); library("pdp")
source('load_data.R')
sales <- read_and_preprocess_data_file('data/BADS_WS1718_known.csv')

# Deciding on the significance of the item_size and item_color levels
sales2 <- subset(sales, select = c("item_size", "item_color", "return"))
# I selected a subset of the sales for speed gains
sales2 <- data.frame(sales2)
# Explicitly transform factor variables to dummy variables 
sales2 <- mlr::createDummyFeatures(sales2, target = "return")

# Splitting the data into a test and a training set 
idx.test <- createDataPartition(y = sales2$return, p = 0.2, list = FALSE) # Draw a random, stratified sample including p percent of the data
ts <-  sales2[idx.test, ] # test set
tr <- sales2[-idx.test, ] # training set

# Train simple logit model
logit <- glm(return~., data = tr,  family = binomial(link = "logit"))


# mlr random forest 
task <- makeClassifTask(data = tr, target = "return", positive = "1")

# use the optimal parameters found by Hyunnah, need update
rf.mlr <- makeLearner("classif.randomForest", predict.type = "prob", 
                      par.vals = list("replace" = TRUE, "importance" = TRUE, 
                                      "mtry" = 4, "sampsize" = 200, "ntree" = 1000))
rf <- mlr::train(rf.mlr, task = task)

# rf variable importance
featureImportance <- list()
getFeatureImportance(rf, type = 2)
featureImportance[["rf"]] <- unlist(getFeatureImportance(rf, type = 2)$res)
class(rf$learner.model)
# have a look at the mean decrease accuracy
importance(rf$learner.model)

# Xgboost variable importance
library("xgboost")

# use the optimal parameters found by Felix, need update
xgb.mlr <- makeLearner("classif.xgboost", predict.type = "prob", 
                       par.vals = list("nrounds" = 100, "verbose" = 0, "max_depth" = 4, "eta" = 0.15, 
                                       "gamma" = 0, "colsample_bytree" = 0.8, "min_child_weight" = 1, "subsample" = 0.8))
xgb <- mlr::train(xgb.mlr, task = task)

xgb.importance(model = xgb$learner.model, feature_names = colnames(task$env$data))
# Using caret's varImp() for variable importance for xgb
# 'Gain' in Gini purity
getFeatureImportance(xgb)
featureImportance[["xgb"]] <- unlist(getFeatureImportance(xgb)$res)

# Plot relative variable importance scaled from 0 to 100
# Compare the importance results from rf and xgb
maxMinStandardize <- function(x) ( (x - min(x)) / (max(x) - min(x)) ) * 100
importanceTable <- as.data.frame(sapply(featureImportance, maxMinStandardize, USE.NAMES = TRUE))
importanceTable[order(rowSums(importanceTable), decreasing = TRUE),]

# PDSs

# We can compute PDPs using function partial() 
library("pdp")

str(logit)

pdps <- list()
pdps[['logit']] <- partial(object = logit, # the model
                           pred.var = "return", # the variables of interest
                           prob = TRUE) # effect on raw output or probability?
str(pdps[["logit"]]) # We get the average predicted probabilty at each of the variable levels
# Plot the result
plot(pdps[['logit']], ylim = c(0,1), type = "l")

# The plot for the highly non-linear random forest should be more interesting
pdps[['rf']] <- partial(object = rf$learner.model, train = task$env$data, pred.var = "return", prob = TRUE)
lines(pdps[['rf']], col = "darkgreen")

# Note that {pdp} for classification problems displays the marginal effect 
# of a variable on the 'first' class Let's take a look at the
# random forest object to find out what class it considers as the first class
rf

# Specify the correct target class explicitely
pdps[['rf']] <- partial(object = rf$learner.model, train = task$env$data, pred.var = "return", prob = TRUE, which.class = 1)
lines(pdps[['rf']], col = "darkgreen")


pdps[['xgb']] <- partial(object = xgb$learner.model, train = task$env$data, pred.var = "return", prob = TRUE, which.class = 1)
lines(pdps[['xgb']], col = "blue")

