if(!require("parallel")) install.packages("parallel"); library("parallel")
if(!require("caret")) install.packages("caret"); library("caret")

#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
d = subset(d, select = -c(delivery_date)) # remove NAs
d$return = factor(d$return)

# 10-times cross validation
#tc = tune.control(sampling = "cross")
#best_tuned_svm = best.tune(svm,
#                      train.x = return ~ .,
#                      kernel="radial",
#                      data = d,
#                      traincontrol = tc,
#                      ranges=list(cost=10^(-2:1), gamma=2^(-2:2)))
#best_tuned_svm = svm(return ~ . -return,
#                     data = d,
#                     kernel = "radial")
set.seed(3233)
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
radsvm = train(return ~ . -return,
                data = d,
                method = "svmRadial",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)

predictions = predict(radsvm, newdata = d)
d.result = data.frame(d$order_item_id, predictions)
names(d.result) = c("order_item_id", "return")

save(radsvm, file = "models/svm.model")
write.csv(d.result, "data/svm_predictions_known.csv", row.names = FALSE)
