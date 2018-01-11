if(!require("parallel")) install.packages("parallel"); library("parallel")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("e1071")) install.packages("e1071"); library("e1071")

#setwd("/mnt/learning/business-analytics-data-science/groupwork/")
source('load_data.R')
d = read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
d = subset(d, select = -c(delivery_date)) # remove NAs
d$return = factor(d$return)

classdata = read_and_preprocess_data_file('data/BADS_WS1718_class.csv')
classdata = subset(classdata, select = -c(delivery_date)) # remove NAs

# 10-times cross validation
set.seed(3233)
tc = tune.control(sampling = "cross")
radsvm = best.tune(svm,
		   train.x = return ~ .,
		   kernel="radial",
		   data = d,
		   traincontrol = tc,
		   ranges=list(cost=10^(-2:1), gamma=2^(-2:2)))
#radsvm = svm(return ~ . -return,
#                     data = d,
#                     kernel = "radial")
#trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

#radsvm = train(return ~ . -return,
#                data = d,
#                method = "svmRadial",
#                trControl=trctrl,
#                preProcess = c("center", "scale"),
#                tuneLength = 10)

predictions = predict(radsvm, newdata = d)
d.result = data.frame(d$order_item_id, predictions)
names(d.result) = c("order_item_id", "return")

predicted_class = predict(bst, newdata = classdata) 
classdata.result = data.frame(classdata$order_item_id, predicted_class)
names(classdata.result) = c("order_item_id", "return")

save(radsvm, file = "models/svm.model")
write.csv(d.result, "data/svm_known.csv", row.names = FALSE)
write.csv(classdata.result, "data/svm_class", row.names = FALSE)
