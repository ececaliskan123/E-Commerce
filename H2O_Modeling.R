# Neural network with package h2o

getwd()

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wheeler/2/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()

# Load required packages if necessary
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("pROC")) install.packages("pROC"); library("pROC")

# Now we start a local H2O cluster with 1GB RAM
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

# Now we load our dataset and process it by means of our helper function
source("load_data.R")

retail<-read_and_preprocess_data_file("data/BADS_WS1718_known.csv")

class(retail$return)
head(retail$return)
retail$return<-factor(retail$return)
levels(retail$return)
head(retail$return)

set.seed(123)

# Creating training and test data set
idx.train<-createDataPartition(y = retail$return, p = 0.8, list = FALSE)
tr<-retail[idx.train,] # is the unstandardized training set
ts<-retail[-idx.train,] # is the unstandardized test set

normalizer<-caret::preProcess(tr, method = c("center", "scale"))
tr<-predict(normalizer, newdata = tr) # is the standardized training set
ts<-predict(normalizer, newdata = ts) # is the standardized test set

tr_h2o <- as.h2o(localH2O, tr, key = "dat")
ts_h2o <- as.h2o(localH2O, ts, key = "dat")

# Training our model

nn_h2o <- h2o.deeplearning(x = c(5, 6, 8, 11, 12, 15, 16, 17)  # column numbers for predictors
                   y = 14,   # column number for label
                   data = tr_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

# Testing the model
h2o_yhat_test <- h2o.predict(nn_h2o, ts_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)