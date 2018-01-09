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
h2oServer<-h2o.init(nthreads = -1)

# Load required packages if necessary
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("ModelMetrics")) install.packages("ModelMetrics"); library("ModelMetrics")
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("data.table")) install.packages("data.table"); library("data.table")

# Now we load our dataset and process it by means of our helper function
source("load_data.R")

retail<-read_and_preprocess_data_file("data/BADS_WS1718_known.csv")
retail_class<-read_and_preprocess_data_file("data/BADS_WS1718_class.csv")

# Convert target variable into a factor
retail$return<-factor(retail$return, levels = c(0,1), labels = c("No Return","Return"))

# Creating training and test data set
set.seed(123)
idx.train<-createDataPartition(y = retail$return, p = 0.8, list = FALSE)
tr<-retail[idx.train,] # is the unstandardized training set
ts<-retail[-idx.train,] # is the unstandardized test set

normalizer<-caret::preProcess(tr, method = c("center", "scale"))
tr<-predict(normalizer, newdata = tr) # is the standardized training set
ts<-predict(normalizer, newdata = ts) # is the standardized test set

retail_class<-predict(normalizer, newdata = retail_class)

# Save training and testing set as csv.files and specify data paths to prepare data import
write.csv(tr, file = "data/tr_norm.csv")
write.csv(ts, file = "data/ts_norm.csv")
tr_path<-"C:/Users/Sledge Hammer/Documents/badass-group/data/tr_norm" # Please specify local directory
ts_path<-"C:/Users/Sledge Hammer/Documents/badass-group/data/ts_norm" # Please specify local directory

# Import the data into h2o cluster
tr<-h2o.importFile(tr_path)
ts<-h2o.importFile(ts_path)

# Specify the response and predictor columns
y<-"return"
x<-c("user_dob", "user_maturity", "delivery_duration", "user_state", "item_price", "item_color", "item_size", "month_of_delivery")

# Perform hyperparameter tuning with k-fold cross validation
hyper_params <- list(
  hidden = list(c(100, 100, 100), c(200, 200, 200), c(300, 300, 300)),
  input_dropout_ratio = c(0, 0.05),
  epochs = c(10, 100, 1000),
  l1 = c(1e-4, 1e-3, 1e-2), 
  activation = c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
)

# Grid search
grid_id <- "dl_grid"
model_dl_grid <- h2o.grid(
  algorithm = "deeplearning", # Name of the algorithm 
  grid_id = grid_id, 
  training_frame = tr,
  nfolds = 5,
  x = x, 
  y = y,
  stopping_metric = "AUC",
  stopping_tolerance = 1e-2, # stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds = 2,
  # score_validation_samples = 10000, # downsample validation set for faster scoring
  max_runtime_secs = 600,
  hyper_params = hyper_params
)

# Identification of best model and its performance
stopping_metric <- 'AUC'
sorted_models <- h2o.getGrid(
  grid_id = grid_id, 
  sort_by = stopping_metric,
  decreasing = TRUE
)
best_model <- h2o.getModel(sorted_models@model_ids[[1]])
pred.h2o <- h2o.predict(best_model, ts)
model.perf<-h2o.performance(best_model, newdata = ts)
model.perf@metrics$AUC

# Saving optimal nnet model
saveRDS(best_model, file = "models/H2O_Model.R") 

# Predictions of tuned model for unknown data
pred_h2o_final<-h2o.predict(best_model, retail_class)

# Saving h2o class predictions
write.csv(pred_h2o_final$predict, file = "data/H2o_Predictions_Class.csv")

# Saving h2o probability predictions
write.csv(pred_h2o_final$Return, file = "data/H2o_Predictions_Prob.csv")