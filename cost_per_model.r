source('load_data.R')
df_known <- subset(df_known, select = c(item_id, item_price, return))
rf_known <- read.csv('data/randomforest_known.csv')
nnet_known <- read.csv('data/nnet_known.csv')
xgboost_known <- read.csv('data/xgboost_known.csv')

### Randomized return cost 
n = nrow(df_known)
ratio = sum(df_known$return) / n

set.seed(1)
accuracy = 100000
randomized_returns = sample(accuracy, n, replace=TRUE)/accuracy
randomized_returns = ifelse(randomized_returns > ratio, 1, 0)

df_known$randomized_return <- randomized_returns
df_known$missclassification_randomized <- df_known$return - df_known$randomized_return
df_known$cost_randomized <- df_known$return - df_known$randomized_return

# Calculation the cost for cost matrix
g <- which(df_known$missclassification_randomized == 1)
df_known$cost_randomized[g] <- 0.5*5*{3+(0.1*df_known$item_price[g])} 
h <- which(df_known$missclassification_randomized == -1)
df_known$cost_randomized[h] <- 0.5*(df_known$item_price[h]) 

# Sum of the cost
randomized_cost_sum <- sum(df_known$cost_randomized)
randomized_cost_sum #1473405 


### Random Forest cost
df_known$pred_rf_prob <- rf_known$pred
prob.pred_rf_known  <- as.vector(df_known$pred_rf_prob)
df_known$pred_rf_prob <- ifelse(prob.pred_rf_known > 0.5, "1", "0")

df_known$pred_rf_prob <- as.numeric(df_known$pred_rf_prob)

df_known$missclassification_rf <- df_known$return - df_known$pred_rf_prob
df_known$cost_rf<- df_known$return - df_known$pred_rf_prob

# Calculation the cost for cost matrix
a <- which(df_known$missclassification_rf == 1)
df_known$cost_rf[a] <- 2.5*{3+(0.1*df_known$item_price[a])} 
b <- which(df_known$missclassification_rf == -1)
df_known$cost_rf[b] <- 0.5*(df_known$item_price[b]) 

# Sum of the cost
rf_cost_sum <- sum(df_known$cost_rf)
rf_cost_sum #243997.7

### NN cost
df_known$pred_nn_prob <- nnet_known$return
prob.pred_nn_known  <- as.vector(df_known$pred_nn_prob)
df_known$pred_nn_prob <- ifelse(prob.pred_nn_known > 0.5, "1", "0")

df_known$pred_nn_prob <- as.numeric(df_known$pred_nn_prob)

df_known$missclassification_nn <- df_known$return - df_known$pred_nn_prob
df_known$cost_nn<- df_known$return - df_known$pred_nn_prob

# Calculation the cost for cost matrix
c <- which(df_known$missclassification_nn == 1)
df_known$cost_nn[c] <- 0.5*5*{3+(0.1*df_known$item_price[c])} 
d <- which(df_known$missclassification_nn == -1)
df_known$cost_nn[d] <- 0.5*(df_known$item_price[d]) 

# Sum of the cost
nn_cost_sum <- sum(df_known$cost_nn)
nn_cost_sum

### xgboost cost
df_known$pred_xg_prob <- xgboost_known$return
prob.pred_xg_known  <- as.vector(df_known$pred_xg_prob)
df_known$pred_xg_prob <- ifelse(prob.pred_xg_known > 0.5, "1", "0")

df_known$pred_xg_prob <- as.numeric(df_known$pred_xg_prob)

df_known$missclassification_xg <- df_known$return - df_known$pred_xg_prob
df_known$cost_xg <- df_known$return - df_known$pred_xg_prob

# Calculation the cost for cost matrix
e <- which(df_known$missclassification_xg == 1)
df_known$cost_xg[e] <- 0.5*5*{3+(0.1*df_known$item_price[e])} 
f <- which(df_known$missclassification_xg == -1)
df_known$cost_xg[f] <- 0.5*(df_known$item_price[f]) 

# Sum of the cost
xg_cost_sum <- sum(df_known$cost_xg)
xg_cost_sum #508233.6











