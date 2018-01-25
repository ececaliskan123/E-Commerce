source('load_data.R')

d = read.csv('data/BADS_WS1718_known.csv')
n = nrow(d)
ratio = sum(d$return) / n

set.seed(1)
accuracy = 100000
randomized_returns = sample(accuracy, n, replace=TRUE)/accuracy
randomized_returns = ifelse(randomized_returns > ratio, 1, 0)

d$randomized_return <- randomized_returns

### Randomized return cost
retail2$pred_randomized_prob <- d$randomized_return

retail2$pred_randomized_prob <- as.numeric(retail2$pred_randomized_prob)

retail2$missclassification_randomized <- retail2$return2 - retail2$pred_randomized_prob
retail2$cost_randomized<- retail2$return2 - retail2$pred_randomized_prob

# Calculation the cost for cost matrix
g <- which(retail2$missclassification_randomized == 1)
retail2$cost_randomized[g] <- 0.5*5*{3+(0.1*retail2$item_price[g])} 
h <- which(retail2$missclassification_randomized == -1)
retail2$cost_randomized[h] <- 0.5*(retail2$item_price[h]) 

# Sum of the cost
randomized_cost_sum <- sum(retail2$cost_randomized)
randomized_cost_sum


### Random Forest cost
prob.pred_known  <- as.vector(retail$pred2)
retail$pred_class <- ifelse(prob.pred_known > 0.5, "1", "0")

retail2 <- subset(retail, select = c(order_item_id, item_price, return2, pred_class))
head(retail2)

retail2$return2 <- as.numeric(retail2$return2)
retail2$pred_class <- as.numeric(retail2$pred_class)

retail2$missclassification_rf <- retail2$return2 - retail2$pred_class
retail2$cost_rf<- retail2$return2 - retail2$pred_class

# Calculation the cost for cost matrix
a <- which(retail2$missclassification_rf == 1)
retail2$cost_rf[a] <- 2.5*{3+(0.1*retail2$item_price[a])} 
b <- which(retail2$missclassification_rf == -1)
retail2$cost_rf[b] <- 0.5*(retail2$item_price[b]) 

# Sum of the cost
rf_cost_sum <- sum(retail2$cost_rf)
rf_cost_sum

### NN cost
retail2$pred_nn_prob <- nnet_known$return
prob.pred_nn_known  <- as.vector(retail2$pred_nn_prob)
retail2$pred_nn_prob <- ifelse(prob.pred_nn_known > 0.5, "1", "0")

retail2$pred_nn_prob <- as.numeric(retail2$pred_nn_prob)

retail2$missclassification_nn <- retail2$return2 - retail2$pred_nn_prob
retail2$cost_nn<- retail2$return2 - retail2$pred_nn_prob

# Calculation the cost for cost matrix
c <- which(retail2$missclassification_nn == 1)
retail2$cost_nn[c] <- 0.5*5*{3+(0.1*retail2$item_price[c])} 
d <- which(retail2$missclassification_nn == -1)
retail2$cost_nn[d] <- 0.5*(retail2$item_price[d]) 

# Sum of the cost
nn_cost_sum <- sum(retail2$cost_nn)
nn_cost_sum

### xgboost cost
retail2$pred_xg_prob <- xgboost_known$return
prob.pred_xg_known  <- as.vector(retail2$pred_xg_prob)
retail2$pred_xg_prob <- ifelse(prob.pred_xg_known > 0.5, "1", "0")

retail2$pred_xg_prob <- as.numeric(retail2$pred_xg_prob)

retail2$missclassification_xg <- retail2$return2 - retail2$pred_xg_prob
retail2$cost_xg<- retail2$return2 - retail2$pred_xg_prob

# Calculation the cost for cost matrix
e <- which(retail2$missclassification_xg == 1)
retail2$cost_xg[e] <- 0.5*5*{3+(0.1*retail2$item_price[e])} 
f <- which(retail2$missclassification_xg == -1)
retail2$cost_xg[f] <- 0.5*(retail2$item_price[f]) 

# Sum of the cost
xg_cost_sum <- sum(retail2$cost_xg)
xg_cost_sum


#rf cost sum : 315473.1 
#nn cost sum : 1662204
#xgboost cost sum : 848712.4
#randomized cost sum : 1477106










