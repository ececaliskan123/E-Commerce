if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("klaR")) install.packages("klaR"); library("klaR")
if(!require("mlr")) install.packages("mlr"); library("mlr")


..standardize <- function(x){
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  
  return(result)
}

..mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

..static_user_statistics <- NULL
..read_and_preprocess_data_file = function(fp) {
  sales = read.csv(fp, stringsAsFactors = FALSE)
  
  sales$item_size [sales$item_size == "XS" ] <- "xs"
  sales$item_size [sales$item_size == "XL" ] <- "xl"
  sales$item_size [sales$item_size == "L" ] <- "l"
  sales$item_size [sales$item_size == "M" ] <- "m"
  sales$item_size [sales$item_size == "S" ] <- "s"
  sales$item_size [sales$item_size == "XXL" ] <- "xxl"
  sales$item_size [sales$item_size == "XXXL" ] <- "xxxl"
  sales$item_size [sales$item_size == "36" ] <- "s"
  sales$item_size [sales$item_size == "37" ] <- "s"
  sales$item_size [sales$item_size == "38" ] <- "m"
  sales$item_size [sales$item_size == "35" ] <- "xs"
  sales$item_size [sales$item_size == "38+" ] <- "m"
  sales$item_size [sales$item_size == "36+" ] <- "s"
  sales$item_size [sales$item_size == "37+" ] <- "s"
  sales$item_size [sales$item_size == "39+" ] <- "m"
  sales$item_size [sales$item_size == "39" ] <- "m"
  sales$item_size [sales$item_size == "40" ] <- "m"
  sales$item_size [sales$item_size == "40+" ] <- "m"
  sales$item_size [sales$item_size == "41" ] <- "l"
  sales$item_size [sales$item_size == "41+" ] <- "l"
  sales$item_size [sales$item_size == "42+" ] <- "l"
  sales$item_size [sales$item_size == "42" ] <- "l"
  sales$item_size [sales$item_size == "43" ] <- "l"
  sales$item_size [sales$item_size == "43+" ] <- "xl"
  sales$item_size [sales$item_size == "44+" ] <- "xl"
  sales$item_size [sales$item_size == "44" ] <- "xl"
  sales$item_size [sales$item_size == "45" ] <- "xl"
  sales$item_size [sales$item_size == "45+" ] <- "xl"
  sales$item_size [sales$item_size == "46" ] <- "xl"
  sales$item_size [sales$item_size == "46+" ] <- "xl"
  sales$item_size [sales$item_size == "47" ] <- "xxl"
  sales$item_size [sales$item_size == "48" ] <- "xxl"
  sales$item_size [sales$item_size == "50" ] <- "xxl"
  sales$item_size [sales$item_size == "34" ] <- "xs"
  sales$item_size [sales$item_size == "52" ] <- "xxxl"
  sales$item_size [sales$item_size == "54" ] <- "xxxl"
  sales$item_size [sales$item_size == "56" ] <- "xxxl"
  
  
  sales$user_dob <- substring(sales$user_dob,1,4)
  sales$user_dob [sales$user_dob == "?"] <- NA
  sales$user_dob [sales$user_dob < 1920 ] <-NA
  count<- table(sales$user_dob)
  MFV <- names(count) [count == max (count)]
  sales$user_dob [is.na(sales$user_dob)] <- MFV 
  sales$user_dob <- as.numeric(sales$user_dob)
  
  zScores_sales_dob <- ..standardize(sales$user_dob)
  sales$user_dob [zScores_sales_dob > 3] <- round(mean(sales$user_dob) + 3*sd(sales$user_dob), digit=0)
  sales$user_dob [zScores_sales_dob < -3] <- round(mean(sales$user_dob) - 3*sd(sales$user_dob), digit=0)
  
  # order_date and delivery_date / data cleansing
  sales$order_date <-   as.Date(sales$order_date, "%Y-%m-%d")
  sales$delivery_date <- as.Date(sales$delivery_date, "%Y-%m-%d")
  sales$delivery_duration<- difftime(sales$delivery_date , sales$order_date, units = c("days"))
  sales$delivery_duration [sales$delivery_duration < 0] <- NA
  sales$delivery_duration [sales$delivery_duration == "?"] <- NA
  sales$delivery_duration [is.na (sales$delivery_duration)] <- 2 
  sales$delivery_duration <- as.numeric(sales$delivery_duration) 
  
  # item-price / data cleansing
  sales$item_price <- as.numeric(sales$item_price)
  sort(table(sales$item_price), decreasing = TRUE) #MFV
  sales$item_price [is.na(sales$item_price) ] <- 59.9 

  zScores <- ..standardize(sales$item_price)
  sales$item_price [zScores > 3] <- round(mean(sales$item_price) + 3*sd(sales$item_price), digit=2)
  
  #user_reg_date / data cleansing
  sales$user_reg_date <-   as.Date(sales$user_reg_date, "%Y-%m-%d")
  sales$ user_maturity <- difftime(sales$order_date , sales$user_reg_dat, units = c("days"))
  sales$user_maturity <- as.numeric(sales$user_maturity) 
  
  #user_title / data cleansing
  sales$user_title [sales$user_title == "not reported"] <-NA
  sales$user_title [is.na(sales$user_title)] <- "Mrs"
  
  #item_color /  data cleansing
  sales$item_color [sales$item_color == "blau"] <- "blue"
  sales$item_color [sales$item_color == "brwon"] <- "brown"
  sales$item_color [sales$item_color == "oliv"] <- "olive"
  sales$item_color [sales$item_color == "?"] <- NA
  #sort(table(sales$item_color), decreasing = TRUE) ## MFV
  sales$item_color [is.na (sales$item_color)] <- "black"
  sales$user_dob <- as.numeric(sales$user_dob)
  
  # Month of Delivery
  sales$month_of_delivery <- substring(sales$delivery_date,6,7)
  sales$month_of_delivery [is.na(sales$month_of_delivery)] <- "01"
  sales$month_of_delivery = as.numeric(sales$month_of_delivery)
  
  # Factoring
  chrIdx <- which(sapply(sales, is.character))
  sales[, chrIdx] <- lapply( sales[, chrIdx],factor)
  sales$item_price <- as.numeric(sales$item_price)
  
  sales$price_and_age <- sales$item_price * sales$user_dob
  
  # Specify the 'keys' i.e. ID variables for additional speed gains when merging or sorting
  sales = data.table(sales)
  data.table::setkey(sales, user_id, item_id, order_item_id)
  
  if ("return" %in% colnames(sales)) {
    ..static_user_statistics <<- sales[, list("avg_return" = mean(return), "nr_obs" = .N), by = "user_id"]
  } else {
    avgrets = subset(..static_user_statistics, select = -c(nr_obs))
    df2 = subset(..static_user_statistics, select=-c(avg_return))
    df1 = sales[, list("nr_obs" = .N), by = "user_id"]
    
    # bind the two dataframes together by row and aggregate
    ..static_user_statistics <<- aggregate(cbind(nr_obs) ~ user_id, rbind(df1,df2), sum)
    # or (thx to @alistaire for reminding me):
    #..static_user_statistics <<- aggregate(. ~ user_id, rbind(df1,df2), sum)
    ..static_user_statistics = data.frame(merge(x = ..static_user_statistics, y = avgrets, by = "user_id", all.x = TRUE))
  }
  
  sales = data.frame(merge(x = sales, y = ..static_user_statistics, by = "user_id", all.x = TRUE))
  sales[is.na(sales$avg_return), "avg_return"] = ..mode(..static_user_statistics$avg_return)

  return(sales)
}

amend_features = function(dd){
  dd$order_year  = as.numeric(format(dd$order_date, "%Y"))
  dd$order_month = as.numeric(format(dd$order_date, "%m"))
  dd$order_day   = as.numeric(format(dd$order_date, "%d"))
  dd             = subset(dd, select=-order_date)
  
  dd$reg_year  = as.numeric(format(dd$user_reg_date, "%Y"))
  dd$reg_month = as.numeric(format(dd$user_reg_date, "%m"))
  dd$reg_day   = as.numeric(format(dd$user_reg_date, "%d"))
  dd           = subset(dd, select=-user_reg_date)
  
  dd$del_year  = as.numeric(format(dd$delivery_date, "%Y"))
  dd$del_month = as.numeric(format(dd$delivery_date, "%m"))
  dd$del_day   = as.numeric(format(dd$delivery_date, "%d"))
  dd           = subset(dd, select=-delivery_date)
  dd$return    = as.factor(dd$return)
  
  if("return" %in% colnames(dd)) {
    dd = normalizeFeatures(dd, target="return")
  }
  
  return(dd)
}

df_known = ..read_and_preprocess_data_file('data/BADS_WS1718_known.csv')
df_known$return = factor(df_known$return)

woe.object <- woe(return ~ item_size + item_color + user_title + user_state,
                  data = df_known,
                  zeroadj = 0.5)

df_known$user_state = woe.object$woe$user_state[df_known$user_state]
df_known$user_title = woe.object$woe$user_title[df_known$user_title]
df_known$item_size  = woe.object$woe$item_size[df_known$item_size]
df_known$item_color = woe.object$woe$item_color[df_known$item_color]

df_known = amend_features(df_known)

localH2O = h2o.init(ip='localhost',
                    nthreads=-1,
                    min_mem_size='4G',
                    max_mem_size='5G') # with system memory of 8GB
train_task = makeClassifTask(data = df_known, target = "return", positive = 1)
rf_learner = makeLearner(
  "classif.h2o.randomForest",
  predict.type = "prob",
  par.vals = list(
    ntrees   = 241,
    mtries   = 8,
    min_rows = 47
  )
)
cv.ranger = crossval(learner   = rf_learner,
                     task      = train_task,
                     iters     = 5,
                     stratify  = TRUE,
                     measures  = acc,
                     show.info = T)
rf_model = mlr::train(rf_learner, train_task)
importance = h2o.varimp(rf_model$learner.model)
