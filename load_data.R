# PLEASE COPY THE CONTENTS OF THIS FILE AND FEEL FREE TO IMPROVE

standardize <- function(x){
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std

  return(result)
}

# This function reads in the CSV file located at fp
# and returns a dataframe where all the unwanted values have been replaced
read_and_preprocess_data_file = function(fp) {
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

  zScores_sales_dob <- standardize(sales$user_dob)
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
  #boxplot(sales$item_price)
  zScores <- standardize(sales$item_price)
  sales$item_price [zScores > 3] <- round(mean(sales$item_price) + 3*sd(sales$item_price), digit=2)
  #boxplot(sales$item_price)
  
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
  
  # A new feature 
  sales$price_and_age <- sales$item_price * sales$user_dob
  
  # commented since the return column does not exist yet
  #sales$return <- factor(sales$return, labels = c("keep","return"))
  return(sales)
}

# Specify the 'keys' i.e. ID variables for additional speed gains when merging or sorting
setkey(raw, user_id, item_id, order_item_id)

# Splitting the data into a test and a training set 
idx.train <- caret::createDataPartition(y = raw$return, p = 0.8, list = FALSE) # Draw a random, stratified sample including p percent of the data

# Use data.table to calculate grouped summary statistics efficiently
customers <- raw[ ,  mean(return), by = "user_id"]
# Every piece of information could be relevant, here for example the number of times a customer came back
customers <- raw[ , list("avg_return" = mean(return), "nr_obs" = .N), by = "user_id"]
# Careful: When using the target variable as a feature, only calculate it on the training data
# You can merge data tables X and Y using the syntax X[Y]
raw <- raw[ raw[idx.train, list("avg_return" = mean(return), "nr_obs" = .N), by = "user_id"]]

# Tackle cleaning and featurization problems with data science and statistics
length(unique(raw$item_size))
unique(raw$item_size)

sizes <- raw[, list("count" = .N), by = c("user_id", "item_size") ]
sizes_table <- dcast(sizes, user_id ~ item_size, value.var = "count", fill = 0)
sizes_sim <- cor(sizes_table[,2:length(sizes_table)])
#sizes_sim <- dist(sizes_table[,2:length(sizes_table)], method = "cosine")
sizes_sim[c("S", "M", "L"),]
sizes_cluster <- hclust(as.dist(1-abs(sizes_sim)), method = "ward.D2")
plot(sizes_cluster)
rect.hclust(sizes_cluster, k=9, border="red")
# Use the size cluster instead of or in addition to the original sizes
sizes_grouping <- cutree(sizes_cluster, k=5) # cut tree into 5 clusters

