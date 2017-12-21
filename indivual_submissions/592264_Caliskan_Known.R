setwd("~/Desktop/Assignment_BADS_WS1718")
# caliskae@hu-berlin.de

# Data read / Data cleansing

retail <- read.csv2("BADS_WS1718_known.csv", sep = ",", stringsAsFactors = FALSE)

# Item-size conversion
retail$item_size [retail$item_size == "XS" ] <- "xs"
retail$item_size [retail$item_size == "XL" ] <- "xl"
retail$item_size [retail$item_size == "L"  ] <- "l"
retail$item_size [retail$item_size == "M"  ] <- "m"
retail$item_size [retail$item_size == "S"  ] <- "s"
retail$item_size [retail$item_size == "XXL"] <- "xxl"
retail$item_size [retail$item_size == "XXXL"]<- "xxxl"
retail$item_size [retail$item_size == "36" ] <- "s"
retail$item_size [retail$item_size == "37" ] <- "s"
retail$item_size [retail$item_size == "38" ] <- "m"
retail$item_size [retail$item_size == "35" ] <- "xs"
retail$item_size [retail$item_size == "38+"] <- "m"
retail$item_size [retail$item_size == "36+"] <- "s"
retail$item_size [retail$item_size == "37+"] <- "s"
retail$item_size [retail$item_size == "39+"] <- "m"
retail$item_size [retail$item_size == "39" ] <- "m"
retail$item_size [retail$item_size == "40" ] <- "m"
retail$item_size [retail$item_size == "40+"] <- "m"
retail$item_size [retail$item_size == "41" ] <- "l"
retail$item_size [retail$item_size == "41+"] <- "l"
retail$item_size [retail$item_size == "42+"] <- "l"
retail$item_size [retail$item_size == "42" ] <- "l"
retail$item_size [retail$item_size == "43" ] <- "l"
retail$item_size [retail$item_size == "43+"] <- "xl"
retail$item_size [retail$item_size == "44+"] <- "xl"
retail$item_size [retail$item_size == "44" ] <- "xl"
retail$item_size [retail$item_size == "45" ] <- "xl"
retail$item_size [retail$item_size == "45+"] <- "xl"
retail$item_size [retail$item_size == "46" ] <- "xl"
retail$item_size [retail$item_size == "46+"] <- "xl"
retail$item_size [retail$item_size == "47" ] <- "xxl"
retail$item_size [retail$item_size == "48" ] <- "xxl"
retail$item_size [retail$item_size == "50" ] <- "xxl"
retail$item_size [retail$item_size == "34" ] <- "xs"
retail$item_size [retail$item_size == "52" ] <- "xxxl"
retail$item_size [retail$item_size == "54" ] <- "xxxl"
retail$item_size [retail$item_size == "56" ] <- "xxxl"
#retail$item_size [retail$item_size == "unsized"] <- "m"
# user_dob / data cleansing
retail$user_dob <- substring(retail$user_dob,1,4)
retail$user_dob [retail$user_dob == "?"] <- NA
retail$user_dob [retail$user_dob < 1920 ] <-NA
count<- table(retail$user_dob)
MFV <- names(count) [count == max (count)]
retail$user_dob [is.na(retail$user_dob)] <- MFV # which is "1966"
retail$user_dob <- as.numeric(retail$user_dob)
standardize <- function(x){
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  return(result)}
zScores_dob <- standardize(retail$user_dob)
retail$user_dob [zScores_dob > 3] <- round(mean(retail$user_dob) + 3*sd(retail$user_dob), digit=0)
retail$user_dob [zScores_dob < -3] <- round(mean(retail$user_dob) - 3*sd(retail$user_dob), digit=0)
# order_date and delivery_date / data cleansing
retail$order_date <-   as.Date(retail$order_date, "%Y-%m-%d")
retail$delivery_date <- as.Date(retail$delivery_date, "%Y-%m-%d")
retail$delivery_duration<- difftime(retail$delivery_date , retail$order_date, units = c("days"))
retail$delivery_duration [retail$delivery_duration < 0] <- NA
retail$delivery_duration [retail$delivery_duration == "?"] <- NA
retail$delivery_duration [is.na (retail$delivery_duration)] <- 2 
retail$delivery_duration <- as.numeric(retail$delivery_duration, units="days") 
# item-price / data cleansing
retail$item_price <- as.numeric(retail$item_price)
boxplot(retail$item_price)
zScores <- standardize(retail$item_price)
retail$item_price [is.na(retail$item_price) ] <- 59.9
retail$item_price [zScores > 3] <- round(mean(retail$item_price) + 3*sd(retail$item_price), digit=2)

#user_reg_date / data cleansing
retail$user_reg_date <-   as.Date(retail$user_reg_date, "%Y-%m-%d")
retail$user_maturity <- difftime(retail$order_date , retail$user_reg_dat, units = c("days"))
retail$user_maturity <- as.numeric(retail$user_maturity, units="days") 
#user_title / data cleansing
retail$user_title [retail$user_title == "not reported"] <-NA
retail$user_title [is.na(retail$user_title)] <- "Mrs"
#item_color /  data cleansing
retail$item_color [retail$item_color == "blau"] <- "blue"
retail$item_color [retail$item_color == "brwon"] <- "brown"
retail$item_color [retail$item_color == "oliv"] <- "olive"
retail$item_color [retail$item_color == "?"] <- NA
sort(table(retail$item_color), decreasing = TRUE) ## MFV
retail$item_color [is.na (retail$item_color)] <- "black"
retail$user_dob <- as.numeric(retail$user_dob)
# Month of Delivery
retail$month_of_delivery <- substring(retail$delivery_date,6,7)
retail$month_of_delivery [is.na(retail$month_of_delivery)] <- "01"
# Factoring
chrIdx <- which(sapply(retail, is.character))
retail[, chrIdx] <- lapply( retail[, chrIdx],factor)
retail$return2 <- retail$return##Hyunah added
retail$return <- factor(retail$return, labels = c("keep","return"))
retail$item_price <- as.numeric(retail$item_price)

str(retail)


# I am using a Decision Tree in the predictions.

if(!require("caret")) install.packages("caret"); library("caret")
if(!require("rpart")) install.packages("rpart"); library("rpart") 
if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")


dt <- rpart(return ~  user_dob + user_maturity + delivery_duration+ user_state + item_price + item_color +item_size  + month_of_delivery, data = retail, method = "class",  control = rpart.control( cp = 0.001, minsplit =  20 ))

# Accuracy
  
prob.pred_known  = as.vector(predict(dt, newdata=retail, type="prob") [,"return"])
class.pred_known  = ifelse(prob.pred_known > 0.5, "return", "keep")
confusionMatrix(data = class.pred_known, reference = retail$return, positive = "return")
prp(dt, extra = 104, border.col = 0, box.palette="auto")
  
  

  
  
  
