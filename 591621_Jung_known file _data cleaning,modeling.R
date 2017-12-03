#Email address : bluehyunah@gmail.com

#installing all packages needed for cleaning data and the prediction
install.packages("lubridate")
install.packages("tidyr")
install.packages("stringr")
install.packages("rpart")
install.packages("ggplot2")
install.packages("GGally")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
install.packages("caret")
install.packages("hmeasure")
install.packages("glmnet")
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(rpart)
library(eeptools)
library(ggplot2)
library(GGally)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(class)
library(caret)
library(hmeasure)
library(glmnet)


###load known file
known = read.csv(file.choose(), sep = ",", header = TRUE)

###finding out how the data is structured
summary(known)
glimpse(known)

###changing the type of the variables to the correct one
known$user_reg_date <- as.Date(known$user_reg_date, format="%Y-%m-%d")
known$order_date <- as.Date(known$order_date, format="%Y-%m-%d")
known$delivery_date <- as.Date(known$delivery_date, format="%Y-%m-%d")
known$user_dob <- as.Date(known$user_dob, format="%Y-%m-%d")


###recognizing NA values and renaming them so that R can understand that they are NA
known$delivery_date[known$delivery_date == "?"] <- NA
known$item_color[known$item_color == "?"] <- NA
known$item_size[known$item_size == "unsized"] <- NA
known$user_title[known$user_title == "not reported"] <- NA
known$user_dob[known$user_dob == "?"] <- NA

###dealing/replacing the missing data
##dealing with missing data from user_title
#find most frequent value of user_title #item_color, item_size, user_dob
names(table(known$user_title))[table(known$user_title) == max(table(known$user_title))]
names(table(known$item_color))[table(known$item_color) == max(table(known$item_color))] ##
names(table(known$item_size))[table(known$item_size) == max(table(known$item_size))] ##
sort(table(known$user_dob),decreasing=TRUE)[1:3] ##
#most frequent value is "Mrs" in user_title by a very large margin
known$user_title[is.na(known$user_title)] <- "Mrs"

##replacement missing values in item_color to the most frequent value
known$item_color[is.na(known$item_color)] <- "black"

#replacement missing values in user_dob
#I found the three most frequent birthdays, 1900-11-19, 1949-11-19 and 1961-12-31.
#I chose 1961-12-31. Reasoning: the first one does not make any sense. The second one would make sense, but using a histogram we see that most users are born in the 1960s. So I chose the third one
known$user_dob[is.na(known$user_dob)] <- "1961-12-31"


##dealing with missing data from item_color
#check distribution of item_color
plot(known$item_color)
table(known$item_color)
#colors are very distributed, so we cannot attribute the NA values to any single color
#thus NA values should be omitted
#NA values will be omitted once all other NA values are dealt with

##dealing with missing data from delivery_date
#trying to find an average value of the number of days it takes to deliver the package
#first step is to find the days it takes between order_date and delivery_date
days_until_delivery <- known$delivery_date - known$order_date
#it turns out there are many outliers (inconsistent values) from delivery_dates in the 1990s, so these had to be filtered out
positive_days_until_delivery <- subset(days_until_delivery, days_until_delivery>=0)
#average of non-negative values of days until delivery, excluding the NAs, turns out to be 11 (rounded up)
mean(positive_days_until_delivery, na.rm = TRUE)
#changing NA values from delivery_date to the order date of each observation added with the average time to delivery
a <- which(is.na(known$delivery_date))
known$delivery_date[a] <- known$order_date[a]+11


#dealing with outliers from delivery_date, values found in 1990, changed to order date + average time to deliver
hist(known$delivery_date, breaks=50)
k <- which(known$delivery_date < "2009-01-01")
known$delivery_date[k] <- known$order_date[k]+11


##item_size; change the values with sub(), using regex to avoid wrong replacements

i = c("\\<1\\>","\\<2\\>","\\<3\\>","\\<31\\>","\\<32\\>","\\<33\\>","\\<76\\>","\\<80\\>","\\<116\\>","\\<2\\+\\>","\\<3\\+\\>","\\<xs\\>")
for(i in i){
  known$item_size = sub(i,"XS",known$item_size)
}

q = c("\\<4\\>","\\<5\\>","\\<6\\>","\\<7\\>","\\<8\\>","\\<9\\>","\\<34\\>","\\<35\\>","\\<36\\>","\\<84\\>","\\<128\\>","\\<2932\\>","\\<36\\+\\>","\\<4\\+\\>","\\<5\\+\\>","\\<6\\+\\>","\\<7\\+\\>","\\<8\\+\\>","\\<s\\>")
for(q in q){
  known$item_size = sub(q,"S",known$item_size)
}
w = c("\\<10\\>","\\<11\\>","\\<12\\>","\\<37\\>","\\<38\\>","\\<39\\>","\\<40\\>","\\<88\\>","\\<89\\>","\\<90\\>","\\<91\\>","\\<92\\>","\\<93\\>","\\<94\\>","\\<95\\>","\\<140\\>","\\<3132\\>","\\<3332\\>","\\<3334\\>","\\<3432\\>","\\<3434\\>","\\<10\\+\\>","\\<37\\+\\>","\\<38\\+\\>","\\<39\\+\\>","\\<9\\+\\>","\\<m\\>")
for(w in w){
  known$item_size = sub(w,"M",known$item_size)
}
e = c("\\<13\\>","\\<14\\>","\\<41\\>","\\<42\\>","\\<43\\>","\\<44\\>","\\<45\\>","\\<96\\>","\\<100\\>","\\<152\\>","\\<3632\\>","\\<3634\\>","\\<11\\+\\>","\\<12\\+\\>","\\<40\\+\\>","\\<41\\+\\>","\\<42\\+\\>","\\<l\\>")
for(e in e){
  known$item_size = sub(e,"L",known$item_size)
}
r = c("\\<18\\>","\\<19\\>","\\<20\\>","\\<46\\>","\\<47\\>","\\<48\\>","\\<104\\>","\\<105\\>","\\<164\\>","\\<3832\\>","\\<3834\\>","\\<43\\+\\>","\\<44\\+\\>","\\<xl\\>","\\<xL\\>")
for(r in r){
  known$item_size = sub(r,"XL",known$item_size)
}
t = c("\\<21\\>","\\<22\\>","\\<23\\>","\\<24\\>","\\<25\\>","\\<50\\>","\\<52\\>","\\<176\\>","\\<4032\\>","\\<4034\\>","\\<45\\+\\>","\\<46\\+\\>","\\<49\\>","\\<xxl\\>","\\<xxL\\>")
for(t in t){
  known$item_size = sub(t,"XXL",known$item_size)
}
u = c("\\<26\\>","\\<27\\>","\\<28\\>","\\<29\\>","\\<30\\>","\\<54\\>","\\<55\\>","\\<56\\>","\\<57\\>","\\<58\\>","\\<xxxl\\>","\\<xxxL\\>")
for(u in u){
  known$item_size = sub(u,"XXXL",known$item_size)
}

known$item_size = gsub("\\+","",known$item_size)
summary(known$item_size)
known$item_size <- as.factor(known$item_size)

##replacement missing values in item_color to the most frequent value
known$item_size[is.na(known$item_size)] <- "M"
summary(known$item_size)

###user_dob outliers
##convert user_dob(ymd) to age
#convert all dates within test to r data type "Date"
known$user_dob = as.Date(known$user_dob,format="%Y-%m-%d")
#calculate the age depending on the birthdate
known$user_dob = age_calc(known$user_dob, units="years")
summary(known$user_dob)
mean(known$user_dob)
# Calculate the z-score with the function we created earlier for standardization
standardize <- function(x){
  # The actual calculations necessary to standardize the values
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  # Return ends the function and outputs the result
  return(result)
  # A list can be returned to output more than one result
  #return(list("mean" = mu, "sd" = std, "output" = result))
}
# Calculate and save the z-scores
zScores <- standardize(known$user_dob)
summary(zScores)
# Replace the assumed outliers with a threshold value
known$user_dob[zScores > 3] <- mean(known$user_dob) + 3*sd(known$user_dob)
#Convert the age to date and store it in user_dob
known$user_dob = date_decimal(2017.92-known$user_dob)
known$user_dob = as.Date(known$user_dob,format="%Y-%m-%d")
hist(known$user_dob, breaks=100)
summary(known$user_dob)


###determining which variables are correlated with each other

#finding correlation of numeric variables
known_only_numeric <- known[, sapply(known, is.numeric)]
cor(known_only_numeric)

#it looks like the item price has a large correlation on the return

#finding out the age of users in years
dob_age <- age_calc(known$user_dob, units="years")


#first model with return modeled on all variables in known
model.glm0 <- glm(formula = return ~., family = binomial(link = "logit"), data = known)
#calculating the residuals
residuals0 <- resid(model.glm0)
#calculating the rmse to see model fit
sqrt(mean(residuals0^2))
#calculating the pseudo-r-squared value
with(model.glm0, 1 - deviance/null.deviance)


known2 <- known
days_until_delivery <- known2$delivery_date - known2$order_date
known2$days_until_delivery <- days_until_delivery
known2$dob_age <- dob_age


### Model Partitioning
# Setting the seed allows the computer to remember the state of time you run this analysis 
set.seed(1)
# Need the full sample size for the following steps
n <- NROW(known2)
# Split point for partition: 75% of sample
sample.size <- ceiling(n*0.75) 
# Random sampling of indices
# This lets us choose what rows to use for the training/test sets
indices <- sample(n, sample.size) 
# Training set, 75% of known2
known2_tr <- known2[indices, ] 
# Test set, remaining 25% of known2
known2_ts <- known2[-indices, ]
# Estimate model on the training dataset
model.glm5 <- glm(data = known2_tr, 
                  formula = return ~
                    poly(item_price,2) +
                    factor(user_title)*factor(item_size) +
                    dob_age +
                    as.numeric(days_until_delivery),
                  family = binomial(link = "logit"))
# Training residuals
residuals5_tr <- resid(model.glm5) 
# RMSE of the training set
rmse_tr <- sqrt(mean(residuals5_tr^2)) 
# Test residuals
residuals5_ts <- resid(model.glm5, newdata = known2_ts)
# Test residuals
rmse_ts <- sqrt(mean(residuals5_ts^2)) 
# Combine to compare side-by-side
rmse_compare <- cbind(rmse_tr, rmse_ts) 
rmse_compare 

### glmnet(); Standardized Logistic Regression
mymodel <- model.matrix(return ~
                          poly(item_price,2) +
                          factor(user_title)*factor(item_size) +
                          dob_age +
                          as.numeric(days_until_delivery),
                        known2_tr)
depvar <- known2_tr$return
lasso <- glmnet(x = mymodel,
                y = depvar,
                family = 'binomial',
                standardize = TRUE,
                alpha = 1,
                nlambda = 100)
dt <- rpart(return~., data = known2_tr, method = "class")
basic_coef <- round(coef(model.glm5), 2)
lasso_coef <- round(coef(lasso, 0.01), 2)[ , 1]
basic_coef
lasso_coef
with(lasso, qplot(y = dev.ratio, x = lambda))
qplot(y = lasso$dev.ratio, x = lasso$lambda)

# Performance_train
predict_model.glm5 <- predict(model.glm5, newdata = known2_tr, type = "response")
predict_lasso <- as.vector(predict(lasso, newx = mymodel, s = 0.001, type = "response"))
predict_dt <- predict(dt, newdata = known2_tr, type = "prob")[, 2]
m <- max(length(predict_model.glm5), length(predict_dt))
length(predict_model.glm5) <- m
length(predict_dt) <- m
yhat.m  <- cbind(predict_model.glm5, predict_dt)
yhat.df <- na.omit(as.data.frame(yhat.m))
h <- HMeasure(as.numeric(known2_tr$return)-1, yhat.df, severity.ratio = 0.1)
auc_trainset <- h$metrics['AUC']
auc_trainset
# Performance_test
predict_dt_1 <- predict(dt, newdata = known2_ts, type = "prob")[, 2]
predict_model.glm5_1 <- predict(model.glm5, newdata = known2_ts, type = "response")
m <- max(length(predict_model.glm5), length(predict_dt))
length(predict_model.glm5) <- m
length(predict_dt_1) <- m
yhat.m1  <- cbind(predict_model.glm5, predict_dt_1)
yhat.df1 <- na.omit(as.data.frame(yhat.m1))
h <- HMeasure(as.numeric(known2_ts$return)-1, yhat.df1, severity.ratio = 0.1)
auc_testset <- h$metrics['AUC']
auc_testset











