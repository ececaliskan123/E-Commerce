#Email address : bluehyunah@gmail.com

install.packages("readr")
library(readr)


###load class file
class = read.csv(file.choose(), sep = ",", header = TRUE)

###changing the type of the variables to the correct one
class$user_reg_date <- as.Date(class$user_reg_date, format="%Y-%m-%d")
class$order_date <- as.Date(class$order_date, format="%Y-%m-%d")
class$delivery_date <- as.Date(class$delivery_date, format="%Y-%m-%d")
class$user_dob <- as.Date(class$user_dob, format="%Y-%m-%d")


###recognizing NA values and renaming them so that R can understand that they are NA
class$delivery_date[class$delivery_date == "?"] <- NA
class$item_color[class$item_color == "?"] <- NA
class$item_size[class$item_size == "unsized"] <- NA####
class$user_title[class$user_title == "not reported"] <- NA
class$user_dob[class$user_dob == "?"] <- NA

###dealing/replacing the missing data
##dealing with missing data from user_title
#find most frequent value of user_title #item_color, item_size, user_dob
names(table(class$user_title))[table(class$user_title) == max(table(class$user_title))]
names(table(class$item_color))[table(class$item_color) == max(table(class$item_color))] ##
names(table(class$item_size))[table(class$item_size) == max(table(class$item_size))] ##
sort(table(class$user_dob),decreasing=TRUE)[1:3] ##
#most frequent value is "Mrs" in user_title by a very large margin
class$user_title[is.na(class$user_title)] <- "Mrs"

##replacement missing values in item_color to the most frequent value
class$item_color[is.na(class$item_color)] <- "black"

#replacement missing values in user_dob
#I found the three most frequent birthdays, 1900-11-19, 1949-11-19 and 1961-12-31.
#I chose 1961-12-31. Reasoning: the first one does not make any sense. The second one would make sense, but using a histogram we see that most users are born in the 1960s. So I chose the third one
class$user_dob[is.na(class$user_dob)] <- "1961-12-31"


##dealing with missing data from item_color
#check distribution of item_color
plot(class$item_color)
table(class$item_color)
#colors are very distributed, so we cannot attribute the NA values to any single color
#thus NA values should be omitted
#NA values will be omitted once all other NA values are dealt with

##dealing with missing data from delivery_date
#trying to find an average value of the number of days it takes to deliver the package
#first step is to find the days it takes between order_date and delivery_date
days_until_delivery <- class$delivery_date - class$order_date
#it turns out there are many outliers (inconsistent values) from delivery_dates in the 1990s, so these had to be filtered out
positive_days_until_delivery <- subset(days_until_delivery, days_until_delivery>=0)
#average of non-negative values of days until delivery, excluding the NAs, turns out to be 11 (rounded up)
mean(positive_days_until_delivery, na.rm = TRUE)
#changing NA values from delivery_date to the order date of each observation added with the average time to delivery
a <- which(is.na(class$delivery_date))
class$delivery_date[a] <- class$order_date[a]+11


#dealing with outliers from delivery_date, values found in 1990, changed to order date + average time to deliver
hist(class$delivery_date, breaks=50)
k <- which(class$delivery_date < "2009-01-01")
class$delivery_date[k] <- class$order_date[k]+11


##item_size; change the values with sub(), using regex to avoid wrong replacements

i = c("\\<1\\>","\\<2\\>","\\<3\\>","\\<31\\>","\\<32\\>","\\<33\\>","\\<76\\>","\\<80\\>","\\<116\\>","\\<2\\+\\>","\\<3\\+\\>","\\<xs\\>")
for(i in i){
  class$item_size = sub(i,"XS",class$item_size)
}

q = c("\\<4\\>","\\<5\\>","\\<6\\>","\\<7\\>","\\<8\\>","\\<9\\>","\\<34\\>","\\<35\\>","\\<36\\>","\\<84\\>","\\<128\\>","\\<2932\\>","\\<36\\+\\>","\\<4\\+\\>","\\<5\\+\\>","\\<6\\+\\>","\\<7\\+\\>","\\<8\\+\\>","\\<s\\>")
for(q in q){
  class$item_size = sub(q,"S",class$item_size)
}
w = c("\\<10\\>","\\<11\\>","\\<12\\>","\\<37\\>","\\<38\\>","\\<39\\>","\\<40\\>","\\<88\\>","\\<89\\>","\\<90\\>","\\<91\\>","\\<92\\>","\\<93\\>","\\<94\\>","\\<95\\>","\\<140\\>","\\<3132\\>","\\<3332\\>","\\<3334\\>","\\<3432\\>","\\<3434\\>","\\<10\\+\\>","\\<37\\+\\>","\\<38\\+\\>","\\<39\\+\\>","\\<9\\+\\>","\\<m\\>")
for(w in w){
  class$item_size = sub(w,"M",class$item_size)
}
e = c("\\<13\\>","\\<14\\>","\\<41\\>","\\<42\\>","\\<43\\>","\\<44\\>","\\<45\\>","\\<96\\>","\\<100\\>","\\<152\\>","\\<3632\\>","\\<3634\\>","\\<11\\+\\>","\\<12\\+\\>","\\<40\\+\\>","\\<41\\+\\>","\\<42\\+\\>","\\<l\\>")
for(e in e){
  class$item_size = sub(e,"L",class$item_size)
}
r = c("\\<18\\>","\\<19\\>","\\<20\\>","\\<46\\>","\\<47\\>","\\<48\\>","\\<104\\>","\\<105\\>","\\<164\\>","\\<3832\\>","\\<3834\\>","\\<43\\+\\>","\\<44\\+\\>","\\<xl\\>","\\<xL\\>")
for(r in r){
  class$item_size = sub(r,"XL",class$item_size)
}
t = c("\\<21\\>","\\<22\\>","\\<23\\>","\\<24\\>","\\<25\\>","\\<50\\>","\\<52\\>","\\<176\\>","\\<4032\\>","\\<4034\\>","\\<45\\+\\>","\\<46\\+\\>","\\<49\\>","\\<xxl\\>","\\<xxL\\>")
for(t in t){
  class$item_size = sub(t,"XXL",class$item_size)
}
u = c("\\<26\\>","\\<27\\>","\\<28\\>","\\<29\\>","\\<30\\>","\\<54\\>","\\<55\\>","\\<56\\>","\\<57\\>","\\<58\\>","\\<xxxl\\>","\\<xxxL\\>")
for(u in u){
  class$item_size = sub(u,"XXXL",class$item_size)
}

class$item_size = gsub("\\+","",class$item_size)
summary(class$item_size)
class$item_size <- as.factor(class$item_size)

##replacement missing values in item_color to the most frequent value
class$item_size[is.na(class$item_size)] <- "M"


###user_dob outliers
##convert user_dob(ymd) to age
#convert all dates within test to r data type "Date"
class$user_dob <- as.Date(class$user_dob,format="%Y-%m-%d")
#calculate the age depending on the birthdate
class$user_dob <- age_calc(class$user_dob, units="years")
summary(class$user_dob)
mean(class$user_dob)
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
zScores <- standardize(class$user_dob)
summary(zScores)
# Replace the assumed outliers with a threshold value
class$user_dob[zScores > 3] <- mean(class$user_dob) + 3*sd(class$user_dob)
#Convert the age to date and store it in user_dob
class$user_dob <- date_decimal(2017.92-class$user_dob)
class$user_dob <- as.Date(class$user_dob,format="%Y-%m-%d")
hist(class$user_dob, breaks=100)
summary(class$user_dob)

#finding out the age of users in years
dob_age <- age_calc(class$user_dob, units="years")

#create return column in class
class2 <- class
days_until_delivery <- class2$delivery_date - class2$order_date
class2$days_until_delivery <- days_until_delivery
class2$dob_age <- dob_age


###prediction for return
predict_return <- predict(model.glm5, newdata = class2, type = 'response')
class2$predict_return <- with(class2, ifelse(predict_return < 0.5, 0, 1))

#Select only 2 columns for csv file
class3 <- subset(class2, select = c(order_item_id, predict_return))

csv <- write_csv(class3, '591621_Jung.csv')


