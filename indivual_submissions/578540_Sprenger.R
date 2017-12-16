# BADS WiSe 2017/2018

# Individual Assignment

# Author: Nicolai Sprenger
# Student ID: 578540
# E-mail: sprengernicolai@gmail.com

setwd("~/Google Drive/Studies/Master/MEMS/Studium/WiSe 1718/BADS/Assignment/Individual Assignment")
data_known <- read.csv("BADS_WS1718_known.csv", header = TRUE, sep = ",")

# Data Preparation of known data
  install.packages(c("lubridate","plyr"))
  library("lubridate")
  library("plyr")
  
# delivery date unknown apparently denoted with "?"
# variable delivery time missing
# variable item size
  # shoe sizes, clothes sizes, "unsized"
    # shoe sizes EU and UK or US formats
    # clothes sizes with inconsistent capitalization
    # meaning of unsized
# variable user_title corresponds to sex (female or male) as realizations seem limited to "Mrs" and "Mr"
# user dob unknown apparently denoted with "?"

str(data_known)

# data frame with 100,000 observations over 14 variables
  # order_item_id int
  # order_date factor 365 levels
  # delivery_date factor 320 levels
  # item_id int
  # item_size factor
  # item_color factor
  # brand_id int
  # item_price num
  # user_id int
  # user_title factor 5 levels, i.e. not just gender
  # user_dob factor
  # user_state factor 16 levels, i.e. complete
  # user_reg_date factor, complete
  # return int

summary(data_known)

# variable item price has a minimum of 0. This may be due to, e.g., coupons / vouchers redeemed during the purchase. However, prices of 0 may also be due to a data generation mistake
# variable user_dob contains many values from 1900. Unlikely! (cp. jump b/w earliest and second earliest YOB)
# variable user_title contains the realizations "Company", "Family", "Mr", "Mrs" and "not reported"
  # unknown user titles are apparently denoted with "not reported"
# mean of variable return shows that c. 48% of orders were returned
head(data_known)


table(data_known$order_date)

# data about orders between 2012-04-01 and 2013-03-31

# variable delivery date

table(data_known$delivery_date)

# issue 1: Unknown dates denoted by "?" make up a significant number of orders (8292, i.e., c. 8%)
# issue 2: Inappropriate delivery date given order dates
  # issue 2a: delivery time of up to 151 days seems inappropriate
  # issue 2b: 959 observations have a delivery date in 1990 even though table(data_known$order_date) has shown that the earliest order date is 2012-04-01

# issue 1: 4 possible explanations
  # 1) the orders were cancelled before they could be dispatched
  # 2) the orders were dispatched but never arrived, i.e. they got lost 
  # 3) the orders were dispatched but could not reach their destination as the recipient was unavailable (wrong address, no pick-up, etc.) 
  # 4) the orders did arrive s.t. the lack of a definite date is due to some mistake in the data generating process
    
# Explanations 1 - 4
  # Check viability of explanations by considering the respective return rate. Explanations 3) implies a return rate of 100%. Explanation 1) and 2) imply a return rate of 0%. Explanation 4) implies a return rate similar to the average over all 100,000 orders (Test against mean?!)
    delivery_date_unknown<-subset(data_known, data_known$delivery_date=="?")
    table(delivery_date_unknown$return)
    # 0% returned vs 48% in all of data
      # Substantiates Explanations 1) and 2)
  #Solution issue 1: As the orders never arrived at the customers', I exclude these obeservations in my predition altogether as they would skew the results otherwise
  #Delete observatons where the variable delivery date takes on "?"  
    to.be.removed_delivery_date<-which(data_known$delivery_date=="?")
    data_known<-data_known[-to.be.removed_delivery_date,]

# issue 2b: 2 possible sets of explanations 
  # 1) same set as for issue 1
    # Same check as for issue 1
     data_known$delivery_date<-as.Date.factor(data_known$delivery_date)
     class(data_known$delivery_date)
     delivery_date_1990<-subset(data_known, data_known$delivery_date==min(data_known$delivery_date,na.rm = TRUE))
     table(delivery_date_1990$return) 
      # Number of returns appears "normal". Hence true date unknown
  # 2) mistake in data generating process
    
  # Solution issue 2b: Replace the delivery date with the average expected delivery date or median delivery date with the help of a newly created variable, delivery time (in days)
    # Replacement
      to.be.replaced_delivery_date<-which(year(data_known$delivery_date)==min(year(data_known$delivery_date),na.rm = TRUE))
    # Introduce a new variable, delivery time, measired in days
      data_known$delivery_time_days<-difftime(data_known$delivery_date, data_known$order_date, units = c("days"))
    # Distribution of delivery time
      summary(as.numeric(data_known$delivery_time_days))
      # Minimum delivery time of 0 days seems reasonable, since the store could offer a same day delivery option
      # However, maximum is too high
    # Calculate the average delivery time in days disregarding those that correpsond to a delivery date in 1990
      mean_delivery_time_days<-mean.difftime(data_known$delivery_time_days[-to.be.replaced_delivery_date])
      
      mean_delivery_time_days
    # For comparison, also compute the Median  
      median_delivery_time_days<-median(data_known$delivery_time_days[-to.be.replaced_delivery_date])                               
      
      median_delivery_time_days
    # An average expected delivery time of c. 11 days appears to be too long. Mean is here influenced by large outliers. Thus, choose the median rather than the mean
      data_known$delivery_time_days[to.be.replaced_delivery_date]<-median_delivery_time_days
    # Replace delivery dates in 1990 with the expected delivery date given the calculated median delivery time in days
      # Adjust formats of date variables in order to enable replacement  
      data_known$order_date<-as.Date.factor(data_known$order_date)
      # Execute replacement
      data_known$delivery_date<-data_known$order_date+days(data_known$delivery_time_days)
      # Reconvert variables
      data_known$delivery_date<-as.factor(data_known$delivery_date)
      data_known$order_date<-as.factor(data_known$order_date)      
      table(data_known$delivery_date)

# issue 2a: 2 possible explanations
  # 1) The given delivery time is correct
    # Number of cases should be minimal, since inventory / delivery issues should occur rarely
      table(data_known$delivery_time_days>14)
    # Number of cases amount to c. 20% and can therefore not be regarded as outliers per se
    # Check if particular products are affected disproportionatley often
      products_long_delivery_time<-subset(data_known, data_known$delivery_time_days>50)
      table(products_long_delivery_time$item_id)
      table(products_long_delivery_time$return)
      # 2) Mistake in data generating process
      
  # Solution issue 2a: find outliers for delivery date with interquartile range (IQR) of newly created variable delivery time
      max_delivery_time_days<-quantile(data_known$delivery_time_days,0.75,na.rm = TRUE)+(IQR(data_known$delivery_time_days,na.rm = TRUE)*1.5)
      outlier_delivery_time_days_idc<-which(data_known$delivery_time_days>max_delivery_time_days)
    # Replace outliers with median delivery time of 3 days    
      data_known$delivery_time_days[outlier_delivery_time_days_idc]<-median_delivery_time_days
      max(data_known$delivery_time_days, na.rm = TRUE)
      # Maximum delivery time of 17 days seems reasonable given taht ordered items may sometimes be not in stock
      table(data_known$delivery_time_days)
    # Replace corresponding delivery date entries using the median delivery time in days
      median_delivery_time_idc<-which(data_known$delivery_time_days==median_delivery_time_days)
      data_known$delivery_date<-as.Date.factor(data_known$delivery_date)
      data_known$order_date<-as.Date.factor(data_known$order_date)
      data_known$delivery_date<-data_known$order_date+days(data_known$delivery_time_days)
      table(data_known$delivery_date)
      table(difftime(data_known$delivery_date,data_known$order_date,units = c("days")))
    # Reconvert variables order_date and delivery_date
      data_known$order_date<-as.factor(data_known$order_date)
      data_known$delivery_date<-as.factor(data_known$delivery_date)
# variable user title

table(data_known$user_title)

  # Replace 'not reported' by NA
    data_known$user_title[data_known$user_title=="not reported"]<-NA
  # Create a dummy variable for NA user title  
    data_known$user_title_NA<-ifelse(is.na(data_known$user_title),1,0)

# variable item size

table(data_known$item_size)

# correct for unsized, correct for capitalization, correct for unrealistic sizes
  # correct for capitalization
    data_known$item_size<-as.character.factor(data_known$item_size)
    data_known$item_size<-toupper(data_known$item_size)
    table(data_known$item_size)
    data_known$item_size<-as.factor(data_known$item_size)

    
  # correct for unrealistic sizes
    # isolate sizes above largest conventional size 176
    item_oversized<-subset(data_known$item_size, as.numeric(as.character.factor(data_known$item_size))>176)
    table(item_oversized)
      # overview shows ten sizes. Except for "3434", all of them seem to be ranges of ordinary sizes as is commonly used for socks, for example
        # replace those size ranges with the average value
          item_oversized_idc<-which(as.numeric(as.character.factor(data_known$item_size))>176)
          item_oversized_means<-rowMeans(cbind(as.numeric(substring(as.character.factor(data_known$item_size[item_oversized_idc]),1,2)),as.numeric(substring(as.character.factor(data_known$item_size[item_oversized_idc]),3,4)))) 
          data_known$item_size<-replace(as.character.factor(data_known$item_size),item_oversized_idc,item_oversized_means)
          table(data_known$item_size[item_oversized_idc])
  
  # correct for "UNSIZED"
    # Replace 'UNSIZED' by NA
      data_known$item_size[data_known$user_title=="UNSIZED"]<-NA
    # Create a dummy variable for NA user title  
      data_known$item_size_NA<-ifelse(is.na(data_known$item_size),1,0)          
  
  # reconvert variable item size into a factor
      data_known$item_size<-as.factor(data_known$item_size)
      table(data_known$item_size)
# variable date of birth
      
table(data_known$user_dob)

  # replace '?' by NA as central moments would be a very imprecise approximation only
    data_known$user_dob[data_known$user_dob=="?"]<-NA
    table(data_known$user_dob)
  # identify outliers 
    user_yob<-year(data_known$user_dob)
    table(user_yob)
    # Use interquartile range after replacing the 828 entries for 1900 and the 206 entries from 1901 with NA, since those numbers are definitely off
    wrong_user_yob_idc<-which(user_yob==1990|data_known$user_dob==1991)
    wrong_user_yob_idc
    user_yob[wrong_user_yob_idc]<-NA
    data_known$user_dob[wrong_user_yob_idc]<-NA
    # Identify small outliers using the IQR
      min_yob<-quantile(user_yob,0.25,na.rm = TRUE)-(IQR(user_yob,na.rm = TRUE)*1.5)
      outlier_user_yob_idc<-which(user_yob<min_yob)
    # replace outliers in dob by NA
      data_known$user_dob[outlier_user_yob_idc]<-NA
      table(data_known$user_dob)
      min(year(data_known$user_dob), na.rm = TRUE)
      # 1943 as the earliest yob seems to be reasonable given that we are dealing with an ecommerce shop
    # Clean large outliers by replacing all ages that are not at least 7 from consideration as those indivuauls would be legally incompetent
      legally_incompetent_idc<-which(difftime(data_known$order_date, data_known$user_dob,units = c("days"))<7*365)
      legally_incompetent_idc
      data_known$user_dob[legally_incompetent_idc]<-NA
      max(year(data_known$user_dob), na.rm = TRUE)
      # 2000 as the latest yob of a customer seems reasonable given that we are dealing with an online shop for clothing
    # create a dummy variabke for NA user dob
      data_known$user_dob_NA<-ifelse(is.na(data_known$user_dob),1,0)
      
# Reformat date variables as "YYYY-MM"
  # Variable order_date
    data_known$order_date<-format(as.Date(data_known$order_date,format="%Y-%m-%d"), "%Y-%m")      
    data_known$order_date
    data_known$order_date<-as.factor(data_known$order_date)
  # Variable delivery_date
    data_known$delivery_date<-format(as.Date(data_known$delivery_date,format="%Y-%m-%d"), "%Y-%m")      
    data_known$delivery_date
    data_known$delivery_date<-as.factor(data_known$delivery_date)
  # Variable user_dob  
    data_known$user_dob<-format(as.Date(data_known$user_dob,format="%Y-%m-%d"), "%Y-%m")      
    data_known$user_dob
    data_known$user_dob<-as.factor(data_known$user_dob)    
  # Variable user_reg_date
    data_known$user_reg_date<-format(as.Date(data_known$user_reg_date,format="%Y-%m-%d"), "%Y-%m")      
    data_known$user_reg_date
    data_known$user_reg_date<-as.factor(data_known$user_reg_date)
# Reformat delivery time as factor
  data_known$delivery_time_days<-as.factor(data_known$delivery_time_days)
  
# Predictive modeling
  install.packages(c("rpart","rpart.plot","glmnet","caret","e1071","C50","hmeasure"))
  library("rpart")
  library("rpart.plot")
  library("glmnet")
  library("caret")  
  library("C50")
  library("e1071")
  library("hmeasure")
  
  set.seed(123)
  # Convert target variable into a factor
  data_known$target<-factor(data_known$return, levels = c(0,1), labels = c("No Return","Return"))
  levels(data_known$target)
  # Exclude variables delivery_date, return and item_color for reasons of multicolinearity and dimensionality, respectively
  to.be.excluded<-c("delivery_date","item_color", "return")
  data_known<-data_known[,!(names(data_known)%in%to.be.excluded)]
  
    
  
# Decision trees
    # C5.0 Decision tree with 4 fold cross validation
      explanatory_variables.dt<-names(data_known)[-c(16)]
      form.dt<-as.formula(paste("target ~", paste(explanatory_variables.dt, collapse = "+")))  
      folds.dt<-split(data_known,cut(sample(1:nrow(data_known)),4))
      list_models.dt<-list()
      test_data.dt<-list()
      list_pred.dt<-list()
      
      for (i in 1:length(folds.dt)) {
        test.dt<-ldply(folds.dt[i],data.frame)
        test_data.dt[[i]]<-test.dt
        train.dt<-ldply(folds.dt[-i],data.frame)
        tmp.model.dt<-C5.0(form.dt,train.dt,control = C5.0Control(minCases = 5, CF = 0.2, fuzzyThreshold = TRUE))
        list_models.dt[[i]]<-tmp.model.dt
        tmp.predict.dt<-predict(tmp.model.dt, newdata=test.dt, type = "prob")[,2]
        list_pred.dt[[i]]<-tmp.predict.dt
      }
      list_pred.dt_test<-list()
      list_pred.dt_test[[1]]<-ifelse(list_pred.dt[[1]]>0.52,1,0)
      list_pred.dt_test[[2]]<-ifelse(list_pred.dt[[2]]>0.52,1,0)
      list_pred.dt_test[[3]]<-ifelse(list_pred.dt[[3]]>0.52,1,0)
      list_pred.dt_test[[4]]<-ifelse(list_pred.dt[[4]]>0.52,1,0)
      list_pred.dt_test
      list_pred.dt.df<-data.frame(list_pred.dt_test)
      h<-list()
      
      h[1]<-HMeasure(true.class = as.numeric(test_data.dt[[1]]$target)-1, scores = list_pred.dt.df[1], severity.ratio = 0.1)
      h[2]<-HMeasure(true.class = as.numeric(test_data.dt[[2]]$target)-1, scores = list_pred.dt.df[2], severity.ratio = 0.1)
      h[3]<-HMeasure(true.class = as.numeric(test_data.dt[[3]]$target)-1, scores = list_pred.dt.df[3], severity.ratio = 0.1)
      h[4]<-HMeasure(true.class = as.numeric(test_data.dt[[4]]$target)-1, scores = list_pred.dt.df[4], severity.ratio = 0.1)
      
      AUC.dt<-vector()
      
      for(i in 1:length(h)) {
        AUC.dt[i]<-h[[i]]$AUC
      }
      print(max(AUC.dt))
      which(AUC.dt==max(AUC.dt))
      # list_models.dt[2] has the highest AUC on testing dataset
      
      ACC.1.dt<-mean(list_pred.dt.df[1]==as.numeric(test_data.dt[[1]]$target)-1)
      ACC.2.dt<-mean(list_pred.dt.df[2]==as.numeric(test_data.dt[[2]]$target)-1)
      ACC.3.dt<-mean(list_pred.dt.df[3]==as.numeric(test_data.dt[[3]]$target)-1)
      ACC.4.dt<-mean(list_pred.dt.df[4]==as.numeric(test_data.dt[[4]]$target)-1)
      which(as.numeric(c(ACC.1.dt, ACC.2.dt, ACC.3.dt, ACC.4.dt))==max(as.numeric(c(ACC.1.dt, ACC.2.dt, ACC.3.dt, ACC.4.dt))))
      # list_models.dt[2] has the highest PCC on testing dataset
      ACC.2.dt
      
 # Regression
    # Logistic regression with 2 fold cross validation
      explanatory_variables.lr.cv<-names(data_known)[-16]
      form.lr.cv<-as.formula(paste("target ~", paste(explanatory_variables.lr.cv, collapse = "+")))
      n<-nrow(na.exclude(data_known))
      sample.size<-ceiling(n*0.75)
      idc.train<-createDataPartition(y = na.exclude(data_known$target), p = 0.75, list = FALSE)
      tr<-na.exclude(data_known[idc.train,])
      ts<-na.exclude(data_known[-idc.train,])
      model.lr.cv<-glm(form.lr.cv, data = tr, family = binomial(link = "logit"))
      
      ts$item_size[which(as.character.factor(ts$item_size)=="2+")]<-NA
      ts$item_size[which(as.character.factor(ts$item_size)=="33.5")]<-NA
      ts$user_dob[which(as.character.factor(ts$user_dob)=="1992-06")]<-NA
      ts$user_dob[which(as.character.factor(ts$user_dob)=="1994-11")]<-NA
      ts$user_dob[which(as.character.factor(ts$user_dob)=="1995-07")]<-NA
      ts$user_dob[which(as.character.factor(ts$user_dob)=="1998-06")]<-NA
      
      pred.lr.cv<-predict(model.lr.cv, newdata = ts, type = "response")[,2]
      pred.lr.cv
      predictions.lr.cv<-ifelse(pred.lr.cv>0.49,1,0)
      pred.lr.cv.df<-data.frame(predictions.lr.cv)
      pred.lr.cv.df
      
      h[[5]]<-HMeasure(true.class = as.numeric(ts$target)-1, scores = pred.lr.cv.df, severity.ratio = 0.1)
      h[[5]]$metrics["AUC"]
    
    # Lasso logistic regression with 2 fold cross validation
      x_tr<-model.matrix(target~.-1, tr)
      y_tr<-tr$target
      x_ts<-model.matrix(target~.-1, ts)
      y_ts<-ts$target
      length(y_ts)
      model.lasso.cv<-glmnet(x = x_tr, y = y_tr, family = "binomial", alpha = 1, nlambda = 100)
      pred.lasso.cv_test<-as.vector(predict(model.lasso.cv, newx = x_ts, s = 0.001, type = "response")[,2])
      pred.lasso.cv_test
      
      predictions.lasso.cv_test<-ifelse(pred.lasso.cv_test>0.47,1,0)
      predictions.lasso.cv_test
      pred.lasso.cv.df_test<-data.frame(predictions.lasso.cv_test)
      y_ts<-y_ts[1:20143]
      h[[6]]<-HMeasure(true.class = as.numeric(y_ts)-1, scores = pred.lasso.cv.df_test, severity.ratio = 0.1)
      h[[6]]$metrics["AUC"]

      AUC.all<-as.numeric(c(h[[1]]$AUC, h[[2]]$AUC, h[[3]]$AUC, h[[4]]$AUC, h[[5]]$metrics["AUC"], h[[6]]$metrics["AUC"]))
      
      max(AUC.all)
      which(AUC.all==max(AUC.all))
      
      
# Data Preparation of unknown data
   data_class <- read.csv("BADS_WS1718_class.csv", header = TRUE, sep = ",")
      
      str(data_class)
      # Same variables as for data_known except returns
      
      summary(data_class)
      
      # variable item price has a minimum of 0. This may be due to, e.g., coupons / vouchers redeemed during the purchase. However, prices of 0 may also be due to a data generation mistake
      # variable user_dob contains many values from 1900. Unlikely! (cp. jump b/w earliest and second earliest YOB)
      # variable user_title contains the realizations "Company", "Family", "Mr", "Mrs" and "not reported"
      # unknown user titles are apparently denoted with "not reported"
     
      head(data_class)
      
      table(data_class$order_date)
      
      # data about orders between 2012-04-01 and 2013-03-31
      
      # variable delivery date
      
      table(data_class$delivery_date)
      
      # issue 1: Unknown dates denoted by "?" make up a significant number of orders (8292, i.e., c. 8%)
      # issue 2: Inappropriate delivery date given order dates
      # issue 2a: delivery time appears inappropriately long at times
      # issue 2b: 480 observations have a delivery date in 1990 even though table(data_known$order_date) has shown that the earliest order date is 2012-04-01
      
      # issue 1: 4 possible explanations
      # 1) the orders were cancelled before they could be dispatched
      # 2) the orders were dispatched but never arrived, i.e. they got lost 
      # 3) the orders were dispatched but could not reach their destination as the recipient was unavailable (wrong address, no pick-up, etc.) 
      # 4) the orders did arrive s.t. the lack of a definite date is due to some mistake in the data generating process
      
      #Solution issue 1: I exclude these obeservations in my predition altogether as they would skew the results otherwise (cp. known dataset)
      #Delete observatons where the variable delivery date takes on "?"  
      to.be.removed_delivery_date_class<-which(data_class$delivery_date=="?")
      data_class<-data_class[-to.be.removed_delivery_date_class,]
      
      # issue 2b: 2 possible sets of explanations 
      # Excursion
      data_class$delivery_date<-as.Date.factor(data_class$delivery_date)
      class(data_class$delivery_date)
      delivery_date_1990_class<-subset(data_class, data_class$delivery_date==min(data_class$delivery_date,na.rm = TRUE))
      # Excursion end
      # 1) Assume mistake in data generating process (cp. known dataset)
      
      # Solution issue 2b: Replace the delivery date with the average expected delivery date or median delivery date with the help of a newly created variable, delivery time (in days)
      # Replacement
      to.be.replaced_delivery_date_class<-which(year(data_class$delivery_date)==min(year(data_class$delivery_date),na.rm = TRUE))
      # Introduce a new variable, delivery time, measired in days
      data_class$delivery_time_days<-difftime(data_class$delivery_date, data_class$order_date, units = c("days"))
      # Distribution of delivery time
      summary(as.numeric(data_class$delivery_time_days))
      # Minimum negative due 1990 realizations, maximum is too high
      # Calculate the average delivery time in days disregarding those that correpsond to a delivery date in 1990
      mean_delivery_time_days_class<-mean.difftime(data_class$delivery_time_days[-to.be.replaced_delivery_date_class])
      
      mean_delivery_time_days_class
      # For comparison, also compute the Median  
      median_delivery_time_days_class<-median(data_class$delivery_time_days[-to.be.replaced_delivery_date_class])                               
      
      median_delivery_time_days_class
      # An average expected delivery time of c. 11 days appears to be too long. Mean is here influenced by large outliers. Thus, choose the median rather than the mean
      data_class$delivery_time_days[to.be.replaced_delivery_date_class]<-median_delivery_time_days_class
      # Replace delivery dates in 1990 with the expected delivery date given the calculated median delivery time in days
      # Adjust formats of date variables in order to enable replacement  
      data_class$order_date<-as.Date.factor(data_class$order_date)
      # Execute replacement
      data_class$delivery_date<-data_class$order_date+days(data_class$delivery_time_days)
      # Reconvert variables
      data_class$delivery_date<-as.factor(data_class$delivery_date)
      data_class$order_date<-as.factor(data_class$order_date)      
      table(data_class$delivery_date)
      
      # issue 2a: 2 possible explanations
      # Excursion
      # Number of cases should be minimal, since inventory / delivery issues should occur rarely
      table(data_class$delivery_time_days>14)
      # Number of cases amount to c. 20% and can therefore not be regarded as outliers per se
      # Check if particular products are affected disproportionatley often
      products_long_delivery_time_class<-subset(data_class, data_class$delivery_time_days>50)
      table(products_long_delivery_time_class$item_id)
      # Excursion end
      # 1) Assume mistake in data generating process (cp. known dataset)
      
      # Solution issue 2a: find outliers for delivery date with interquartile range (IQR) of newly created variable delivery time
      max_delivery_time_days_class<-quantile(data_class$delivery_time_days,0.75,na.rm = TRUE)+(IQR(data_class$delivery_time_days,na.rm = TRUE)*1.5)
      outlier_delivery_time_days_class_idc<-which(data_class$delivery_time_days>max_delivery_time_days_class)
      # Replace outliers with median delivery time of 3 days    
      data_class$delivery_time_days[outlier_delivery_time_days_class_idc]<-median_delivery_time_days_class
      max(data_class$delivery_time_days, na.rm = TRUE)
      # Maximum delivery time of 17 days seems reasonable given taht ordered items may sometimes be not in stock
      table(data_class$delivery_time_days)
      # Replace corresponding delivery date entries using the median delivery time in days
      median_delivery_time_class_idc<-which(data_class$delivery_time_days==median_delivery_time_days_class)
      data_class$delivery_date<-as.Date.factor(data_class$delivery_date)
      data_class$order_date<-as.Date.factor(data_class$order_date)
      data_class$delivery_date<-data_class$order_date+days(data_class$delivery_time_days)
      table(data_class$delivery_date)
      table(difftime(data_class$delivery_date,data_class$order_date,units = c("days")))
      # Reconvert variables order_date and delivery_date
      data_class$order_date<-as.factor(data_class$order_date)
      data_class$delivery_date<-as.factor(data_class$delivery_date)
      
      # Variable user title
      
      table(data_class$user_title)
      
      # Replace 'not reported' by NA
      data_class$user_title[data_class$user_title=="not reported"]<-NA
      # Create a dummy variable for NA user title  
      data_class$user_title_NA<-ifelse(is.na(data_class$user_title),1,0)
      
      # variable item size
      
      table(data_class$item_size)
      
      
      # correct for unsized, correct for capitalization, correct for unrealistic sizes
      # correct for capitalization
      data_class$item_size<-as.character.factor(data_class$item_size)
      data_class$item_size<-toupper(data_class$item_size)
      table(data_class$item_size)
      data_class$item_size<-as.factor(data_class$item_size)
      
      
      # correct for unrealistic sizes
      # isolate sizes above largest conventional size 176
      item_oversized_class<-subset(as.numeric(as.character.factor(data_class$item_size))>176)
      table(item_oversized_class)
      # overview shows ten sizes. Except for "3434", all of them seem to be ranges of ordinary sizes as is commonly used for socks, for example
      # replace those size ranges with the average value
      item_oversized_class_idc<-which(as.numeric(as.character.factor(data_class$item_size))>176)
      item_oversized_means_class<-rowMeans(cbind(as.numeric(substring(as.character.factor(data_class$item_size[item_oversized_class_idc]),1,2)),as.numeric(substring(as.character.factor(data_class$item_size[item_oversized_class_idc]),3,4)))) 
      data_class$item_size<-replace(as.character.factor(data_class$item_size),item_oversized_class_idc,item_oversized_means_class)
      table(data_class$item_size[item_oversized_class_idc])
      
      # correct for "UNSIZED"
      # Replace 'UNSIZED' by NA
      data_class$item_size[data_class$user_title=="UNSIZED"]<-NA
      # Create a dummy variable for NA user title  
      data_class$item_size_NA<-ifelse(is.na(data_class$item_size),1,0)          
      
      # reconvert variable item size into a factor
      data_class$item_size<-as.factor(data_class$item_size)
      table(data_class$item_size)
      # variable date of birth
      
      table(data_class$user_dob)
      
      # replace '?' by NA as central moments would be a very imprecise approximation only
      data_class$user_dob[data_class$user_dob=="?"]<-NA
      table(data_class$user_dob)
      # identify outliers 
      user_yob_class<-year(data_class$user_dob)
      table(user_yob_class)
      # Use interquartile range after replacing the 828 entries for 1900 and the 206 entries from 1901 with NA, since those numbers are definitely off
      wrong_user_yob_class_idc<-which(user_yob_class==1990|data_class$user_dob==1991)
      wrong_user_yob_class_idc
      user_yob_class[wrong_user_yob_class_idc]<-NA
      data_class$user_dob[wrong_user_yob_class_idc]<-NA
      # Identify small outliers using the IQR
      min_yob_class<-quantile(user_yob_class,0.25,na.rm = TRUE)-(IQR(user_yob_class,na.rm = TRUE)*1.5)
      outlier_user_yob_class_idc<-which(user_yob_class<min_yob_class)
      # replace outliers in dob by NA
      data_class$user_dob[outlier_user_yob_class_idc]<-NA
      table(data_class$user_dob)
      min(year(data_class$user_dob), na.rm = TRUE)
      # 1943 as the earliest yob seems to be reasonable given that we are dealing with an ecommerce shop
      # Clean large outliers by replacing all ages that are not at least 7 from consideration as those indivuauls would be legally incompetent
      legally_incompetent_class_idc<-which(difftime(data_class$order_date, data_class$user_dob,units = c("days"))<7*365)
      legally_incompetent_class_idc
      data_class$user_dob[legally_incompetent_class_idc]<-NA
      max(year(data_class$user_dob), na.rm = TRUE)
      # 2000 as the latest yob of a customer seems reasonable given that we are dealing with an online shop for clothing
      # create a dummy variabke for NA user dob
      data_class$user_dob_NA<-ifelse(is.na(data_class$user_dob),1,0)
      
      # Reformat date variables as "YYYY-MM"
      # Variable order_date
      data_class$order_date<-format(as.Date(data_class$order_date,format="%Y-%m-%d"), "%Y-%m")      
      data_class$order_date
      data_class$order_date<-as.factor(data_class$order_date)
      # Variable delivery_date
      data_class$delivery_date<-format(as.Date(data_class$delivery_date,format="%Y-%m-%d"), "%Y-%m")      
      data_class$delivery_date
      data_class$delivery_date<-as.factor(data_class$delivery_date)
      # Variable user_dob  
      data_class$user_dob<-format(as.Date(data_class$user_dob,format="%Y-%m-%d"), "%Y-%m")      
      data_class$user_dob
      data_class$user_dob<-as.factor(data_class$user_dob)    
      # Variable user_reg_date
      data_class$user_reg_date<-format(as.Date(data_class$user_reg_date,format="%Y-%m-%d"), "%Y-%m")      
      data_class$user_reg_date
      data_class$user_reg_date<-as.factor(data_class$user_reg_date)
      # Reformat delivery time as factor
      data_class$delivery_time_days<-as.factor(data_class$delivery_time_days)
      
  # Prediction
      # Using the decision tree model list_models.dt[[2]] with second highest AUC of all tested models, a PCC of c. 60% and the fewest warning messages (0)
      
      data_class$item_size[which(as.character.factor(data_class$item_size)=="31.5")]<-NA
      
      data_class$item_size[which(as.character.factor(data_class$item_size)=="49")]<-NA
      
      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1992-02")]<-NA

      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1992-09")]<-NA
 
      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1993-03")]<-NA
 
      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1994-02")]<-NA

      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1994-03")]<-NA

      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1994-08")]<-NA

      data_class$user_dob[which(as.character.factor(data_class$user_dob)=="1999-02")]<-NA
      
      pred.dt.class<-predict(list_models.dt[[2]], newdata = data_class, type = "prob")[,2]
      pred.dt.class      
      data_class$return<-ifelse(pred.dt.class>0.52,1,0)      
      data_class$return
      submit.pred_class<-data_class[, c(1, 18)]
      submit.pred_class
      submit.pred_class<-as.data.frame(submit.pred_class)
      submit.pred_class
      write.csv(submit.pred_class, file = "578540_Sprenger", row.names = FALSE)
      
      