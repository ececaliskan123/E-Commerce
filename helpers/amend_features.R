### Helper function to data preprocessing and feature selection before applying predictive modeling

amend_features = function(dd){
  dd = subset(dd, select = -c(user_reg_date, user_title, user_state, item_price))
  dd$order_month = as.numeric(format(dd$order_date, "%m"))
  dd$order_day   = as.numeric(format(dd$order_date, "%d"))
  dd             = subset(dd, select=-order_date)
  
  dd$del_year  = as.numeric(format(dd$delivery_date, "%Y"))
  dd$del_month = as.numeric(format(dd$delivery_date, "%m"))
  dd$del_day   = as.numeric(format(dd$delivery_date, "%d"))
  dd           = subset(dd, select=-c(delivery_date, month_of_delivery))
  
  if("return" %in% colnames(dd)) {
    dd = normalizeFeatures(dd, target="return")
  }
  return(dd)
}