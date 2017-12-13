# This function reads in the CSV file located at fp
# and returns a dataframe where all the unwanted values have been replaced
read_and_preprocess_data_file = function(fp) {
  d = read.csv(fp, stringsAsFactors = FALSE)
  
  ## DATES
  # convert character order dates to data type and into real numbers
  d$order_date_year  = as.integer(substring(d$order_date, 0, 4))
  d$order_date_month = as.integer(substring(d$order_date, 6, 7))
  d$order_date       = as.Date(d$order_date, "%Y-%m-%d")

  ## DELIVERY DATE
  # clean up delivery dates NULL values
  d$delivery_date[d$delivery_date == "?"] = NA
  d$delivery_date[d$delivery_date == "1990-12-31"] = NA
  ## FULFILMENT DURATION
  # convert to fulfilment range
  d$delivery_date       = as.Date(d$delivery_date, "%Y-%m-%d")
  d$fulfilment_duration = as.integer(d$delivery_date - d$order_date) # NA + date equals NA
  # TODO: NULL value replacement strategy - use the most common
  countFUL = table(d$fulfilment_duration)
  typical_duration = names(countFUL)[countFUL == max(countFUL)]
  d$fulfilment_duration[is.na(d$fulfilment_duration)] = as.integer(typical_duration)
  
  ## CLOTHING SIZES
  d$item_size = toupper(d$item_size)
  d$item_size[d$item_size == "UNSIZED"] = NA
  # replacement strategies - sizes bought less than 500 times
  if (is.na(rarely_bought_sizes)) {
    rarely_bought_sizes = names(table(d$item_size))[table(d$item_size) < 500]
  }
  
  d$item_size[is.element(d$item_size, rarely_bought_sizes)] = "other"
  # NULL value replacement strategy - take the most common one
  sizeTable = table(d$item_size)
  typical_size = names(sizeTable)[sizeTable == max(sizeTable)]
  d$item_size[is.na(d$item_size)] = typical_size
  d$item_size = factor(d$item_size)
  
  ## ITEM COLORS
  d$item_color[d$item_color == "brwon"] = "brown" # stupid typo
  d$item_color[d$item_color == "?"] = NA
  # group all colors bought less than 1000 times
  if (is.na(rarely_bought_colors)) {
    rarely_bought_colors = names(table(d$item_color))[table(d$item_color) < 1000]
  }
  d$item_color[is.element(d$item_color, rarely_bought_colors)] = "other"
  # NULL value replacement strategy - take the most common one
  colorTable = table(d$item_color)
  typical_color = names(colorTable)[colorTable == max(colorTable)]
  d$item_color[is.na(d$item_color)] = typical_color
  d$item_color = factor(d$item_color)
  
  ## USER REGISTRATION DATES
  # 29416 registrations on 2011-02-16 ... fishy...
  d$order_reg_date_year  = as.integer(substring(d$user_reg_date, 0, 4))
  d$order_reg_date_month = as.integer(substring(d$user_reg_date, 6, 7))
  d$user_reg_date = as.Date(d$user_reg_date, "%Y-%m-%d")
  
  ## USER STATES
  d$user_state = factor(d$user_state)
  # some algorithms can not cope with spaces in level names so replace
  levels(d$user_state) = gsub(" ", "-", levels(d$user_state))

  
  ## USER TITLES
  d$user_title[d$user_title == "not reported"] = "other"
  d$user_title[d$user_title != "Mr" & d$user_title != "Mrs"] = "other"
  d$user_title = factor(d$user_title)
  
  ## USER DATE OF BIRTH
  # numerous users born on 19.11.1900 ?! - seems suspicious on histogram
  # TODO come up with normal distribution approximation here
  d$user_dob[d$user_dob == "?"] = NA
  d$user_dob[d$user_dob <= "1901-01-01"] = NA
  d$user_dob[is.na(d$user_dob)] = names(median(sort(table(d$user_dob))))
  d$user_dob_year  = as.integer(substring(d$user_dob, 0, 4))
  d$user_dob_month = as.integer(substring(d$user_dob, 6, 7))
  d$user_dob = as.Date(d$user_dob, "%Y-%m-%d")
  
  ## REMOVE ALL DATES SINCE THEY WERE REFORMATTED
  d = subset(d, select = -c(delivery_date, user_dob, order_date, user_reg_date))
  
  return(d)
}
