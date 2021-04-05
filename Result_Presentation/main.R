
source("helper.R")

library(tidyverse)
library(lubridate)
library(rugarch)
library(tseries)

Clean_df <- function(STOCK_NAME, PRICE_FILE, VOL_FILE){
  
  #' This function converts from messy dataframes into a single nice dataframe
  #' @param STOCK_NAME A stock abbreviation
  #' @param PRICE_FILE A csv directory of price dataframe
  #' @param VOL_FILE A csv directory of volume traded dataframe
  #' @return A tibble (dataframe) of a single nice dataframe
  
  price_df <- read_csv(PRICE_FILE)
  names(price_df) <- parse_header(names(price_df), "price")
  
  vol_df <- read_csv(VOL_FILE)
  names(vol_df) <- parse_header(names(vol_df), "volume")
  
  if(STOCK_NAME == "Code"){
    stop("Invalid STOCK_NAME argument")
  }
  
  if((!(STOCK_NAME %in% names(price_df))) |
     (!(STOCK_NAME %in% names(vol_df)))){
    stop("Stock is not found!")
  }
  
  stock_price <- price_df %>% select(Code, STOCK_NAME)
  stock_vol <- vol_df %>% select(Code, STOCK_NAME)
  
  log_df <- stock_price %>% 
    left_join(stock_vol, by = "Code", suffix = c("_p", "_v")) %>%
    filter(!is.na(get(col_cat(STOCK_NAME, "volume")))) %>%
    mutate(log_ret = log(get(col_cat(STOCK_NAME, "price"))) - 
             lag(get(col_cat(STOCK_NAME, "price"))))
  
  cleaned_df <- log_df %>% 
    mutate(Date = parse_date(Code, format = "%m/%d/%Y")) %>%
    select(Date, log_ret) %>%
    filter(!is.na(log_ret)) %>% 
    filter(Date > mdy(01012014) & Date < mdy(01012020))
  
  cleaned_df
}

Run_model <- function(cleaned_df, model){
  
  #' This function runs the specified model on cleaned dataframe
  #' @param cleaned_df A cleaned dataframe using Clean_df
  #' @param model A specified model: either garch or gjr
  #' @return A list of model results
  
  Model_Spec <- ugarchspec(
          variance.model=list(model=convert_model(model),
                              garchOrder=c(1,1)),
          mean.model=list(armaOrder=c(0,0)), 
          distribution.model="norm")
  
  Model_Fit <- ugarchfit(spec=Model_Spec, data=cleaned_df$log_ret)
  
  list(Coef = coef(Model_Fit), Goodness = infocriteria(Model_Fit))
}