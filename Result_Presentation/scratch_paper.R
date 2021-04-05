library(tidyverse)
library(lubridate)
library(rugarch)


STOCK_NAME <- "LHBA"

parse_header <- function(header, type){
  if(type == "price"){
    modified <- str_extract(header[-1], "(?<=Q:)([A-Za-z]+)(?=\\(P\\))")
    c(header[1], modified)
  } else if (type == "volume"){
    modified <- str_extract(header[-1], "(?<=Q:)([A-Za-z]+)(?=\\(VO\\))")
    c(header[1], modified)    
  } else {
    stop("Invalid argument found!")
  }
}

get_col <- function(STOCK_NAME, type){
  if (type == "price"){
    str_c(STOCK_NAME, "_p")
  } else if (type == "vol"){
    str_c(STOCK_NAME, "_v")
  } else {
    STOCK_NAME
  }
}

price_df <- read_csv("data/Bank_Price.csv")
price_df
names(price_df) <- parse_header(names(price_df), "price")

vol_df <- read_csv("data/Bank_Volume.csv")
vol_df
names(vol_df) <- parse_header(names(vol_df), "volume")

stock_price <- price_df %>% select(Code, STOCK_NAME)
stock_vol <- vol_df %>% select(Code, STOCK_NAME)

log_df <- stock_price %>% 
  full_join(stock_vol, by = "Code", suffix = c("_p", "_v")) %>%
  filter(!is.na(get(str_c(STOCK_NAME, "_v")))) %>%
  mutate(log_ret = log(get(get_col(STOCK_NAME, "price"))) - 
           lag(get(get_col(STOCK_NAME, "price")))) 

cleaned_df <- log_df %>% 
  mutate(Date = parse_date(Code, format = "%m/%d/%Y")) %>%
  select(Date, log_ret) %>%
  filter(!is.na(log_ret)) %>% 
  filter(Date > mdy(01012014) & Date < mdy(01012020))

ggplot(cleaned_df, aes(Date, log_ret)) + geom_line()

garchSpec <- ugarchspec(
  variance.model=list(model="sGARCH",
                      garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(0,0)), 
  distribution.model="norm")
garchFit <- ugarchfit(spec=garchSpec, data=cleaned_df$log_ret)
coef(garchFit)

GJR_garchSpec <- ugarchspec(
  variance.model=list(model="gjrGARCH"),
  mean.model=list(armaOrder=c(0,0)), 
  distribution.model="norm")
GJR_garchFit <- ugarchfit(spec=GJR_garchSpec, data=cleaned_df$log_ret)
coef(GJR_garchFit)

infocriteria(GJR_garchFit)["Akaike",]

cleaned_df

df <- Clean_df("LHBA", "data/Bank_Price.csv", "data/Bank_Volume.csv")
result <- Run_model(df, "garch")