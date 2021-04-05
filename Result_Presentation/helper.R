library(tidyverse)
library(lubridate)
library(rugarch)

parse_header <- function(header, type){
  
  #' This function parses the vector of headers into usable form from datastream
  #' @param header A vector of headers (assuming string)
  #' @param type Either "price" or "volume" specifying the type of cols
  #' @return A vector of cleaned header

  if(type %in% c("price", "volume")){
    modified <- str_extract(header[-1], "(?<=Q:)([A-Za-z]+)(?=\\([A-Za-z]+\\))")
    c(header[1], modified)
  } else {
    stop("Invalid argument found!")
  }
}

col_cat <- function(STOCK_NAME, type){
  
  #' This function concats the stock_name with type
  #' @param STOCK_NAME A specified stock abbreviation (string)
  #' @param type Either "price" or "volume" specifying the type of vars
  #' @return A string consisting of STOCK_NAME and type
  
  if (type == "price"){
    str_c(STOCK_NAME, "_p")
  } else if (type == "volume"){
    str_c(STOCK_NAME, "_v")
  } else {
    STOCK_NAME
  }
}

convert_model <- function(model){
  
  #' This function returns the model that needs to be run in rugarch
  #' @param model Either garch or gjr
  #' @return the model parameter needed to be used in rugarch
  
  if (tolower(model) == "garch"){
    "sGARCH"
  } else if (tolower(model) == "gjr"){
    "gjrGARCH"
  } else {
    stop("Invalid model argument: either sGARCH or gjr only!")
  }
}