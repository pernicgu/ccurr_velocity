
#' download market data from coinmarket cap for an individual coin and timeframe
#' 
#' @param coin as string as of coinmarketcap.com (no ticker). dates as unix timestamps since 01.01.1970 as *string*.
#' 
#' @return dataframe with market cap, price in BTC, price in USD, volume in USD

loadCoinsCMC <- function(coin, date_start, date_end){

  # (Thanking: https://coinmarketcap.com/de/faq/ -> Content and data can be used for personal or scientific reasons, if source is provided.)
  
  url <- paste("https://graphs2.coinmarketcap.com/currencies/", coin ,"/", date_start ,"/", date_end,"/" ,sep="")
  
  json_raw <- GET(url=url)
  json <- content(json_raw, "text")
  
  list_raw <- fromJSON(json)
  df <- as.data.frame(lapply(list_raw, function(x){x[,2]}))
  
  df <- cbind(list_raw[[1]][,1] , df)
  colnames(df)[1] <- "timestamp"
  
  return(df)

}

#' match filenames for downloaded data with requested coins
#' 
#' @param coin as string as of coinmarketcap.com (no ticker)
#' 
#' @return vector of missing coins
#' 
getMissingCoins <- function(coins_desired){
  file_list <- list.files(path = paste(dirname(wd_home), "/data/marketdata_raw", sep=""))
  coins_loaded_raw <- lapply(strsplit(file_list, split="_"), function(x){x[1]})
  coins_loaded <- unlist(coins_loaded_raw)
  coins_toload <- coins_desired[!coins_desired %in% coins_loaded]
  
  return(coins_toload)
}


#' download data for a list of coin names
#' 
#' @param coin as string as of coinmarketcap.com (no ticker). dates as unix timestamps since 01.01.1970 as *string*.
#' 
#' @return -
#' 
downloadCoinData <- function(coins, date_start, date_end){
    
    data_list <- list()
    for(coin in coins){
        
                                        # load data
        dta <- loadCoinsCMC(coin, date_start, date_end)
        Sys.sleep(20)
        
                                        # save data in ram
        data_list[[which(coin == coins)]] <- dta
        
                                        # save data on hdd
        setwd(paste(dirname(wd_home), "/data/marketdata_raw/", sep=""))
        write.csv(dta, row.names = FALSE, file=paste(coin, "_" ,date_start, "_" ,date_end , ".csv", sep=""))
        cat("Saved data. \n")
        setwd(wd_home)
        
    }
    names(data_list) <- coins
    
    return(print("Appearently successfull. Please give write checks into me!"))
    
}


#' gather the csvs as dataframes in one list to work with it further in R
#' 
#' @param coin as string as of coinmarketcap.com (no ticker). dates as unix timestamps since 01.01.1970 as *string*.
#' 
#' @return list of dataframes with market cap, price in BTC, price in USD, volume in USD

getCoinData <- function(coins, date_start, date_end){
  
  data_list <- list()
  file_list <- list.files(path = paste(dirname(wd_home), "/data/marketdata_raw/", sep=""))
  for(coin in coins){
    
    # find file
    file <- file_list[grep(coin, c(file_list, date_start, date_end))]
    
    # error exeption
    if(length(file) > 1) {break; print("Check your downloads for duplicates!")}
    
    # load data
      dta <- read.csv(file = paste(path = paste(dirname(wd_home), "/data/marketdata_raw/",
                                              coin, "_" ,date_start, "_" ,date_end , ".csv", sep="")))
    
    # save data in ram
    data_list[[which(coin == coins)]] <- dta
    
  }
    names(data_list) <- coins

  
  return(data_list) 
}






.getTimestamp <- function(){
    timestamp <- as.character(round(as.numeric(Sys.time())*1000))
    return(timestamp)
}
