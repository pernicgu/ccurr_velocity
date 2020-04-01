##### Environment ####
setwd("../../")
wd_home <- getwd()
setwd(paste(wd_home, "impl", "marketdata_generation", sep="/"))
source("SETTINGS.R")


##### Download data #####
coins_missing <- getMissingCoins(SETTINGS$coins)
if(length(coins_missing) > 0){
    downloadCoinData(coins_missing,
                     SETTINGS$date_start,
                     SETTINGS$date_end)
}


#### Load data ####
data_list <- getCoinData(SETTINGS$coins,
                         SETTINGS$date_start,
                         SETTINGS$date_end)
str(data_list)
