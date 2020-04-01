##### Environment ####
setwd("../../")
wd_home <- getwd()
setwd(paste(wd_home, "impl", "marketdata_generation", sep="/"))
source("SETTINGS.R")


#### Load data ####
data_list <- getCoinData(SETTINGS$coins,
                         SETTINGS$date_start,
                         SETTINGS$date_end)
data_list <- lapply(data_list,
                    timestampsToPosixct, "timestamp")

## timeseries characteristics
                                        # supply and change in supply
data_list <- lapply(data_list,
                    addCoinSupply,
                    cap_col = "market_cap_by_available_supply",
                    price_col = "price_usd",
                    replaceInf = TRUE,
                    replaceMult = 2,
                    replaceType = "mult",
                    removeFirstRow = FALSE)

                                        # returns
data_list <- lapply(data_list,
                    addReturns,
                    type = "simple",
                    applLog = FALSE,
                    price_col = "price_usd",
                    removeFirstRow = TRUE)


                                        # volatility
data_list <- lapply(data_list,
                    addVolatility,
                    type="squaredreturns",
                    return_col = "return_wrt_price_usd.simple",
                    replaceInf=TRUE,
                    replaceMult=2,
                    replaceType="na")

data <- as.data.frame(data_list)

setwd(paste(dirname(wd_home), "/data/marketmeasures_raw/", sep=""))
write.csv(data, row.names = FALSE, file=paste("marketmeasures.csv"))
cat("Saved data. \n")
setwd(wd_home)








