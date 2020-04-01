
#'
#' Improved cbind, for binding dataframes of different length
#' 
my.cbind <- function(x,y,first) {
    
    if(nrow(x)<nrow(y)) {
        na_chunks <- matrix(NA, nrow = abs(nrow(y)-nrow(x)), ncol = ncol(x))
        colnames(na_chunks) <- colnames(x)
        if(first==1) x = rbind(na_chunks, x);y=y
        if(first==0) x = rbind(x, na_chunks);y=y
    } 
    
    if(nrow(y)<nrow(x)) {
        na_chunks <- matrix(NA, nrow = abs(nrow(x)-nrow(y)), ncol = ncol(y))
        colnames(na_chunks) <- colnames(y)
        if(first==1) y = rbind(na_chunks, y) ;x=x
        if(first==0) y = rbind(y , na_chunks) ;x=x
    } 
    
    return(cbind(x,y))
    
}


#'
#'
#' @input x : numeric vector
#'
#' @output lr : log returns of numeric vector
#' 
logReturn.element <- function(x){
  lr <- (log(x)-log(dplyr::lag(x)))
  return(lr)
}


#'
#'
#' @input x : numeric vector
#'
#' @output lr : simple returns of numeric vector
#' 
return.element <- function(x){
  r <- ((x/dplyr::lag(x))-1)
  return(r)
}


#'
#'
#' @input raw_df : standardized coinbase dataframe, stored in data_list
#' @input type : way for return calulation, currently only "simple"
#' @input appLog : logarithmic returns or not
#' @input price_col : colname of column identifying the price index
#'
#' @output R_df : raw_df with appened column for returns
#' 
addReturns <- function(raw_df,
                       type = "simple",
                       applLog = FALSE,
                       price_col = "price_usd",
                       removeFirstRow = TRUE){


                                        # extract correct colname for price index
        cname_prices <- colnames(raw_df)[grepl(x = colnames(raw_df), pattern = price_col)] 
    
    if (type == "simple"){

                                        # extract price index and calc returns / log returrs
        price_df <- raw_df[ ,cname_prices]
        ret <- if(applLog == FALSE){
                   return.element(price_df)
               } else if (applLog == TRUE) {
                   logReturn.element(price_df)
               }
        
        ret = as.data.frame(ret)
        colnames(ret) <-paste0("return_wrt_", price_col, ".",type)
        ret_df <- cbind(raw_df, ret)
    }
    
        
    if(class(ret_df$timestamps) == "string"){ret_df$timestamp <- as.numeric(as.POSIXct(ret_df$timestamp))}

    if(removeFirstRow == TRUE){ret_df <- ret_df[-1, ]}
    
    return(ret_df)
}




#'
#'
#' @input raw_df : standardized coinbase dataframe, stored in data_list
#' @input type : way for volatility calulation, currently only "squaredreturns"
#' @input return_col : the column name of the column, we want to calc retruns from
#' @input replaceInf : boolean, TRUE if should be replaced
#' @input replaceMult : numeric, multiplicator of maximum / mininum volatility for infinite values
#' @input replaceType : string, method of replacement
#'
#' @output R_df : raw_df with appened column for volatility
#' 
addVolatility <- function(raw_df,
                          type="squaredreturns",
                          return_col = "return_wrt_price_usd%simple%",
                          replaceInf = TRUE,
                          replaceMult = 2,
                          replaceType = "mult"){
                                        #calc Volatility wrt. type
    if (type == "squaredreturns"){
        
        vol <- (raw_df[ ,return_col])^2
        
    } else if (type == "realizedvolatility"){

        print("currently no HF data availble")
        
    }

                                        # replace inf
    if(replaceInf == TRUE){
    vol <- replInfVol(vol, multiplicator = replaceMult, type = replaceType)
    }
    
    vol_df <- as.data.frame(vol)
    colnames(vol_df) <- paste0("vol", ".",type)
    vol_df <- cbind(raw_df, vol_df)
    
    return(vol_df)
    
}

#' @input vol : vector giving volatility
#' @input multiplicator : numeric, multiplicator of maximum / mininum volatility for infinite values
#' @input type : string, method of replacement
#'
#' @output vol : vector of cleaned volatility
replInfVol <- function(vol, multiplicator = 2, type=c("mult", "na")){

    vol.max <- max(vol)
    vol.min <- min(vol)
    
    if(type == "mult"){
            
        vol[vol == "Inf" ] <- vol.max * multiplicator
        vol[vol == "-Inf" ] <- vol.min * multiplicator
        
    } else if(type == "na"){

        vol[vol == "Inf" ] <- NA
        vol[vol == "-Inf" ] <- NA

    }

    return(vol)
    
}



################################### IN CONSTRUCTION


timestampsToPosixct <- function(df, cname){

    df[ ,cname] <- round_date(as.POSIXct(df[ ,cname]/1000, origin="1970-01-01"), "day")
    
    return(df)

    }

getXTS <- function(df, name_of_time_col = "timestamp"){
    df.xts <- xts(df[ ,!colnames(df) %in% name_of_time_col],
                  order.by=df[ ,name_of_time_col])
    
    return(df.xts)
}

getZOO <- function(df, name_of_time_col = "timestamp"){
    
    ## set column types
    df[ , name_of_time_col] <- as.POSIXct(df[ ,name_of_time_col], origin = "1970-01-01")
    df.zoo <- read.zoo(df, format = "%Y-%m-%d")
    
    return(df.zoo)
}



plotIndexEtc <- function(lst,
                         coin,
                         c.price,
                         c.return,
                         c.vol,
                         c.sup,
                         peg,
                         peg.thresh){

    df <- lst[[coin]]

    
    par(mfrow=c(1,1), xpd=FALSE)                                    # choose col

    img <- plot(x = df[ ,c.price],
                subset = "",
                plot.type = "single",
                panels = NULL,
                multi.panel = FALSE,
                col = 1:8,
                up.col = "red",
                dn.col = "black",
                bg = "#FFFFFF",
                type = "l",
                lty = 1,
                lwd = 2,
                lend = 1,
                main = paste(coin, " - ", "Price and other metrics"),
                observation.based = FALSE,
                ylim = NULL, yaxis.same = TRUE,
                yaxis.left = TRUE,
                yaxis.right = FALSE,
                major.ticks = "months",
                minor.ticks = "weeks",
                grid.ticks.on = "months",
                grid.ticks.lwd = 1,
                grid.ticks.lty = 1,
                grid.col = "darkgray",
                labels.col = "#333333",
                format.labels = TRUE,
                grid2 = "#F5F5F5",
                legend.loc = NULL,
                ylab = "Prices in USD")

    if(!missing(peg)) {

        thresh_upper.xts <- getXTS(data.frame(timestamp = index(df), 
                                              thresh_upper = (peg+peg.thresh)))
        thresh_lower.xts <- getXTS(data.frame(timestamp = index(df), 
                                              thresh_upper = (peg-peg.thresh)))

        lines(thresh_upper.xts, type="s", lty=2 , col = 3)
        lines(thresh_lower.xts, type="s", lty=2 , col = 3)
    }
    
    lines(df[ ,c.return], type="l", on=NA, main = c.return)
    lines(df[ ,c.vol], type="l", on=NA, main = c.vol)
    lines(df[ ,c.sup], type="l", on=NA, main = c.sup)
    
    plot(img)

}




getDesc.element <- function(vec){
    desc <- list()
    vec <- as.vector(vec)

    desc[["mean"]] <- mean(vec)
    desc[["sd"]] <- sd(vec)
    desc[["skewness"]] <- skewness(vec)
    desc[["median"]] <- median(vec)

    desc <- lapply(desc, round, digits = 4)

    return(desc)
}                            # sd boundaries

getDesc <- function(df){

    cnames <- colnames(df)
    desc <- lapply(df, getDesc.element)

    return(desc)
    
}


addCoinSupply <- function(df,
                          cap_col = "market_cap_by_available_supply",
                          price_col = "price_usd",
                          replaceInf = TRUE,
                          replaceMult = 2,
                          replaceType = "mult",
                          removeFirstRow = FALSE){

    supply <- df[ ,cap_col, drop = FALSE] / df[ ,price_col, drop = FALSE]
    colnames(supply) <- "available_supply"
    df_new <- cbind(df, supply)

    supply_change <- return.element(as.vector(t(supply)))
    if(replaceInf == TRUE){
    supply_change <- replInfSup(sup = supply_change, multiplicator = replaceMult, type = replaceType)
    }
    supply_change <- as.data.frame(supply_change)
    colnames(supply_change) <- "supply_change_in_procent"
    df_new <- cbind(df_new, supply_change)
    
    if(removeFirstRow == TRUE){df_new <- df_new[-1, ]}
    
    
    return(df_new)

    }


replInfSup <- function(sup, multiplicator = 2, type=c("mult", "na")){

    sup.max <- max(sup)
    sup.min <- min(sup)
    
    if(type == "mult"){
            
        sup[sup == "Inf" ] <- sup.max * multiplicator
        sup[sup == "-Inf" ] <- sup.min * multiplicator
        
    } else if(type == "na"){

        sup[sup == "Inf" ] <- NA
        sup[sup == "-Inf" ] <- NA

    }

    return(sup)
    
}







cleanOutliers <- function(num.sd = 1, R_df ){
  
  timestamp <- R_df$timestamp
  R_df <- R_df[ ,!colnames(R_df) %in% "timestamp", drop = FALSE]
  
  threshold_upper <- function(y){thu <- mean(y, na.rm = TRUE) + num.sd*sd(y, na.rm = TRUE); return(thu)}
  threshold_lower <- function(y){thl <- mean(y, na.rm = TRUE) - num.sd*sd(y, na.rm = TRUE); return(thl)}
  
  R_df <- as.data.frame(lapply(R_df, function(x){replace(x, x > threshold_upper(x), threshold_upper(x))}))
  R_df <- as.data.frame(lapply(R_df, function(x){replace(x, x < threshold_lower(x), threshold_lower(x))}))
  
  R_df <- cbind(timestamp, R_df)
  
  return(R_df)
  
}




plotAcfs <- function(df = RV1d, col, ts_name){
  
  # choose col
  df <- df[ , c("timestamp", col)]

  par(mfrow=c(2*length(col),1), xpd=FALSE)
  
  
  for(i in 2:ncol(df)){
    
    # ACF
    acf <- Acf(getXTS(df[ ,c(1,i)]), lag.max = 100, type = c("correlation"),
               plot = FALSE, na.action = na.contiguous, demean = TRUE)
        
    # PACF
    pacf <- Pacf(getXTS(df[ ,c(1,i)]), lag.max = 100, type = c("correlation"),
               plot = FALSE, na.action = na.contiguous, demean = TRUE)
    
      plot(acf, 
                main = paste("ACF -", ts_name, ": ", colnames(df)[i]))

    
      plot(pacf, 
                main = paste("PACF -", ts_name, ": ", colnames(df)[i]))
    
    
  }

}


addDifferences <- function(df,
                           replaceInf = TRUE,
                           replaceMult = 2,
                           replaceType = "mult",
                           removeFirstRow = TRUE){

    timestamp <- df[ ,"timestamp", drop = FALSE]
    df_wo_time <- df[ ,!colnames(df) %in% "timestamp", drop = FALSE]
    
    D1  <- as.data.frame(lapply(df_wo_time, diff))
    D1  <- rbind(NA, D1) 
    colnames(D1) <- paste0("D1.", colnames(df_wo_time))

    D2  <- as.data.frame(lapply(df_wo_time, diff, differences = 2))
    D2  <-  rbind(NA, NA,  D2) 
    colnames(D2) <- paste0("D2.", colnames(df_wo_time))
    
    D3  <- as.data.frame(lapply(df_wo_time, diff, differences = 3))
    D3  <- rbind(NA, NA, NA, D3)
    colnames(D3) <- paste0("D3.", colnames(df_wo_time))


    if(replaceInf == TRUE){
    D1 <- as.data.frame(lapply(D1, replInfSup, multiplicator = replaceMult, type = replaceType))
    D2 <- as.data.frame(lapply(D2, replInfSup, multiplicator = replaceMult, type = replaceType))
    D3 <- as.data.frame(lapply(D3, replInfSup, multiplicator = replaceMult, type = replaceType))

    }

    df <- cbind(timestamp, df_wo_time, D1, D2, D3)
    
    if(removeFirstRow == TRUE){df <- df[-c(1:3), ]}
    
    
    return(df)

    }




reduceFrequencyTEMP <- function(df){
                                        # collapse data (match timestamps)
    date <- as.POSIXct(df$time , "%Y-%m-%d %H:%M:%S")
    df$week <- strftime(date, format="%Y/W%W")
    
    df_agg <- df %>% 
        mutate(grouper = df$week, "%Y-%m-%d") %>% 
        group_by(grouper) %>%
        summarise(
            timestamp = head(timestamp, n = 1),
            market_cap_by_available_supply = mean(market_cap_by_available_supply, na.rm = TRUE),
            price_usd = mean(price_usd , na.rm = TRUE),
            volume_usd = mean(volume_usd , na.rm = TRUE)) %>%
        as.data.frame()
    df_agg <- df_agg[, colnames(df_agg)[!colnames(df_agg) %in% "grouper"]] 
    return(df_agg)
}
