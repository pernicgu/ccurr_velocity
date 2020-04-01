makeDevTable <- function(dlist = data_list,
                         peg = peg,
                         devs = devs){

    .makeDev <- function(peg, devs, data_vec){
        viol_perc <- sapply(devs, function(dev){round(sum(data_vec < peg - dev | data_vec > peg + dev)/length(data_vec), digits = 4)*100})
        names(viol_perc) <- devs
        return(viol_perc)
    }
    dev_list <- lapply(dlist, function(l){.makeDev(l[["price_usd"]],
                                                  peg = peg,
                                                  devs = devs)})
    tbl <- t(as.data.frame(dev_list, check.names = FALSE))
    tbl <- round(tbl, digits = 2)
    tbl <- tbl[order(rowSums(tbl)), ]
    return(tbl)
}

makeDescTblForAll <- function(dta = data_list_nooutliers,
                              subselection = NULL){
    xts_list <- lapply(dta, getXTS)
    df <- xts_list[[1]]$price_usd

    .getDescTbl <- function(df,
                            vars){
        tbl <- NULL
        for(var in vars){
            df_sub <- df[,var]
            row <- psych::describe(df_sub)
            tbl <- rbind(tbl, row)
        }
        rownames(tbl) <- vars
        return(tbl)    
    }

    ## write table
    desctbls <- lapply(xts_list, .getDescTbl, c("price_usd"))
    desctbls <- do.call(rbind, desctbls)

    ## add daily average volatility
    desctbls$dailyvol_avg <- unname(unlist(lapply(data_list, function(x){mean(x[["vol.squaredreturns"]])})))
    
    ## select rows and columns
    desctbls <- desctbls[ ,c("n", "mean", "min", "max", "sd", "dailyvol_avg")]
    colnames(desctbls) <- c("Obs.", "Mean","Min.", "Max.", "Vol. (annualized)", "Vol. (daily averaged)")
    
    if(is.null(subselection) == FALSE){
        desctbls <- desctbls[subselection, ]
    }

    return(desctbls)
}

plotSupplyRelation <- function(lst,
                               comp,
                               coin,
                               share,
                               c.price,
                               c.return,
                               c.vol,
                               c.sup,
                               max_lag,
                               plotname,
                               title_string){

                                        # %% PLOT TS %%
    
                                        # prepare dataframes of interest
    df_comp <- lst[[comp]]
    df_coin <- lst[[coin]]
    df_share <- lst[[share]]

                                        # prepare max feasible nrow
    min_length <- min(nrow(df_coin), nrow(df_share))

                                        # prepare dateframes
    df_comp <- tail(df_comp, min_length)
    colnames(df_comp) <- paste("COMPARISON.", colnames(df_comp), sep = "")
    index(df_comp) <- round(index(df_comp),"day")

    df_coin <- tail(df_coin, min_length)
    colnames(df_coin) <- paste("COIN.", colnames(df_coin), sep = "")
    index(df_coin) <- round(index(df_coin),"day")

    df_share <- tail(df_share, min_length)
    colnames(df_share) <- paste("SHARE.", colnames(df_share), sep = "")
    index(df_share) <- round(index(df_share),"day")

                                        #merge dataframes
    df <- merge(df_comp, df_coin, df_share ,all=FALSE)
    indexTZ(df) <- "UTC"
    index(df) <- round(index(df), unit = "day")

                                        png(file = paste(wd_home,"plots", plotname, sep ="/"), width = 25, height = 12.5, units = "cm", res = 100)


    par(mfrow=c(1,2), xpd=FALSE, oma = c(1,1,1,1))                                    # choose col
                                        #par(mar=c(10,10,10,10)-2)
    

    img <- plot(x = df[ ,paste("COIN.", c.price, sep = "")],
                screens = 1,
                plot.type = "single",
                panels = NULL,
                multi.panel = FALSE,
                col = 1:8,
                main = "",
                lwd = 2,
                observation.based = FALSE,
                ylim = NULL,
                yaxis.same = TRUE,
                yaxis.left = TRUE,
                yaxis.right = FALSE,
                major.ticks = "years",
                minor.ticks = NULL,
                grid.ticks.on = "years",
                grid.ticks.lwd = 1,
                grid.ticks.lty = 1,
                grid.col = "darkgray",
                grid2 = "#F5F5F5",
                legend.loc = NULL,
                ylab = "Prices in USD",
                bty = "l",
                cex = 0.5)
    

    lines(df[ ,paste("COIN.", c.sup, sep = "")], type="l", on=NA, main = "Coin Supply in Coins Issued")

    plot(img)

    mtext(adj = 0, bquote(bold(.(title_string))), side=3, line=3, col="black", cex=1)

    
                                        # %% PLOT CORRELATIONS %%


                                        # Input variables


    
    supply <- df$COIN.D1.available_supply
    price <- df$COIN.D1.price_usd

    ## cross correlation function, giving the correlation coeffienct between timeseries at t and t+k, where k can be positive or negative.

    ccf_dat <- ccf(data_list[[coin]]$D1.available_supply,
                   data_list[[coin]]$D1.price_usd,
                   lag.max = max_lag,
                   plot = TRUE,
                   bty = "l",
                   axes = FALSE,
                   xlab = "",
                   ylab = "",
                   main = "",
                   ylim = c(-0.5,0.5))
    axis(1, col="grey50", col.ticks="grey50", col.axis="grey20", cex.axis=0.8, bty = "l")
    axis(2, col="grey50", col.ticks="grey50", col.axis="grey20", cex.axis=0.8, bty = "l")
    mtext(expression(paste("Lag of ",Delta," Supply")), side=1, line=3, col="black", cex=0.8)
    mtext(expression(paste("Correlation of the resp. Lag of ",Delta," Supply with ", Delta, " Price")), side=2, line=3, col="black", cex=0.8)
    
    mtext(adj = 0, expression(bold("Cross Correlation Function")), side=3, line=3, col="black", cex=1)

    dev.off()
    
}


                         
plotIndexEtc <- function(lst,
                         coins_sel,
                         coins_sel_lable,
                         c.price,
                         c.return,
                         c.vol,
                         c.sup,
                         peg,
                         peg.thresh,
                         title_vec){
    df <- NULL
    for(coin in coins_sel){

        add <- lst[[coin]]
        colnames(add) <- paste(coin, colnames(add), sep=".")
        indexTZ(add) <- "UTC"

        
        if(!is.null(df)){
            df <- merge(df, add, all = TRUE, check.names = FALSE)
        } else {
            df <- add
        }
    }
    index(df) <- round(index(df),unit="day")
    
    greys <- grep("^grey", colours(), value = TRUE)
    date_sel <- seq(as.POSIXct("2018-01-01", tz = "UTC"), max(index(df), na.rm = TRUE), by = "days")
    data <- tail(df[date_sel , as.vector(outer(coins_sel, c.price, paste, sep="."))], n = 365)
    extreme_val <- max(max(data , na.rm = TRUE)-1 , 1-min(data , na.rm = TRUE))
    
    
    
    img <- plot(x = data,
                subset = "",
                plot.type = "single",
                panels = NULL,
                multi.panel = FALSE,
                col = greys[round(seq(1, 102, 102/length(coins_sel)))],#rainbow(10, start = 0.35, end = 0.6)[sample(1:10, 100, TRUE)],
                bg = "#FFFFFF",
                type = "l",
                lty = 1,#10:1,
                lwd = 2,
                lend = 1,
                main = "",
                observation.based = FALSE,
                ylim = c(1-extreme_val, 1+extreme_val),
                yaxis.same = TRUE,
                yaxis.left = TRUE,
                yaxis.right = FALSE,
                major.ticks = "months",
                minor.ticks = NULL,
                grid.ticks.on = "months",
                grid.ticks.lwd = 1,
                grid.ticks.lty = 1,
                grid.col = "darkgray",
                labels.col = "#333333",
                format.labels = TRUE,
                grid2 = "#F5F5F5",
                legend.loc = NULL,
                cex = 0.5)

    addLegend("topleft", on=1, 
              legend.names = coins_sel_lable,
              lty=c(1, 1), lwd=c(3, 3),
              col = greys[round(seq(1, 102, 102/length(coins_sel)))])

   
    if(!missing(peg)) {

        thresh_upper.xts <- getXTS(data.frame(timestamp = index(df), 
                                              thresh_upper = (peg+peg.thresh)))
        thresh_lower.xts <- getXTS(data.frame(timestamp = index(df), 
                                              thresh_upper = (peg-peg.thresh)))

        lines(thresh_upper.xts, type="s", lty=2 , col = 1, lwd = 2)
        lines(thresh_lower.xts, type="s", lty=2 , col = 1, lwd = 2)
    }

    
    plot(img)

    mtext(adj = 0, bquote(bold(.(title_vec))), side=3, line=3, col="black", cex=0.8)
   
}


plotCoinOverview <- function(lst,
                         coin,
                         c.price,
                         c.return,
                         c.vol,
                         c.sup,
                         peg,
                         peg.thresh,
                         only_prices = FALSE,
                         no_title = FALSE,
                         mindate = FALSE,
                         maxdate = FALSE){

    mindate <- parse_date_time(mindate, orders = c("dmy"))
    maxdate <- parse_date_time(maxdate, orders = c("dmy"))

    if(mindate != FALSE){
        df <- lst[[coin]][index(lst[[coin]]) >= mindate, ]
    }
    if(maxdate != FALSE){
        df <- df[index(df) <= maxdate, ]
    }
    
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
                main = ifelse(no_title,"",paste(coin, " - ", "Price and other metrics")),
                observation.based = FALSE,
                ylim = NULL, yaxis.same = TRUE,
                yaxis.left = TRUE,
                yaxis.right = FALSE,
                major.ticks = "years",
                minor.ticks = "months",
                grid.ticks.on = "years",
                grid.ticks.lwd = 1,
                grid.ticks.lty = 1,
                grid.col = "darkgray",
                labels.col = "#333333",
                format.labels = TRUE,
                grid2 = "#F5F5F5",
                legend.loc = NULL,
                ylab = "Prices in USD")

    if(!missing(peg)) {

        thresh_upper = rep(peg+peg.thresh, length(index(df)))
        thresh_lower = rep(peg-peg.thresh, length(index(df)))
        
        thresh_upper.xts <- xts(x = thresh_upper, 
                                order.by = index(df))
        thresh_lower.xts <- xts(x = thresh_lower, 
                                order.by = index(df))
        
        lines(thresh_upper.xts, type="s", lty=2 , col = 3, lwd=3)
        lines(thresh_lower.xts, type="s", lty=2 , col = 3, lwd=3)
    }

    if(!only_prices){
        lines(df[ ,c.return], type="l", on=NA, main = c.return)
        lines(df[ ,c.vol], type="l", on=NA, main = c.vol)
        lines(df[ ,c.sup], type="l", on=NA, main = c.sup)
    }
    
    plot(img)

}




makeVolaRanking <- function(dta, desc, coins, tail_length = 365){
    
    vol_ranking <- NULL

    for (coin in coins){

        if(!is.null(tail_length)){
            
            if(all(is.na(dta[[coin]])) == FALSE && nrow(dta[[coin]]) >= tail_length){
                add_lst.p1<- desc[[coin]][["return_wrt_price_usd.simple"]] 
                add_vol_ranking.p1 <- as.data.frame(add_lst.p1)
                add_lst.p2<- desc[[coin]][["vol.squaredreturns"]] 
                add_vol_ranking.p2 <- as.data.frame(add_lst.p2)
                add_lst.p3<- desc[[coin]][["volume_usd"]] 
                add_vol_ranking.p3 <- as.data.frame(add_lst.p3)
                add_lst.p4<- desc[[coin]][["market_cap_by_available_supply"]] 
                add_vol_ranking.p4 <- as.data.frame(add_lst.p4)


                add_vol_ranking <- cbind(add_vol_ranking.p1,
                                         add_vol_ranking.p2[, "mean", drop = FALSE],
                                         add_vol_ranking.p3[ ,"mean", drop = FALSE],
                                         add_vol_ranking.p4[ ,"mean", drop = FALSE])
                colnames(add_vol_ranking) <- c(paste0("return_",colnames(add_vol_ranking.p1)),
                                               "volatility_mean", "volume_mean", "market_cap_mean")
                
            } else {
                add_lst <- NA
                add_vol_ranking <- NA
            }
            
        } else {
                add_lst.p1<- desc[[coin]][["return_wrt_price_usd.simple"]] 
                add_vol_ranking.p1 <- as.data.frame(add_lst.p1)
                add_lst.p2<- desc[[coin]][["vol.squaredreturns"]] 
                add_vol_ranking.p2 <- as.data.frame(add_lst.p2)
                add_lst.p3<- desc[[coin]][["volume_usd"]] 
                add_vol_ranking.p3 <- as.data.frame(add_lst.p3)
                add_lst.p4<- desc[[coin]][["market_cap_by_available_supply"]] 
                add_vol_ranking.p4 <- as.data.frame(add_lst.p4)


                add_vol_ranking <- cbind(add_vol_ranking.p1,
                                         add_vol_ranking.p2[, "mean", drop = FALSE],
                                         add_vol_ranking.p3[ ,"mean", drop = FALSE],
                                         add_vol_ranking.p4[ ,"mean", drop = FALSE])
                colnames(add_vol_ranking) <- c(paste0("return_",colnames(add_vol_ranking.p1)),
                                               "volatility_mean", "volume_mean", "market_cap_mean")
                
        }
    
    if(is.null(vol_ranking)){
        vol_ranking <- add_vol_ranking
    }else{
        vol_ranking <- rbind(vol_ranking, add_vol_ranking)
    }
        
}
    
    rownames(vol_ranking) <- coins
    vol_ranking <- vol_ranking[order(vol_ranking$return_sd), ]

return(vol_ranking)

}



element.funProp <- function(dat, cname) {
    df_prop<- dat[, cname]/max(dat[, cname])
    return(df_prop)
}


## plotVolaRanking <- function(ranking1,
##                     ranking2,
##                     titles,
##                     axenames,
##                     features,
##                     feature_lables,
##                     plotname){

##                                         # saving path
    
##     png(file = paste(wd_home,"plots", plotname, sep ="/"), width = 25, height = 12.5, units = "cm", res = 100)

##     # merge rankings
##     ranking1 <- ranking1[order(rownames(ranking1)), ]
##     ranking2 <- ranking2[order(rownames(ranking2)), ]
##     vol_df <- cbind(ranking1, ranking2)
##     colnames(vol_df) <- c(paste0("1.", colnames(ranking2)),
##                           paste0("2.", colnames(ranking1)))

##     # set par
##     par(mfrow=c(1, length(features)), xpd=FALSE, mar=c(12,12,12,12)-1, oma = c(2,2,2,2))
    
##     df <- vol_df[ ,as.vector(outer(c("1", "2"),features, paste, sep=".")), drop = FALSE]
##     df <- df[order(df[ , "2.volatility_mean"]), ]
##     df <- df[rowSums(is.na(df)) != ncol(df), ]

##     # plot
##     for(feature in features){
        
##         img <- barplot(t(as.matrix(df[, as.vector(outer(c("1", "2"),feature, paste, sep="."))])),
##                        yaxt = "n",
##                        names.arg = rownames(df),
##                        las = 2, # rotates labels
##                        beside = TRUE, # all in df in one plot
##                        col = c("grey50", "grey90"))

##         axis(2)
        
##         mtext(axenames[which(feature == features)], side = 2, line=4)

##         title(titles[which(feature == features)])

##         box()

##         legend("topleft", 
##                legend = paste0(feature_lables[which(feature ==features)], c("- wrt. 1 month history","- wrt. 1 year history")), 
##                fill = c("grey50", "grey90"))
##     }

    
##     dev.off()
    
## }




aggregateRankings <- function(tail_len,
                              shorttail_len,
                              dta,
                              coins){ 

    dta_tail <- lapply(dta, function(x){if((nrow(x) < tail_len)){NA}else{tail(x, n = tail_len)}})
    descriptives_tail <- lapply(dta_tail, function(x){if(all(is.na(x))){NA}else{getDesc(x)}})

    dta_shorttail <- lapply(dta, function(x){if((nrow(x) < shorttail_len)){NA}else{tail(x, n = shorttail_len)}})
    descriptives_shorttail <- lapply(dta_shorttail, function(x){if(all(is.na(x))){NA}else{getDesc(x)}})

                                        # make ranking volatility ranking since creation
    vol_ranking_full <- makeVolaRanking(dta = dta,
                                        desc = lapply(dta, getDesc),
                                        coins = coins,
                                        tail_length = NULL)

                                        # using the above shortened data_lists, make ranking volatility ranking since tail_length
    vol_ranking_tail <- makeVolaRanking(dta = lapply(dta, tail, n = tail_len),
                                        desc = lapply(dta_tail, getDesc),
                                        coins = coins,
                                        tail_length = tail_len)

                                        # using the above shortened data_lists, make ranking volatility ranking since shorttail_length
    vol_ranking_shorttail <- makeVolaRanking(dta = dta_shorttail,
                                             desc = lapply(dta_shorttail, getDesc),
                                             coins = coins,
                                             tail_length = shorttail_len)

                                        # add a Ratio between average trading volume on exchanges and market capitalization
    vol_ranking_full$RatioVolCap <- vol_ranking_full$volume_mean/vol_ranking_full$market_cap_mean
    vol_ranking_tail$RatioVolCap <- vol_ranking_tail$volume_mean/vol_ranking_tail$market_cap_mean
    vol_ranking_shorttail$RatioVolCap <- vol_ranking_shorttail$volume_mean/vol_ranking_shorttail$market_cap_mean

    return(list(ranking_full = vol_ranking_full,
                ranking_tail = vol_ranking_tail,
                ranking_shorttail = vol_ranking_shorttail))
}


makeVolaRanking <- function(dta, desc, coins, tail_length = 365){
    
    vol_ranking <- NULL

    for (coin in coins){

        if(!is.null(tail_length)){
            
            if(all(is.na(dta[[coin]])) == FALSE && nrow(dta[[coin]]) >= tail_length){
                add_lst.p1<- desc[[coin]][["return_wrt_price_usd.simple"]] 
                add_vol_ranking.p1 <- as.data.frame(add_lst.p1)
                add_lst.p2<- desc[[coin]][["vol.squaredreturns"]] 
                add_vol_ranking.p2 <- as.data.frame(add_lst.p2)
                add_lst.p3<- desc[[coin]][["volume_usd"]] 
                add_vol_ranking.p3 <- as.data.frame(add_lst.p3)
                add_lst.p4<- desc[[coin]][["market_cap_by_available_supply"]] 
                add_vol_ranking.p4 <- as.data.frame(add_lst.p4)


                add_vol_ranking <- cbind(add_vol_ranking.p1,
                                         add_vol_ranking.p2[, "mean", drop = FALSE],
                                         add_vol_ranking.p3[ ,"mean", drop = FALSE],
                                         add_vol_ranking.p4[ ,"mean", drop = FALSE])
                colnames(add_vol_ranking) <- c(paste0("return_",colnames(add_vol_ranking.p1)),
                                               "volatility_mean", "volume_mean", "market_cap_mean")
                
            } else {
                add_lst <- NA
                add_vol_ranking <- NA
            }
            
        } else {
                add_lst.p1<- desc[[coin]][["return_wrt_price_usd.simple"]] 
                add_vol_ranking.p1 <- as.data.frame(add_lst.p1)
                add_lst.p2<- desc[[coin]][["vol.squaredreturns"]] 
                add_vol_ranking.p2 <- as.data.frame(add_lst.p2)
                add_lst.p3<- desc[[coin]][["volume_usd"]] 
                add_vol_ranking.p3 <- as.data.frame(add_lst.p3)
                add_lst.p4<- desc[[coin]][["market_cap_by_available_supply"]] 
                add_vol_ranking.p4 <- as.data.frame(add_lst.p4)


                add_vol_ranking <- cbind(add_vol_ranking.p1,
                                         add_vol_ranking.p2[, "mean", drop = FALSE],
                                         add_vol_ranking.p3[ ,"mean", drop = FALSE],
                                         add_vol_ranking.p4[ ,"mean", drop = FALSE])
                colnames(add_vol_ranking) <- c(paste0("return_",colnames(add_vol_ranking.p1)),
                                               "volatility_mean", "volume_mean", "market_cap_mean")
                
        }
    
    if(is.null(vol_ranking)){
        vol_ranking <- add_vol_ranking
    }else{
        vol_ranking <- rbind(vol_ranking, add_vol_ranking)
    }
        
}
    
    rownames(vol_ranking) <- coins
    vol_ranking <- vol_ranking[order(vol_ranking$return_sd), ]

return(vol_ranking)

}



element.funProp <- function(dat, cname) {
    df_prop<- dat[, cname]/max(dat[, cname])
    return(df_prop)
}


plotVolaRanking <- function(ranking1,
                    ranking2,
                    titles,
                    axenames,
                    features,
                    feature_lables,
                    plotname){

                                        # saving path
    
    png(file = paste(wd_home,"plots", plotname, sep ="/"), width = 25, height = 12.5, units = "cm", res = 100)

    # merge rankings
    ranking1 <- ranking1[order(rownames(ranking1)), ]
    ranking2 <- ranking2[order(rownames(ranking2)), ]
    vol_df <- cbind(ranking1, ranking2)
    colnames(vol_df) <- c(paste0("1.", colnames(ranking2)),
                          paste0("2.", colnames(ranking1)))

    # set par
    par(mfrow=c(1, length(features)), xpd=FALSE, cex = 0.7)
    par(mar=c(8,4,4,4)-1)
    
    df <- vol_df[ ,as.vector(outer(c("1", "2"),features, paste, sep=".")), drop = FALSE]
    df <- df[order(df[ , "2.volatility_mean"]), ]
    df <- df[rowSums(is.na(df)) != ncol(df), ]

    # plot
    for(feature in features){
        
        img <- barplot(t(as.matrix(df[, as.vector(outer(c("1", "2"),feature, paste, sep="."))])),
                       yaxt = "n",
                       names.arg = rownames(df),
                       las = 2, # rotates labels
                       beside = TRUE, # all in df in one plot
                       col = c("grey50", "grey90"))

        axis(2)
        
        mtext(axenames[which(feature == features)], side = 2, line=4, cex = 0.8)

        title(adj = 0, line = 3, titles[which(feature == features)])

        box()

        legend("topleft", 
               legend = paste0(feature_lables[which(feature ==features)], c("- wrt. 1 month history","- wrt. 1 year history")), 
               fill = c("grey50", "grey90"),
               bty = "n")
    }

    
    dev.off()
    
}

plotVolaRanking_simple<- function(ranking1,
                                  ranking2,
                                  titles,
                                  axenames,
                                  feature,
                                  feature_lables){

                                        # saving path
    
  #  png(file = paste(wd_home,"plots", plotname, sep ="/"), width = 25, height = 12.5, units = "cm", res = 100)

    # merge rankings
    ranking1 <- ranking1[order(rownames(ranking1)), ]
    ranking2 <- ranking2[order(rownames(ranking2)), ]
    vol_df <- cbind(ranking1, ranking2)
    colnames(vol_df) <- c(paste0("1.", colnames(ranking2)),
                          paste0("2.", colnames(ranking1)))

    # set par
    #par(mfrow=c(1, length(features)), xpd=FALSE)
    par(mar=c(8,4,4,4)-1, cex = 0.7)
    
    df <- vol_df[ ,as.vector(outer(c("1", "2"),feature, paste, sep=".")), drop = FALSE]
    df <- df[order(df[ , "2.volatility_mean"]), ]
    df <- df[rowSums(is.na(df)) != ncol(df), ]

    # plot
    
    img <- barplot(t(as.matrix(df[, as.vector(outer(c("1", "2"),feature, paste, sep="."))])),
                   yaxt = "n",
                   names.arg = rownames(df),
                   las = 2, # rotates labels
                   beside = TRUE, # all in df in one plot
                   col = c("grey50", "grey90"))

    axis(2)
    
    mtext(axenames[which(feature == feature)], side = 2, line=2.5, cex = 0.8)

    title(adj = 0, line = 2, titles[which(feature == feature)], cex = 1)

    box()

    legend(bty = "n", "topleft", 
           legend = paste0(feature_lables[which(feature ==feature)], c("- wrt. 1 month history","- wrt. 1 year history")), 
           fill = c("grey90", "grey50"))

    #dev.off()    
}



plotCoinPair = function(plotdt, coin1, coin2, shape_dist=10, major_breaks = "2 week", minor_breaks = "1 week",
                        xlab=T) {
  q = ggplot(plotdt, aes(x=timestamp, y=value, color=variable, shape=variable)) +
    geom_line() + geom_point(size=1, data=plotdt[seq(1, nrow(plotdt), shape_dist)]) + 
    xlab("") + ylab("Price [USD]") +
    scale_x_datetime(date_breaks = major_breaks, date_minor_breaks = minor_breaks, date_labels = "%d-%m-%y") +
    scale_color_manual(name="Coin", values=palette, labels=c(coin1, coin2)) +
    scale_shape_discrete(name="Coin", labels=c(coin1, coin2)) +
    geom_hline(yintercept = 1.01, linetype="dashed") +
    geom_hline(yintercept = 0.99, linetype="dashed") +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  return(q)
}

constructPlotDT = function(coin_list, coin1ID, coin2ID) {
  coin1_data = data.table("timestamp" = coin_list[[coin1ID]]$timestamp,
                      V1 = coin_list[[coin1ID]]$price_usd)
  setkey(coin1_data, "timestamp")
  coin2_data = data.table("timestamp" = data_list_nooutliers[[coin2ID]]$timestamp,
                    V2 = data_list_nooutliers[[coin2ID]]$price_usd)
  setkey(coin2_data, "timestamp")
  mergedDT = merge(coin1_data, coin2_data, all=T)
  setnames(mergedDT, c("V1", "V2"), c(coin1ID, coin2ID))
  meltedMerge = melt(mergedDT, id.vars = c("timestamp"))
  return(meltedMerge)
}
