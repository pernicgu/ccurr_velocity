### prepare environment
library(anytime)
library(dplyr)
options(scipen=999)
#### HELPERS ###################################################################
date_indctr <- function(datevec, date){
    ind <- as.Date(datevec, format = "%m/%d/%y") == anydate(date)
    return(ind)
}
get_tx_with_hash <- function(dta, hash){
    idctr <- hash == dta$hash
    return(dta[ idctr, ])
}
get_bdd_of_inpt <- function(amount,timediff){
    return((amount*timediff)/100000000)
}
get_int_txes <- function(df = dta_txly, date = NA){
    if(is.na(date)){
        out <- df[!df$is_coinbase, ]
    }else{
        out <- df[date_indctr(df$time, date) &
                  !df$is_coinbase, ]
    }
    return(out)
}

#### PREPARTIONS ###############################################################
### read in file
dta_bchair    <- read.csv("blockchair_tx_dump.csv", sep="\t")
dta_velo      <- read.csv("data_to_check_2009_2010_full.csv")

### minor preprocessing
                                        # date
dta_velo$date <- paste0("20", as.character(dta_velo$date))
dta_velo$date <- anytime(as.character(dta_velo$date),
                         asUTC = TRUE)

dta_bchair$time <- anytime(as.character(dta_bchair$time),
                           asUTC = TRUE)
                                        # strip big dataframe
dta_txly <- dta_bchair[ ,c("hash",
                           "time",
                           "block_id",
                           "is_coinbase",
                           "input_total",
                           "output_total",
                           "input_count",
                           "output_count",
                           "cdd_total")]
dta_txly$fees                       <- dta_bchair$output_total - dta_bchair$input_total
dta_txly$fees[dta_txly$is_coinbase == 1] <- 0
dta_txly$tx_vol                     <- dta_bchair$output_total

                                        # data shorter
indctr   <- as.numeric(dta_bchair$time) < as.numeric(anytime("2009-01-16"))
dta_txly <- dta_txly[indctr, ]
indctr   <- ((as.numeric(dta_velo$date) < as.numeric(anytime("2009-01-16"))) &
             (as.Date(dta_velo$date) %in% as.Date(dta_txly$time)))
dta_velo <- dta_velo[indctr, ]

### overview over interesting transactions:
no_coinbase_cond <- dta_txly$is_coinbase == FALSE
tx_nocoinbase    <- dta_txly[no_coinbase_cond, ]
tx_nocoinbase

### extract hashes for input and output tx-hash, pubkey and returning Cluster IDs from BlockSci
write.table(tx_nocoinbase,
            file = "blockchair_tx_dump_hashes_nocoinbase.csv",
            row.names = FALSE,
            col.names = TRUE,
            sep = ",",
            quote = FALSE)



#### CALCULATIONS SIMPLE #######################################################
### calculate data that is easy
dta_dly <- dta_txly %>%
    mutate(date = as.Date(time, format = "%m/%d/%y")) %>%
    group_by(date) %>%
    summarize(fees     = sum(fees),
              tx_vol   = sum(tx_vol),
              change   = ifelse(all(is_coinbase == TRUE) || all(is_coinbase | input_total == output_total),0,NA),
              only_cb  = all(is_coinbase),
              tx_number = n(),
              m_total_delta     = sum(output_total[is_coinbase == TRUE]),
              m_circ_mc_lifo_1d = ifelse(all(is_coinbase),0,NA),
              m_circ_mc_fifo_1d = ifelse(all(is_coinbase),0,NA),
              m_circ_wb_1d      = ifelse(all(is_coinbase),0,NA),
              bdd_1d_theirs     = sum(cdd_total),
              bdd_1d_ours       = ifelse(all(is_coinbase),0,NA),
              dormancy_1d       = NA
              )


dta_dly <- as.data.frame(dta_dly)

#### CALCULATIONS NOT SIMPLE ###################################################

#### START OF 2009-01-12 #######################################################
                                        # 1) Initialize beginning values 
DAY_m_circ_mc_lifo_1d <- 0 
DAY_m_circ_mc_fifo_1d <- 0
DAY_m_circ_wb_1d      <- 0
DAY_bdd_1d            <- 0
DAY_dormancy_1d       <- 0

                                        # 2) get txs of day that are interesting
get_int_txes(date="2009-01-12")
                                        # 3) Look through interesting txes and get input hashes
                                        # 3.1)
                                        # for f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16")
tx_temp
system("python3 read_tx_info.py -i 'f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16' -o 'inputs_txes_hashes'")
system("python3 read_tx_info.py -i 'f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16' -o 'changeout_txes_value'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta = dta_txly,
                                    hash = "0437cd7f8525ceed2324359c2d0ba26006d92d856a9c20fa0241106ee5a597c9")
tx_temp_inputtx

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + tx_temp$input_total # aged tx as input 
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + tx_temp$input_total - 1000000000  
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + tx_temp$input_total - 1000000000
dormancy_weight <- tx_temp$input_total /(sum(get_int_txes(date="2009-01-12")$tx_vol))
DAY_dormancy_1d       <- DAY_dormancy_1d + dormancy_weight * get_bdd_of_inpt(amount      = tx_temp$input_total,
                                                                             timediff = tx_temp$time - tx_temp_inputtx$time)
DAY_bdd_1d            <- DAY_bdd_1d + get_bdd_of_inpt(amount   = tx_temp$input_total,
                                                      timediff = tx_temp$time - tx_temp_inputtx$time)
                                        # 3.2)
                                        # for a16f3ce4dd5deb92d98ef5cf8afeaf0775ebca408f708b2146c4fb42b41e14be
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "a16f3ce4dd5deb92d98ef5cf8afeaf0775ebca408f708b2146c4fb42b41e14be")
tx_temp
system("python3 read_tx_info.py -i 'a16f3ce4dd5deb92d98ef5cf8afeaf0775ebca408f708b2146c4fb42b41e14be' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta = dta_txly,
                                    hash = "f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16")
tx_temp_inputtx
system("python3 read_tx_info.py -i 'f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16' -o 'inputs_txes_iscoinb'")

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0 # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d            <- DAY_bdd_1d + 0 
DAY_dormancy_1d       <- DAY_dormancy_1d +0

                                        # 3.3)
                                        # for 591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073")
tx_temp
system("python3 read_tx_info.py -i '591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta = dta_txly,
                                    hash = "a16f3ce4dd5deb92d98ef5cf8afeaf0775ebca408f708b2146c4fb42b41e14be")
tx_temp_inputtx
system("python3 read_tx_info.py -i 'a16f3ce4dd5deb92d98ef5cf8afeaf0775ebca408f708b2146c4fb42b41e14be' -o 'inputs_txes_iscoinb'")

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0 # no coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d            <- DAY_bdd_1d + 0 
DAY_dormancy_1d       <- DAY_dormancy_1d + 0

                                        # 3.4)
                                        # for 12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba")
tx_temp
system("python3 read_tx_info.py -i '12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta = dta_txly,
                                    hash = "591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073")
tx_temp_inputtx
system("python3 read_tx_info.py -i '591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073' -o 'inputs_txes_iscoinb'")

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0 # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d            <- DAY_bdd_1d + 0 
DAY_dormancy_1d       <- DAY_dormancy_1d + 0

                                        # 3.5)
                                        # for 4385fcf8b14497d0659adccfe06ae7e38e0b5dc95ff8a13d7c62035994a0cd79
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "4385fcf8b14497d0659adccfe06ae7e38e0b5dc95ff8a13d7c62035994a0cd79")
tx_temp
system("python3 read_tx_info.py -i '4385fcf8b14497d0659adccfe06ae7e38e0b5dc95ff8a13d7c62035994a0cd79' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta = dta_txly,
                                    hash = "12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba")
tx_temp_inputtx
system("python3 read_tx_info.py -i '12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba' -o 'inputs_txes_iscoinb'")

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0 # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d            <- DAY_bdd_1d + 0 
DAY_dormancy_1d       <- DAY_dormancy_1d + 0

                                        # 3.6)
                                        # for 298ca2045d174f8a158961806ffc4ef96fad02d71a6b84d9fa0491813a776160
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "298ca2045d174f8a158961806ffc4ef96fad02d71a6b84d9fa0491813a776160")
tx_temp
system("python3 read_tx_info.py -i '298ca2045d174f8a158961806ffc4ef96fad02d71a6b84d9fa0491813a776160' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta = dta_txly,
                                    hash = "591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073")
tx_temp_inputtx
system("python3 read_tx_info.py -i '591e91f809d716912ca1d4a9295e70c3e78bab077683f79350f101da64588073' -o 'inputs_txes_iscoinb'")

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0  # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d            <- DAY_bdd_1d + 0 
DAY_dormancy_1d       <- DAY_dormancy_1d + 0

                                        # 3.7)
                                        # for 828ef3b079f9c23829c56fe86e85b4a69d9e06e5b54ea597eef5fb3ffef509fe
tx_temp <- get_tx_with_hash(dta  = dta_txly,
                            hash = "828ef3b079f9c23829c56fe86e85b4a69d9e06e5b54ea597eef5fb3ffef509fe")
tx_temp
system("python3 read_tx_info.py -i '828ef3b079f9c23829c56fe86e85b4a69d9e06e5b54ea597eef5fb3ffef509fe' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx <- get_tx_with_hash(dta  = dta_txly,
                                    hash = "12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba")
tx_temp_inputtx
system("python3 read_tx_info.py -i '12b5633bad1f9c167d523ad1aa1947b2732a865bf5414eab2f9e5ae5d5c191ba' -o 'inputs_txes_iscoinb'")

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0  # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d            <- DAY_bdd_1d + 0 
DAY_dormancy_1d       <- DAY_dormancy_1d + 0


                                        # 4) Put missing data into dataframe
dta_dly[date_indctr(dta_dly$date, "2009-01-12"), "m_circ_mc_lifo_1d"] <- DAY_m_circ_mc_lifo_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-12"), "m_circ_mc_fifo_1d"] <- DAY_m_circ_mc_fifo_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-12"), "m_circ_wb_1d"]      <- DAY_m_circ_wb_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-12"), "bdd_1d_ours"]       <- DAY_bdd_1d
#### END OF 2009-01-12 #########################################################

#### START OF 2009-01-14 #######################################################
                                        # 1) Initialize beginning values 
DAY_m_circ_mc_lifo_1d <- 0 
DAY_m_circ_mc_fifo_1d <- 0
DAY_m_circ_wb_1d      <- 0
DAY_bdd_1d            <- 0
DAY_dormancy_1d       <- 0
                                        # 2) get txs of day that are interesting
get_int_txes(date="2009-01-14")                                        # 3) Look through interesting txes and get input hashes
                                        # 3.1)
                                        # for a3b0e9e7cddbbe78270fa4182a7675ff00b92872d8df7d14265a2b1e379a9d33
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "a3b0e9e7cddbbe78270fa4182a7675ff00b92872d8df7d14265a2b1e379a9d33")
tx_temp
system("python3 read_tx_info.py -i 'a3b0e9e7cddbbe78270fa4182a7675ff00b92872d8df7d14265a2b1e379a9d33' -o 'inputs_txes_hashes'")
system("python3 read_tx_info.py -i 'a3b0e9e7cddbbe78270fa4182a7675ff00b92872d8df7d14265a2b1e379a9d33' -o 'inputs_txes_values'")
system("python3 read_tx_info.py -i 'a3b0e9e7cddbbe78270fa4182a7675ff00b92872d8df7d14265a2b1e379a9d33' -o 'outputs_txes_values'")
                                        # 3.1.1) of .3)
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "264299886446921c89e598ec2b1ec3eab6a2c9b0235b310ff513a039315ff721")
tx_temp_inputtx_1
                                        # 3.1.2) of .3)
tx_temp_inputtx_2 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "4385fcf8b14497d0659adccfe06ae7e38e0b5dc95ff8a13d7c62035994a0cd79")
tx_temp_inputtx_2
                                        # 3.1.3) of .3)
tx_temp_inputtx_3 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "828ef3b079f9c23829c56fe86e85b4a69d9e06e5b54ea597eef5fb3ffef509fe")
tx_temp_inputtx_3


                                        # Question, hwo much of 1,2,3 goes into the TX and what is change for us?
                                        # 1 is coinbase, so only one output and no change-> fully (+50 BTC)
                                        # 2 is has only one in and output. (+1 BTC)
                                        # 3 has two outputs, 18 BTC and 10. The one linked to the tx is 10 BTC. (+10 BTC)
                                        # Also there is only one output, so no change
DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + sum(50,1,10)*100000000
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + sum(50,1,10)*100000000
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + sum(50,1,10)*100000000
dormancy_weight_1 <-  50*100000000/(sum(get_int_txes(date="2009-01-12")$tx_vol))
dormancy_weight_2 <-  1*100000000/(sum(get_int_txes(date="2009-01-12")$tx_vol))
dormancy_weight_3 <-  10*100000000/(sum(get_int_txes(date="2009-01-12")$tx_vol))
DAY_dormancy_1d       <- DAY_dormancy_1d +
    dormancy_weight_1 * get_bdd_of_inpt(amount      = 50*100000000,
                                        timediff = tx_temp$time - tx_temp_inputtx$time) +
    dormancy_weight_2 * get_bdd_of_inpt(amount      = 1*100000000,
                                        timediff = tx_temp$time - tx_temp_inputtx$time) +
    dormancy_weight_3 * get_bdd_of_inpt(amount      = 10*100000000,
                                        timediff = tx_temp$time - tx_temp_inputtx$time)
DAY_bdd_1d <- DAY_bdd_1d +
    get_bdd_of_inpt(amount   = 50*100000000,timediff = tx_temp$time - tx_temp_inputtx_1$time) +
    get_bdd_of_inpt(amount   = 1*100000000,timediff = tx_temp$time - tx_temp_inputtx_2$time)  +
    get_bdd_of_inpt(amount   = 10*100000000,timediff = tx_temp$time - tx_temp_inputtx_3$time)

dta_dly[date_indctr(dta_dly$date, "2009-01-14"), "m_circ_mc_lifo_1d"] <- DAY_m_circ_mc_lifo_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-14"), "m_circ_mc_fifo_1d"] <- DAY_m_circ_mc_fifo_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-14"), "m_circ_wb_1d"]      <- DAY_m_circ_wb_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-14"), "bdd_1d_ours"]       <- DAY_bdd_1d
#### END OF 2009-01-14 #########################################################

#### START OF 2009-01-15 #######################################################
                                        # 1) Initialize beginning values 
DAY_m_circ_mc_lifo_1d <- 0 
DAY_m_circ_mc_fifo_1d <- 0
DAY_m_circ_wb_1d      <- 0
DAY_bdd_1d            <- 0
DAY_dormancy_1d       <- 0

                                        # 2) get txs of day that are interesting
get_int_txes(date="2009-01-15")
                                        # 3) Look through interesting txes and get input hashes
                                        # 3.1) of .8)
                                        # for d71fd2f64c0b34465b7518d240c00e83f6a5b10138a7079d1252858fe7e6b577
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "d71fd2f64c0b34465b7518d240c00e83f6a5b10138a7079d1252858fe7e6b577")
tx_temp
system("python3 read_tx_info.py -i 'd71fd2f64c0b34465b7518d240c00e83f6a5b10138a7079d1252858fe7e6b577' -o 'outputs_txes_values'")
system("python3 read_tx_info.py -i 'd71fd2f64c0b34465b7518d240c00e83f6a5b10138a7079d1252858fe7e6b577' -o 'changeout_txes_value'")
system("python3 read_tx_info.py -i 'd71fd2f64c0b34465b7518d240c00e83f6a5b10138a7079d1252858fe7e6b577' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1)
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "00ff9e64c9a2e7793e6f8c2b04072b4b22648cdedd46cd1c3ae3d6a23c8ec1eb")
tx_temp_inputtx_1

                                        # Question, hwo much goes from 1 into the TX and what is change for us?
                                        # 1 is coinbase so all. ()
                                        # Two outputs, so one is change. Both are 25 BTC
DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + sum(tx_temp_inputtx_1$output_total)
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + tx_temp_inputtx_1$output_total - 2500000000
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + tx_temp_inputtx_1$output_total - 2500000000
dormancy_weight_1 <-  tx_temp_inputtx_1$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
DAY_dormancy_1d       <- DAY_dormancy_1d + dormancy_weight_1 * get_bdd_of_inpt(amount      = tx_temp_inputtx_1$output_total,
                                                                               timediff    = tx_temp$time - tx_temp_inputtx$time)
DAY_bdd_1d            <- DAY_bdd_1d + get_bdd_of_inpt(amount   = tx_temp_inputtx_1$output_total,
                                                      timediff = tx_temp$time - tx_temp_inputtx_1$time)


                                        # 3.2) of .8)
                                        # for 35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055")
tx_temp
system("python3 read_tx_info.py -i '35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055' -o 'inputs_txes_hashes'")
system("python3 read_tx_info.py -i '35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055' -o 'outputs_txes_values'")
system("python3 read_tx_info.py -i '35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055' -o 'changeout_txes_value'")
                                        # 3.1.1) of .1)
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "d71fd2f64c0b34465b7518d240c00e83f6a5b10138a7079d1252858fe7e6b577")
tx_temp_inputtx_1


DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0  # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d <- DAY_bdd_1d + 0


                                        # 3.3) of .8)
                                        # for 28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f")
tx_temp
system("python3 read_tx_info.py -i '28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1) ?? see https://blockstream.info/tx/28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055")
tx_temp_inputtx_1


DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0  # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d <- DAY_bdd_1d + 0


                                        # 3.4) of .8)
                                        # for 6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2")
tx_temp
system("python3 read_tx_info.py -i '6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1) ?? see https://blockstream.info/tx/6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f")
tx_temp_inputtx_1


DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0  # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d <- DAY_bdd_1d + 0


                                        # 3.5) of .8)
                                        # for 3c1d7e82342158e4109df2e0b6348b6e84e403d8b4046d7007663ace63cddb23
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "3c1d7e82342158e4109df2e0b6348b6e84e403d8b4046d7007663ace63cddb23")
tx_temp
system("python3 read_tx_info.py -i '3c1d7e82342158e4109df2e0b6348b6e84e403d8b4046d7007663ace63cddb23' -o 'inputs_txes_hashes'")
                                        # 3.1.1) of .1) ?? see https://blockstream.info/tx/6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2")
tx_temp_inputtx_1

DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + 0  # Not coinbase and from within that day
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + 0
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + 0
DAY_bdd_1d <- DAY_bdd_1d + 0


                                        # 3.6) of .8)
                                        # for 4d6edbeb62735d45ff1565385a8b0045f066055c9425e21540ea7a8060f08bf2
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "4d6edbeb62735d45ff1565385a8b0045f066055c9425e21540ea7a8060f08bf2")
tx_temp
system("python3 read_tx_info.py -i '4d6edbeb62735d45ff1565385a8b0045f066055c9425e21540ea7a8060f08bf2' -o 'inputs_txes_hashes'")
system("python3 read_tx_info.py -i '4d6edbeb62735d45ff1565385a8b0045f066055c9425e21540ea7a8060f08bf2' -o 'inputs_txes_values'")
system("python3 read_tx_info.py -i '4d6edbeb62735d45ff1565385a8b0045f066055c9425e21540ea7a8060f08bf2' -o 'changeout_txes_value'")
system("python3 read_tx_info.py -i '4d6edbeb62735d45ff1565385a8b0045f066055c9425e21540ea7a8060f08bf2' -o 'outputs_txes_values'")
                                        # 3.1.1) of .5)
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "c3f0bb699bcc8a4e0716de45aef74c40aabeb80f7f00b3bdb45e115ee6f5400f")
tx_temp_inputtx_1
                                        # 3.1.2) of .5) 
tx_temp_inputtx_2 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "193b51cd0c5a44bf6593e69fea91e9ddd311f610c5c23187552e3347b275b81b")
tx_temp_inputtx_2
                                        # 3.1.3) of .5) 
tx_temp_inputtx_3 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "b6c967d8f3a3d5fe859a12e9f385531655c2c457326845065fc3942da9e19920")
tx_temp_inputtx_3
                                        # 3.1.4) of .5) 
tx_temp_inputtx_4 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "a739f9909bdf50466fd746e42394fada8e245f29e6f5747fca0a70dec470b75f")
tx_temp_inputtx_4
                                        # 3.1.5) of .5) 
tx_temp_inputtx_5 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "d8bb7a39f85135c14c37c8d370c97d642b907a791dd235793061e86e094c8d96")
tx_temp_inputtx_5


                                        # Question, hwo much of 1,2,3,4,5 goes into the TX and what is change for us?
                                        # 1,2,3,4,5 are coinbase, so all of it
                                        # Only one output. No change. 
inputsum <- sum(tx_temp_inputtx_1$output_total,
                tx_temp_inputtx_2$output_total,
                tx_temp_inputtx_3$output_total,
                tx_temp_inputtx_4$output_total,
                tx_temp_inputtx_5$output_total)
DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + inputsum
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + inputsum
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + inputsum
dormancy_weight_1 <-  tx_temp_inputtx_1$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
dormancy_weight_2 <-  tx_temp_inputtx_2$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
dormancy_weight_3 <-  tx_temp_inputtx_3$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
dormancy_weight_4 <-  tx_temp_inputtx_4$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
dormancy_weight_5 <-  tx_temp_inputtx_5$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
DAY_dormancy_1d       <- DAY_dormancy_1d +
    dormancy_weight_1 * get_bdd_of_inpt(amount      = tx_temp_inputtx_1$output_total,
                                        timediff = tx_temp$time - tx_temp_inputtx_1$time) +
    dormancy_weight_2 * get_bdd_of_inpt(amount      = tx_temp_inputtx_2$output_total,
                                        timediff = tx_temp$time - tx_temp_inputtx_2$time) +
    dormancy_weight_3 * get_bdd_of_inpt(amount      = tx_temp_inputtx_3$output_total,
                                        timediff = tx_temp$time - tx_temp_inputtx_3$time) +
    dormancy_weight_4 * get_bdd_of_inpt(amount      = tx_temp_inputtx_4$output_total,
                                        timediff = tx_temp$time - tx_temp_inputtx_4$time) +
    dormancy_weight_5 * get_bdd_of_inpt(amount      = tx_temp_inputtx_5$output_total,
                                        timediff = tx_temp$time - tx_temp_inputtx_5$time)
DAY_bdd_1d <- DAY_bdd_1d +
    get_bdd_of_inpt(amount   = tx_temp_inputtx_1$output_total,
                    timediff = tx_temp$time - tx_temp_inputtx_1$time) +
    get_bdd_of_inpt(amount   = tx_temp_inputtx_2$output_total,
                    timediff = tx_temp$time - tx_temp_inputtx_2$time) +
    get_bdd_of_inpt(amount   = tx_temp_inputtx_3$output_total,
                    timediff = tx_temp$time - tx_temp_inputtx_3$time) +
    get_bdd_of_inpt(amount   = tx_temp_inputtx_4$output_total,
                    timediff = tx_temp$time - tx_temp_inputtx_4$time) +
    get_bdd_of_inpt(amount   = tx_temp_inputtx_5$output_total,
                    timediff = tx_temp$time - tx_temp_inputtx_5$time)


                                        # 3.7) of .8)
                                        # for 6bf363548b08aa8761e278be802a2d84b8e40daefe8150f9af7dd7b65a0de49f
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "6bf363548b08aa8761e278be802a2d84b8e40daefe8150f9af7dd7b65a0de49f")
tx_temp
system("python3 read_tx_info.py -i '6bf363548b08aa8761e278be802a2d84b8e40daefe8150f9af7dd7b65a0de49f' -o 'inputs_txes_hashes'")
system("python3 read_tx_info.py -i '6bf363548b08aa8761e278be802a2d84b8e40daefe8150f9af7dd7b65a0de49f' -o 'outputs_txes_values'")
system("python3 read_tx_info.py -i '6bf363548b08aa8761e278be802a2d84b8e40daefe8150f9af7dd7b65a0de49f' -o 'changeout_txes_value'")
                                        # 3.1.1) of .1) 
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "e7caf9a784751643f7b71881aaf96e2b3e041950b42638b4fcfe82ff57ba260d")
tx_temp_inputtx_1
                                        # Question, hwo much of 1 goes into the TX and what is change for us?
                                        # 1 is coinbase, so all of it
                                        # Only one output, so no change
DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d + sum(tx_temp_inputtx_1$output_total)
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d + sum(tx_temp_inputtx_1$output_total)
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d + sum(tx_temp_inputtx_1$output_total)
dormancy_weight_1 <- tx_temp_inputtx_1$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
DAY_dormancy_1d       <- DAY_dormancy_1d + dormancy_weight_1 * get_bdd_of_inpt(amount   = tx_temp_inputtx_1$output_total,
                                                                               timediff = tx_temp$time - tx_temp_inputtx_1$time)
DAY_bdd_1d            <- DAY_bdd_1d + get_bdd_of_inpt(amount   = tx_temp_inputtx_1$output_total,
                                                      timediff = tx_temp$time - tx_temp_inputtx_1$time)


                                        # 3.8) of .8)
                                        # for e36f06a8dfe44c3d64be2d3fe56c77f91f6a39da4a5ffc086ecb5db9664e8583
tx_temp <- get_tx_with_hash(dta = dta_txly,
                            hash = "e36f06a8dfe44c3d64be2d3fe56c77f91f6a39da4a5ffc086ecb5db9664e8583")
tx_temp
system("python3 read_tx_info.py -i 'e36f06a8dfe44c3d64be2d3fe56c77f91f6a39da4a5ffc086ecb5db9664e8583' -o 'inputs_txes_hashes'")
system("python3 read_tx_info.py -i 'e36f06a8dfe44c3d64be2d3fe56c77f91f6a39da4a5ffc086ecb5db9664e8583' -o 'outputs_txes_values'")
system("python3 read_tx_info.py -i 'e36f06a8dfe44c3d64be2d3fe56c77f91f6a39da4a5ffc086ecb5db9664e8583' -o 'changeout_txes_value'")
                                        # 3.1.1) of .1) 
tx_temp_inputtx_1 <- get_tx_with_hash(dta = dta_txly,
                                      hash = "04256336e9287f3b46508888cf3539dc0ab2fc8803cbe9668749fd18fc5dee85")
tx_temp_inputtx_1
                                        # Question, hwo much of 1 goes into the TX and what is change for us?
                                        # 1 is coinbase, so all of it
                                        # Two outputs, but both 25 BTC, one is change. No matter which one, as no input recyled.
DAY_m_circ_wb_1d      <- DAY_m_circ_wb_1d  + sum(tx_temp_inputtx_1$output_total)
DAY_m_circ_mc_lifo_1d <- DAY_m_circ_mc_lifo_1d  + sum(tx_temp_inputtx_1$output_total) - 2500000000
DAY_m_circ_mc_fifo_1d <- DAY_m_circ_mc_fifo_1d  + sum(tx_temp_inputtx_1$output_total) - 2500000000
dormancy_weight_1 <- tx_temp_inputtx_1$output_total/(sum(get_int_txes(date="2009-01-12")$tx_vol))
DAY_dormancy_1d       <- DAY_dormancy_1d + dormancy_weight_1 * get_bdd_of_inpt(amount   = tx_temp_inputtx_1$output_total,
                                                                               timediff = tx_temp$time - tx_temp_inputtx_1$time)
DAY_bdd_1d <- DAY_bdd_1d +
    get_bdd_of_inpt(amount   = tx_temp_inputtx_1$output_total,
                    timediff = tx_temp$time - tx_temp_inputtx_1$time)

## DAY_dormancy_1d <- DAY_dormancy_1d + get_dorm_of_inpt(amount   = tx_temp$output_total,
##                                                       timediff = tx_temp$time - tx_temp_inputtx$time)



dta_dly[date_indctr(dta_dly$date, "2009-01-15"), "m_circ_mc_lifo_1d"] <- DAY_m_circ_mc_lifo_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-15"), "m_circ_mc_fifo_1d"] <- DAY_m_circ_mc_fifo_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-15"), "m_circ_wb_1d"]      <- DAY_m_circ_wb_1d
dta_dly[date_indctr(dta_dly$date, "2009-01-15"), "bdd_1d_ours"]       <- DAY_bdd_1d
#### END OF 2009-01-15 #########################################################

#### COMPARISONS -- FINE #######################################################
manu <- dta_dly
auto <- dta_velo

manu$date
auto$date

manu$tx_vol
auto$tx_vol

manu$tx_number
auto$tx_number

manu$m_circ_wb_1d
auto$m_circ_wh_bill

manu$fees
auto$tx_fees

manu$m_circ_mc_lifo_1d
auto$m_circ_mc_lifo
manu$m_circ_mc_fifo_1d
auto$m_circ_mc_fifo

#### COMPARISONS -- FOUND BUGS #################################################

##
manu$bdd_1d_ours
manu$bdd_1d_theirs
auto$sdd_1/100000000





