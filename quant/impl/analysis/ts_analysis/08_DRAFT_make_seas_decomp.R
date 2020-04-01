source("SETTINGS.R")
source("helpers.R")
load(file = paste0(SETTINGS$path_data, "tsdata.rda"))

## function that makes diffs
## function that drops NAs
## function that gives periodogram with top frequencies as days
## function that gives ACF 

t <- d$date
d <- d$all[ , c("v_est_m_total", "return_wrt_price_usd.simple")]

d_diff1 <- as.data.frame(sapply(d,diff,differences=1))
colnames(d_diff1) <- paste0(colnames(d_diff1), "_d1")
d_diff2 <- as.data.frame(sapply(d,diff,differences=2))
colnames(d_diff2) <- paste0(colnames(d_diff2), "_d3")
d_diff3 <- as.data.frame(sapply(d,diff,differences=3))
colnames(d_diff3) <- paste0(colnames(d_diff3), "_d3")


s_diff1 <- ts(d_diff1)
s_diff2 <- ts(d_diff2)
s_diff3 <- ts(d_diff3)
plot.ts(s_diff1) # Plot the series
acf(s_diff1)
plot.ts(s_diff2) # Plot the series
acf(s_diff2)
plot.ts(s_diff3) # Plot the series
acf(s_diff3)
var.diff1 <- VAR(s_diff1, type = "none", lag.max = 5, ic = "AIC")
var.diff2 <- VAR(s_diff2, type = "none", lag.max = 5, ic = "AIC")
var.diff3 <- VAR(s_diff3, type = "none", lag.max = 5, ic = "AIC")
res_mtotal_1 <- summary(var.diff1)
res_mtotal_2 <- summary(var.diff2)
res_mtotal_3 <- summary(var.diff3)



t <- dta$time

d <- dta[ , c("v_est_m_circ_wb", "return_wrt_price_usd.simple")]
d_diff1 <- as.data.frame(sapply(d,diff,differences=1))
colnames(d_diff1) <- paste0(colnames(d_diff1), "_d1")
d_diff2 <- as.data.frame(sapply(d,diff,differences=2))
colnames(d_diff2) <- paste0(colnames(d_diff2), "_d3")
d_diff3 <- as.data.frame(sapply(d,diff,differences=3))
colnames(d_diff3) <- paste0(colnames(d_diff3), "_d3")


s       <- ts(d)
s_diff1 <- ts(d_diff1)
s_diff2 <- ts(d_diff2)
s_diff3 <- ts(d_diff3)
plot.ts(s)
plot.ts(s_diff1) # Plot the series
acf(s)
acf(s_diff1)
plot.ts(s_diff2) # Plot the series
acf(s_diff2)
plot.ts(s_diff3) # Plot the series
acf(s_diff3)
var <- VAR(s, type = "none", lag.max = 10, ic = "AIC")
var.diff1 <- VAR(s_diff1, type = "none", lag.max = 10, ic = "AIC")
var.diff2 <- VAR(s_diff2, type = "none", lag.max = 10, ic = "AIC")
var.diff3< <- VAR(s_diff3, type = "none", lag.max = 10, ic = "AIC")
res_m_circ_wb_1 <- summary(var.diff1)
res_m_circ_wb_2 <- summary(var.diff2)
res_m_circ_wb_3 <- summary(var.diff3)

serial.test(var.diff3, lags.pt=10, type="PT.asymptotic")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
serial.test(var, lags.pt=10, type="PT.asymptotic")

res_mtotal_1
res_m_circ_wb_1
res_mtotal_2
res_m_circ_wb_2
res_mtotal_3
res_m_circ_wb_3





######################

# PLAYGROUND FOURIER AND CLEANING
install.packages("TSA")
library(TSA)


decomposed$figure
ts_original  <- decomposed$x[!is.na(decomposed$x)]
ts_detrended <- decomposed$x[!is.na(decomposed$x)]-decom

posed$trend[!is.na(decomposed$x)]


test <- diff(d$all$m_circ_wh_bill)
plot(test, type="l")
acf(test)

p = periodogram(test)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 10)
top2
time = 1/top2$f
time

decomposed <- decompose(ts(test, frequency = 7), "multiplicative")
plot(decomposed)
p <- periodogram(decomposed$random[!is.na(decomposed$random)])
acf(decomposed$random[!is.na(decomposed$random)])
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 10)
top2
time = 1/top2$f
time

#######################

d <- data.frame(v_est_m_circ_wb_clean = as.ts(decomposed$random)[!is.na(decomposed$random)],
                return_wrt_price_usd.simple = dta$return_wrt_price_usd.simple[!is.na(as.ts(decomposed$random))])
s <- ts(d)
var <- VAR(s, type = "none", lag.max = 30, ic = "AIC")
serial.test(var, lags.pt=10, type="PT.asymptotic")

summary(var)
acf(d)
plot(var)


###
acf(diff(dta$m_circ_wh_bill, differences = 3))

decomposed = decompose(ts(dta$m_circ_wh_bill, frequency = 80), "multiplicative")
plot(decomposed)

d <- data.frame(v_est_m_total = as.ts(decomposed$random)[!is.na(decomposed$random)],
                return_wrt_price_usd.simple = dta$return_wrt_price_usd.simple[!is.na(as.ts(decomposed$random))])
acf(d)



s <- ts(d)
var <- VAR(s, type = "none", lag.max = 10, ic = "AIC")
serial.test(var, lags.pt=10, type="PT.asymptotic")

summary(var)
plot(var)

###

plot(dta$v_app_numtx)
plot(dta$v_app_turnover)

##################

