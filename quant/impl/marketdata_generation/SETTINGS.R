###### Load helper funcs #####

source("data_col%hfunc.R")
source("data_proc%hfunc.R")
##### Load packages ####

list_of_packages <- c("httr", "jsonlite", "tidyr", "lubridate", "dplyr", "curl", "ggplot2", "moments", "xts", "data.table", "psych", "stargazer", "tikzDevice") # "tikz"

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)){
    install.packages(new_packages)
} else {
    lapply(list_of_packages, require, character.only = TRUE)       
                                        # (Thanking: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them)
}

##### 
SETTINGS <- list()
SETTINGS$coins <- c("bitcoin")
SETTINGS$date_start <- "1300000000000"
SETTINGS$date_end   <- "1572276932000" #.getTimestamp() #Note: timestamp needs to be frozen, to find the files.
