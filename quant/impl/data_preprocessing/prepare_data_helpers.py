from colorstrings import colorStrings as cs

import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import statsmodels.api as sm
import math
from statsmodels.graphics.tsaplots import plot_pacf
from statsmodels.graphics.tsaplots import plot_acf
from scipy import stats

# parse command line arguments
def parse_args (
):
    parser = argparse.ArgumentParser(
        description=
        'This script gives back first sanity checks and plots'
    )
    #if nargs=1 is enabled, arguments are passed as list instead of string
    parser.add_argument(
        '-i_bc'
        , '--input_file_bc_data'
        , default="../../data/bcdata_raw/velocity_daily.csv"
        , help="Outputfile with market data."
    )
    parser.add_argument(
        '-i_mkt'
        , '--input_file_mkt_data'
        , default="../../data/marketmeasures_raw/marketmeasures.csv"
        , help="Outputfile with market data from exchanges."
    )
    parser.add_argument(
        '-o_bc'
        , '--output_path_data_blockchaindata'
        , default="../../data/bcmeasures_raw/"
        , help="Path for saving the results of the blockchain data."
    )
    parser.add_argument(
        '-o_mkt'
        , '--output_path_data_marketdata'
        , default="../../data/marketmeasures_raw/"
        , help="Path for saving the results of market data."
    )
    parser.add_argument(
        '-o_jnd'
        , '--output_path_data_joineddata'
        , default="../../data/joinedmeasures_raw/"
        , help="Path for saving the results the joined data."
    )
    parser.add_argument(
        '-m'
        , '--mutations'
        , default=""
        , help="Data mutations separated by '_', for example log_diff or diff_diff_log"
    )
    args = parser.parse_args()
    return(args)

def createdirs(dirName):
    if not os.path.exists(dirName):
        os.mkdir(dirName)
        
def calc_V(
    tx_vol,
    tx_corr,
    m,
):
    return (tx_vol - tx_corr)/m

def calc_m_asleep(
    m_total,
    m_circ,
    type="perc",
):
    if type == "perc":
        m_asleep = (m_total - m_circ)/m_total
    elif type == "abs":
        m_asleep = m_total - m_circ
    else:
        raise(ValueError("Wrong input."))
    return m_asleep

def generate_mutations(data, args, logger, crop = False):
    if args.mutations != "":
        mutations = args.mutations.split("_")
        dlist = []
        for mutation in mutations:
            data = mutatedata(
                data=data,
                command=mutation,
                crop=crop
            )
            dlist.append(data)
            logger.info("[Mutation] Did: "+mutation+" on dataset.")
            
        dlist_as_df = pd.concat(dlist, axis = 1)
        df = dlist_as_df.loc[:,~dlist_as_df.columns.duplicated()] # remove duplicated date-columns
    else:
        df = data
    return df

def mutatedata_log(data):
    if data[data.columns.drop("date")].lt(-1).sum().sum() > 0:
        raise(ValueError("Negative values in dataframe."))
    else:
        mdata = np.log(data[data.columns.drop("date")])
        mdata.columns = ["LOG_" + str(col) for col in mdata.columns]
        mdata = pd.concat([data["date"],mdata],axis = 1)
        return mdata

def mutatedata(
    data,
    command,
    crop
):
    if command == "diff":
        mdata=mutatedata_diff(data, crop = crop)
    elif command == "2diff":
        mdata=mutatedata_2diff(data, crop = crop)
    elif command == "log":
        mdata=mutatedata_log(data, crop = crop)
    elif command == "shift":
        mdata=mutatedata_shift(data, crop = crop)
    elif command == "pchange":
        mdata=mutatedata_pchange(data, crop = crop)
    elif command == "difflog":
        mdata=mutatedata_difflog(data, crop = crop)
    elif command == "raw":
        mdata=data
    else:
        raise(ValueError("Wrong input in mutation arguments."))
    return mdata

def mutatedata_diff(
    data,
    crop=True,
):
    mdata = data[data.columns.drop("date")].diff()
    mdata.columns = ["DIFF_" + str(col) for col in mdata.columns]
    mdata = pd.concat([data["date"],mdata],axis = 1)
        
    if crop==True:
        mdata = mdata.dropna()
    return mdata

def mutatedata_2diff(
    data,
    crop=True,
):
    mdata = data[data.columns.drop("date")].diff().diff()
    mdata.columns = ["DIFF_" + str(col) for col in mdata.columns]
    mdata = pd.concat([data["date"],mdata],axis = 1)
        
    if crop==True:
        mdata = mdata.dropna()
    return mdata

def mutatedata_shift(
    data,
    crop=True,
):
    mdata = data[data.columns.drop("date")].shift()
    mdata.columns = ["SHIFT_" + str(col) for col in mdata.columns]
    mdata = pd.concat([data["date"],mdata],axis = 1)
        
    if crop==True:
        mdata = mdata.dropna()
    return mdata


def mutatedata_pchange(
    data,
    crop=True,
):
    if "date" in data.columns:
        cols = data.columns
    else:
        cols = data.columns.drop("date")
    
    nondatecols = data.columns.drop("date")
    mdata = (data[nondatecols]/data[nondatecols].shift(1))-1
    mdata.columns = ["PCHANGE_" + str(col) for col in mdata.columns]
    mdata = pd.concat([data["date"],mdata],axis = 1)

    if crop==True:
        mdata = mdata.dropna()
    return mdata
    
def mutatedata_difflog(
    data,
    crop=True,
):
    nondatecols = data.columns.drop("date")
    mdata = np.log((data[nondatecols]/data[nondatecols].shift(1)))
    mdata.columns = ["DIFFLOG_" + str(col) for col in mdata.columns]
    mdata = pd.concat([data["date"],mdata],axis = 1)

    if crop==True:
        mdata = mdata.dropna()
    return mdata
    
def mkt_data_to_measures(raw_mkt_data, logger, args):
    data = raw_mkt_data

    logger.info("{}[{}Data{}]{}  Read in raw data from R output (CMC wrapper) .".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))
    
    ### Rename panda series's for a new, clean dataframe
    date = pd.to_datetime(
        data["bitcoin.timestamp"],
        format='%Y-%m-%d',
        errors='raise',
        infer_datetime_format=False,
        exact=True
    )
    
    # for comments data  https://blockwatch.cc/databases/markets/COINBASEPRIME:OHLCV
    prices             = data["bitcoin.price_usd"]
    returns            = data["bitcoin.return_wrt_price_usd.simple"]
    volatility         = data["bitcoin.vol.squaredreturns"]
    volume             = data["bitcoin.volume_usd"]
       
    ### Create a dataframe with data to check
    data = pd.concat(
        axis=1,
        keys=["date"
            , "prices"         
            , "returns"
            , "volatility" 
            , "volume"     
        ],
        objs=[date,
              prices,
              returns,
              volatility,
              volume,
        ]
    )
    logger.info("{}[{}Data{}]{}  Finished measures from R output (CMC wrapper) .".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,

    ))
    return data



def bc_data_to_measures(raw_bc_data, logger, args):
    data = raw_bc_data

    logger.info("{}[{}Data{}]{}  Read in raw data from BlockSci output.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))
 
    ### Rename panda series's for a new, clean dataframe
    date = pd.to_datetime(
        data["date"],
        format='%y/%m/%d',
        errors='raise',
        infer_datetime_format=False,
        exact=True
    )
    m_total          = data["m_total"]
    m_circ_wb        = data["m_circ_wh_bill"]
    m_circ_mc_lifo   = data["m_circ_mc_lifo"]
    m_circ_mc_fifo   = data["m_circ_mc_fifo"]
    tx_fees          = data["tx_fees"]
    tx_number        = data["tx_number"]
    tx_vol            = data["tx_vol"]
    tx_vol_peeling    = data["tx_vol_peeling"]
    tx_vol_changeouts = data["tx_vol_changeouts"]
    tx_vol_issues_agg = data["tx_vol_issues_agg"]
    
    tx_corrd  = tx_vol - tx_vol_issues_agg

    ### Calculate velocity
    v_based_on_m_circ_wb_1d         = calc_V(tx_vol  = tx_vol,
                                             tx_corr = tx_corrd,
                                             m       = m_circ_wb)
    v_based_on_m_circ_mc_lifo_1d    = calc_V(tx_vol  = tx_vol,
                                             tx_corr = tx_corrd,
                                             m       = m_circ_mc_lifo)
    v_based_on_m_circ_mc_fifo_1d    = calc_V(tx_vol  = tx_vol,
                                             tx_corr = tx_corrd,
                                             m       = m_circ_mc_fifo)
    v_based_on_m_total              = calc_V(tx_vol  = tx_vol,
                                             tx_corr = tx_corrd,
                                             m       = m_total) 
    # m_asleep = calc_m_asleep(
    #     m_total = m_total,
    #     m_circ = m_circ,
    #     type="abs"
    # )
    # m_asleep_perc = calc_m_asleep(
    #     m_total = m_total,
    #     m_circ = m_circ,
    #     type="perc"
    # )
    

    logger.info("{}[{}Data{}]{}  Calculated velocity.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))
    
    ### Create a dataframe with data to check
    data = pd.concat(
        axis=1,
        keys=[
            "date",
            "m_total",
            "m_circ_wb",
            "m_circ_mc_lifo",
            "m_circ_mc_fifo",
            "tx_fees",
            "tx_number",
            "tx_vol",
            "tx_vol_peeling",
            "tx_vol_changeouts",
            "tx_vol_issues_agg",
            "tx_corrd",
            "v_based_on_m_circ_wb_1d",
            "v_based_on_m_circ_mc_lifo_1d",
            "v_based_on_m_circ_mc_fifo_1d",
            "v_based_on_m_total"
        ],
        objs=[
            date,
            m_total,
            m_circ_wb,
            m_circ_mc_lifo,
            m_circ_mc_fifo,
            tx_fees,
            tx_number,
            tx_vol,
            tx_vol_peeling,
            tx_vol_changeouts,
            tx_vol_issues_agg,
            tx_corrd,
            v_based_on_m_circ_wb_1d,
            v_based_on_m_circ_mc_lifo_1d,
            v_based_on_m_circ_mc_fifo_1d,
            v_based_on_m_total
        ]
    )
    logger.info("{}[{}Data{}]{}  Finished measures from BlockSci output.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))

    return data

