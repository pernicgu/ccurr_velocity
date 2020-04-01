
import pandas as pd
import numpy as np
import pandas as pd
import sys
import logging
import math
import pickle

from colorstrings import colorStrings as cs

from descriptives_joined_data_helpers import parse_args
from descriptives_joined_data_helpers import createdirs
from descriptives_joined_data_helpers import plot_corr_Vs
from descriptives_joined_data_helpers import corr_of_X_with_mutations_of_Y
from descriptives_joined_data_helpers import df_derived_by_shift
from descriptives_joined_data_helpers import stationarity_testing
### Add logger
logger = logging.getLogger("Simple Descriptives");
logging.addLevelName(
    logging.INFO,    "\033[2;36m       \033[1;0m"
)

### Setup logger configuration
logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO
)

### Where am I? 
logger.info("{}[{}Descriptives for joint data{}]{}  --------- ".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))

### Read arguments
args = parse_args()

### Create necessary dirs
createdirs(args.output_path)
    
### Read data
bc_data = pd.read_pickle(args.input_file_bc_data)
mkt_data = pd.read_pickle(args.input_file_mkt_data)
logger.info("{}[{}Data{}]{}  Read in data from pickle file.".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))
data = bc_data.merge(mkt_data,
                     left_on='date',
                     right_on='date',
                     how="inner")
if len(data) == 0:
    logger.info("{}[{}ERROR{}]{}  No overlap in bc_data and mkt_data.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))
    print("<<<Quit. Go and get better data! >>>")
    quit()

data = data[data.columns.drop(list(data.filter(regex=args.filterout)))]
data = data.dropna()
logger.info("{}[{}Data{}]{}  Merged blockchain and marked data.".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))

plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "v_based_on_m_total",
    variable2 = "v_based_on_m_circ_wb_1d",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "v_based_on_m_total",
    variable2 = "v_based_on_m_circ_mc_lifo_1d",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "v_based_on_m_total",
    variable2 = "v_based_on_m_circ_mc_fifo_1d",
)

plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "returns",
    variable2 = "v_based_on_m_total",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "returns",
    variable2 = "v_based_on_m_circ_wb_1d",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "returns",
    variable2 = "v_based_on_m_circ_mc_lifo_1d",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "returns",
    variable2 = "v_based_on_m_circ_mc_fifo_1d",
)

plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "DIFF_returns",
    variable2 = "DIFF_v_based_on_m_total",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "DIFF_returns",
    variable2 = "DIFF_v_based_on_m_circ_wb_1d",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "DIFF_returns",
    variable2 = "DIFF_v_based_on_m_circ_mc_lifo_1d",
)


plot_corr_Vs(
    savingpath = args.output_path,
    data = data,
    save = True,
    args = args,
    variable1 = "DIFF_returns",
    variable2 = "v_based_on_m_circ_mc_fifo_1d",
)




### Check for special values and descriptives of data
sys.stdout = open(args.output_path+'data_descriptives.txt', 'w')
pd.set_option('display.max_columns', 999)  
pd.set_option('display.max_rows', 999)
pd.set_option('display.width', 150)

print(">> CORRELATION MATRIX - All variables:")
print(data.corr())
print("------------------------------------------------------")
print(">> CORRELATION MATRIX - Returns to lagged : v_based_on_m_circ_wb_1d")
corrmat = corr_of_X_with_mutations_of_Y(X = data.loc[:,["returns"]],
                                        Z = data.loc[:,["v_based_on_m_circ_wb_1d"]],
                                        max_lag = 15)
print(corrmat)

m_circs_for_corrplots = ["v_based_on_m_total"
                         ,"v_based_on_m_circ_mc_fifo_1d"
                         ,"v_based_on_m_circ_mc_lifo_1d"
                         ,"v_based_on_m_circ_wb_1d"
                         ,"v_based_on_m_total"
                         ,"v_based_on_m_circ_mc_fifo_1d"
                         ,"v_based_on_m_circ_mc_lifo_1d"
                         ,"v_based_on_m_circ_wb_1d"]

returns_for_corrplots = ["returns"
                         ,"returns"
                         ,"returns"
                         ,"returns"
                         ,"DIFF_returns"
                         ,"DIFF_returns"
                         ,"DIFF_returns"
                         ,"DIFF_returns"]

for i in range(len(m_circs_for_corrplots)):
    print("------------------------------------------------------")
    print(">> CORRELATION MATRIX - Returns to lagged : "+m_circs_for_corrplots[i])
    corrmat = corr_of_X_with_mutations_of_Y(X = data.loc[:,[returns_for_corrplots[i]]],
                                            Z = data.loc[:,[m_circs_for_corrplots[i]]],
                                            max_lag = 15)
    print(corrmat)
    logger.info("{}[{}Decriptives{}]{}  Correlation on lagged data: %s <-> %s.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    )%(returns_for_corrplots[i],m_circs_for_corrplots[i]))

stationarity_results_table = stationarity_testing(data,
                                                  adf = True,
                                                  kpss = True,
                                                  logger = logger,
                                                  cs = cs)

print(stationarity_results_table)
# print("------------------------------------------------------")
