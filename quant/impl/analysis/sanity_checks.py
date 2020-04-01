import pandas as pd
import numpy as np
import pandas as pd
import sys
import logging
import math
import pickle

from colorstrings import colorStrings as cs

from sanity_checks_helpers import parse_args
# from sanity_checks_helpers import plot_obswise
# from sanity_checks_helpers import plot_datewise
# from sanity_checks_helpers import plot_qq
# from sanity_checks_helpers import plot_hist
# from sanity_checks_helpers import plot_acf_agg
# from sanity_checks_helpers import plot_pacf_agg
from sanity_checks_helpers import createdirs
# from sanity_checks_helpers import plot_pairplots
# from sanity_checks_helpers import plot_corr_heat_map
from sanity_checks_helpers import plot_with_type

### Add logger
logger = logging.getLogger("Sanity Checks");
logging.addLevelName(
    logging.INFO,    "\033[2;36m       \033[1;0m"
    # % logging.getLevelName(logging.INFO)
)

### Setup logger configuration
logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO
)

### Where am I? TODO Dynamically add file for that plotted
logger.info("{}[{}Plotting{}]{}  --------- ".format(
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
#data = pd.read_pickle("./mkt_data.pkl")
data = pd.read_pickle(args.input)
data = data[data.columns.drop(list(data.filter(regex=args.filterout)))]
data = data.dropna()
logger.info("{}[{}Data{}]{}  Read in data from pickle file.".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))

# Plot data
plot_with_type(
    savingpath     = args.output_path,
    data           = data,
    plottype       = "qqplot",
    optional_param = "norm",
    save           = True,
)
plot_with_type(
    savingpath     = args.output_path,
    data           = data,
    plottype       = "qqplot",
    optional_param = "t",
    save           = True,)
plot_with_type(
    savingpath     = args.output_path,
    data           = data,
    plottype       = "histogram",
    optional_param = "-",
    save           = True,
)
plot_with_type(
    savingpath     = args.output_path,
    data           = data,
    plottype       = "obswise",
    optional_param = "-",
    save           = True,
)
plot_with_type(
    savingpath     = args.output_path,
    data           = data,
    plottype       = "datewise",
    optional_param = "-",
    save           = True,
)
plot_with_type(
    savingpath     = args.output_path,
    data           = data,
    plottype       = "acf",
    optional_param = "-",
    save           = True,
)
plot_with_type(
     savingpath     = args.output_path,
    data           = data,
    plottype       = "pacf",
    optional_param = "-",
    save           = True,
)
# plot_with_type(
#     savingpath     = args.output_path,
#     data           = data,
#     plottype       = "pairplots",
#     optional_param = "-",
#     save           = True,
# )
# plot_with_type(
#     savingpath     = args.output_path,
#     data           = data,
#     plottype       = "heatmap",
#     optional_param = "-",
#     save           = True,
# )

### Check for special values and descriptives of data
sys.stdout = open(args.output_path+'data_descriptives.txt', 'w')
pd.set_option('display.max_columns', None)  
print(">> NEGATIVE INFs:")
print(data.eq(-np.inf).sum())

print("------------------------------------------------------")
print(">> POSITIVE INFs:")
print(data.eq(np.inf).sum())
print("------------------------------------------------------")

print(">> NANs:")
print(data.eq(np.nan).sum())
print("------------------------------------------------------")

print(">> ZEROs:")
print(data.eq(0).sum())
print("------------------------------------------------------")

print(">> BASIC DECRIPTIVES:")
print(data.describe())
print("------------------------------------------------------")

logger.info("{}[{}Decriptives{}]{}  Calculated basic descriptives.".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))
