import pandas as pd
import numpy as np
import pandas as pd
import sys
import logging
import math
import pickle

from colorstrings import colorStrings as cs

from prepare_data_helpers import parse_args
from prepare_data_helpers import calc_V
from prepare_data_helpers import calc_m_asleep
from prepare_data_helpers import createdirs
from prepare_data_helpers import mutatedata
from prepare_data_helpers import bc_data_to_measures
from prepare_data_helpers import mkt_data_to_measures
from prepare_data_helpers import generate_mutations

### Add logger
logger = logging.getLogger("Prepare data.");
logging.addLevelName(
    logging.INFO,    "\033[2;36m       \033[1;0m"
    # % logging.getLevelName(logging.INFO)
)

### Setup logger configuration
logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO
)

### Read arguments
args = parse_args()

### Create necessary dirs
createdirs(args.output_path_data_marketdata)
createdirs(args.output_path_data_blockchaindata)
createdirs(args.output_path_data_joineddata)
raw_mkt_data = pd.read_csv(args.input_file_mkt_data)
raw_bc_data = pd.read_csv(args.input_file_bc_data)    

   

#raw_mkt_data = pd.read_csv("../data/mkt_data/raw_from_2018-01-01_to_2018-01-31.csv")
#raw_bc_data = pd.read_csv("../data_test/velocity_daily.csv")


### Read in data
bc_data  = bc_data_to_measures(raw_bc_data = raw_bc_data,
                               logger      = logger,
                               args        = args)
mkt_data = mkt_data_to_measures(raw_mkt_data = raw_mkt_data,
                                logger       = logger,
                                args         = args)
    
### Prepare data
mutated_bc_data  = generate_mutations(bc_data,
                                      logger      = logger,
                                      args        = args,
                                      crop        = False)
mutated_mkt_data = generate_mutations(mkt_data,
                                      logger      = logger,
                                      args        = args,
                                      crop        = False)

bc_data = pd.concat([bc_data, mutated_bc_data], axis=1)
bc_data = bc_data.loc[:,~bc_data.columns.duplicated()]
bc_data = bc_data.dropna()

mkt_data = pd.concat([mkt_data, mutated_mkt_data], axis=1)
mkt_data = mkt_data.loc[:,~mkt_data.columns.duplicated()]
mkt_data = mkt_data.dropna()

data = bc_data.merge(mkt_data,
                     left_on='date',
                     right_on='date',
                     how="inner")
if len(data) == 0:
    logger.info("{}[{}WARNING:{}]{}  No overlap in bc_data and mkt_data.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))
    
### Save data

bc_data.to_pickle(args.output_path_data_blockchaindata + "bc_data.pkl")
bc_data.to_csv(args.output_path_data_blockchaindata + "bc_data.csv")

mkt_data.to_pickle(args.output_path_data_marketdata + "mkt_data.pkl")
mkt_data.to_csv(args.output_path_data_marketdata + "mkt_data.csv")

data.to_pickle(args.output_path_data_joineddata + "data.pkl")
data.to_csv(args.output_path_data_joineddata + "data.csv")

logger.info("{}[{}Data{}]{}  Saved data as pkl-file.".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))
 
