from colorstrings import colorStrings as cs

import numpy as np
import pandas as pd
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import statsmodels.api as sm
import math
import sys
import logging
from scipy import stats
from matplotlib import pyplot as plot
from matplotlib.backends.backend_pdf import PdfPages
from string import ascii_letters
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.stattools import kpss

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

### Matplotlib wants some amazing registration (in the future)
pd.plotting.register_matplotlib_converters()


# parse command line arguments
def parse_args (
):
    parser = argparse.ArgumentParser(
        description=
        'This script gives back first sanity checks and plots'
    )
    #if nargs=1 is enabled, arguments are passed as list instead of string
    parser.add_argument(
        '-i_mkt'
        , '--input_file_mkt_data'
        , default="../../data/marketmeasures_raw/mkt_data.pkl"
        , help="Pickle data object with market time series data"
    )
    parser.add_argument(
        '-i_bc'
        , '--input_file_bc_data'
        , default="../../data/bcmeasures_raw/bc_data.pkl"
        , help="Pickle data object with blockchain time series data"
    )
    parser.add_argument(
        '-o'
        , '--output_path'
        , default="../../output_storage/descriptives_joined_data/"
        , help="Path for saving the results of descriptives."
    )
    parser.add_argument(
        '-f'
        , '--filterout'
        , default="$^"
        , help="Regex expression for columns not to push trough the analysis scripts."
    )
    args = parser.parse_args()
    return(args)

def createdirs(dirName):
    if not os.path.exists(dirName):
        os.makedirs(dirName, exist_ok=True)     

        

def plot_corr_Vs(
        data,
        savingpath,
        save,
        args,
        variable1,
        variable2,
        cs = cs,
        logger = logger
):
    dyn_savingpath = savingpath+'corr_'+variable1+'_'+variable2+'.png'
    # set metadata for figure
    fig = plt.figure()
    ax = fig.add_subplot(1,1,1)
    plt.scatter(data[variable1], data[variable2])
    # Title and labels
    ax.set_title('Correlation between ' + variable1 + ' and '+variable2)
    ax.set_xlabel(variable1)
    ax.set_ylabel(variable2)
    # additonal specs:
    ## set size of image
    fig.set_size_inches(8, 8)
    ## set plots close together
    fig.subplots_adjust(wspace=0.2, hspace=0.4)
    ## draw plot and save
    if save:
        fig.savefig(dyn_savingpath, dpi=100)
    else:
        fig.show()
    
        logger.info("{}[{}Plot{}]{}  Correlation plot.".format(
            cs.RES,
            cs.PRGnBA,
            cs.RES,
            cs.PRGnBA,
        ))



        
# Thanking: https://www.kaggle.com/dedecu/cross-correlation-time-lag-with-pandas

def df_derived_by_shift(df,max_lag=0,cols_not_to_lag=[]):
    df = df.copy()
    if not max_lag:
        return df
    cols ={}
    for i in range(1,max_lag+1):
        for x in list(df.columns):
            if x not in cols_not_to_lag:
                if not x in cols:
                    cols[x] = ['{}_{}'.format(x, i)]
                else:
                    cols[x].append('{}_{}'.format(x, i))
    for k,v in cols.items():
        columns = v
        dfn = pd.DataFrame(data=None, columns=columns, index=df.index)    
        i = 1
        for c in columns:
            dfn[c] = df[k].shift(periods=i)
            i+=1
        df = pd.concat([df, dfn], axis=1)
        df = df.reindex(df.index)
    return df

        
def corr_of_X_with_mutations_of_Y(X,
                                  Z,
                                  max_lag,
                                  cols_not_to_lag = ""):
    df         = pd.concat([X,Z], axis=1)
    df_mutated = df_derived_by_shift(df,
                                     max_lag = max_lag,
                                     cols_not_to_lag = cols_not_to_lag)
    df_mutated = df_mutated.dropna()    
    corrmat    = df_mutated.corr()
    corrmat    = corrmat.loc[:,X.columns]
    #corrdf     = pd.DataFrame(data=corrmat, columns=X.columns, index=corrmat.)
    corrmat    = corrmat.sort_values(by=[X.columns[0]], ascending=False)
    
    return corrmat


def ADFtest(series,
            seriesname,
            logger = logger,
            cs = cs):
    print(series)
    result_packaged = adfuller(series)
    result = pd.Series(result_packaged[0:3], index=['Test Statistic','p-value','Lags Used'])
    for key, value in result_packaged[4].items():
        if key in ["1%","5%","10%"]:
            result = result.append(pd.Series([value], index=["Crit.Value="+str(key)]))
    result = result.append(pd.Series(seriesname, index=["Timeseries"]))
    result = result.append(pd.Series("ADF", index=["Test"]))
    result = result.loc[['Timeseries',
                         'Test',
                         'Test Statistic',
                         'p-value',
                         'Lags Used',
                         "Crit.Value=1%",
                         "Crit.Value=5%",
                         "Crit.Value=10%"]]
    return result
        
def KPSStest(series,
             seriesname,
             logger = logger,
             cs = cs):
    result_packaged = kpss(series, regression='c')
    result = pd.Series(result_packaged[0:3], index=['Test Statistic','p-value','Lags Used'])
    for key, value in result_packaged[3].items():
        if key in ["1%","5%","10%"]:
            result = result.append(pd.Series([value], index=["Crit.Value="+str(key)]))
    result = result.append(pd.Series(seriesname, index=["Timeseries"]))
    result = result.append(pd.Series("KPSS", index=["Test"]))
    result = result.loc[['Timeseries',
                         'Test',
                         'Test Statistic',
                         'p-value',
                         'Lags Used',
                         "Crit.Value=1%",
                         "Crit.Value=5%",
                         "Crit.Value=10%"]]
    return result

def stationarity_testing(data,
                         adf = True,
                         kpss = True,
                         logger = logger,
                         cs = cs,
):
    results = pd.DataFrame(None, columns=['Timeseries',
                                          'Test',
                                          'Test Statistic',
                                          'p-value',
                                          'Lags Used',
                                          'Crit.Value=1%',
                                          'Crit.Value=5%',
                                          'Crit.Value=10%',])
    for col in data.columns:
        if not col == "date":
            series = data.loc[:,col]
            if adf == True:
                results = pd.concat(
                    [results,
                     pd.DataFrame(ADFtest(series = series,
                                          seriesname = col)).T], axis=0, sort=False)
            if kpss == True:
                results = pd.concat(
                    [results,
                     pd.DataFrame(KPSStest(series = series,
                                           seriesname = col)).T], axis=0, sort=False)
    logger.info("{}[{}Stationarity{}]{}  Stationarity tests done.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    ))
    return results
