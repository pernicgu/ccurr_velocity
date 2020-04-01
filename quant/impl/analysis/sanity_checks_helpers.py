from colorstrings import colorStrings as cs

import sys
import logging
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import statsmodels.api as sm
import math
import seaborn as sns
from statsmodels.graphics.tsaplots import plot_pacf
from statsmodels.graphics.tsaplots import plot_acf
from scipy import stats
from matplotlib import pyplot as plot
from matplotlib.backends.backend_pdf import PdfPages
from pandas import Series


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
        '-i'
        , '--input'
        , default="CHANGEME"
        , help="Pickle data object with market or blockchain time series data"
    )
    parser.add_argument(
        '-o'
        , '--output_path'
        , default="CHANGEME"
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


def plot_with_type(
        savingpath,
        data,
        plottype,
        optional_param = "norm",
        save=True,
        cs = cs,
        logger = logger,
):
    #
    
    # set margin
    left = -1.8
    fig = plt.figure()

    # create directory for pictures
    savingpath_super = savingpath + plottype + "/"
    createdirs(savingpath_super)
    
    #for loop to iterate over columns to plot
    for type_i in data.columns.drop("date"):

        # set name and saving path
        dyn_savingpath_sub = savingpath_super+plottype
        
        # create plot
        if plottype == "qqplot":

            if optional_param == "norm":
                optional_paramchoice=stats.norm
                dyn_savingpath = dyn_savingpath_sub+'_'+optional_param+"%"+type_i+".png"
            elif optional_param == "t":
                optional_paramchoice=stats.t
                dyn_savingpath = dyn_savingpath_sub+'_'+optional_param+"%"+type_i+".png"
            else:
                raise ValueError("The distchoice parameter has to be either 'norm' or 't'.")
            ax = fig.add_subplot(1,1,1)
            sm.graphics.qqplot(data[type_i], dist=optional_param, ax=ax, line='45', fit=True)
            top = ax.get_ylim()[1] * 0.75
            txt = ax.text(left, top, type_i, verticalalignment='baseline')
            txt.set_bbox(dict(facecolor='k', alpha=0.1))

        elif plottype == "histogram":

            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   
            
            ax = fig.add_subplot(1,1,1)
            ax.hist(data[type_i], color = 'blue', edgecolor = 'black', bins = 100)
            # Title and labels
            ax.set_title('Histogram: '+ type_i)
            ax.set_xlabel('Misc.')
            ax.set_ylabel('Days')
        
        elif plottype == "acf":

            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   
            
            ax = fig.add_subplot(1,1,1)
            plot_acf(data[type_i], lags=20, ax=ax)
            # Title and labels
            ax.set_title('ACF: '+ type_i)
            ax.set_xlabel('Lag.')
            ax.set_ylabel('Correlation')

        elif plottype == "pacf":
            
            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   

            ax = fig.add_subplot(1,1,1)
            plot_pacf(data[type_i], lags=20, ax=ax, method='ldb')
            # Title and labels
            ax.set_title('PACF: '+ type_i)
            ax.set_xlabel('Lag.')
            ax.set_ylabel('Correlation')

        elif plottype == "obswise":

            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   
            
            ax = fig.add_subplot(1,1,1)
            ax.plot(data[type_i])
            # Title and labels
            ax.set_title('Obswise plot: '+ type_i)
            ax.set_xlabel('Value')
            ax.set_ylabel('Observations')

        elif plottype == "datewise":

            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   

            ax = fig.add_subplot(1,1,1)
            ax.plot(data["date"], data[type_i])
            # Title and labels
            ax.set_title('Obswise plot: '+ type_i)
            ax.set_xlabel('Value')
            ax.set_ylabel('Observations')
    
        elif plottype == "pairplots":

            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   

            sns.set(style="white")        
            fig = sns.pairplot(data)
            
        
        elif plottype == "heatmap":

            dyn_savingpath = dyn_savingpath_sub+"%"+type_i+".png"   

            # thanking https://seaborn.pydata.org/examples/many_pairwise_correlations.html
            sns.set(style="white")

            data_nodate = data.loc[:data.columns.drop("date")]
            
            # Compute the correlation matrix
            corr = data_nodate.corr()
            
            # Generate a mask for the upper triangle
            mask = np.zeros_like(corr, dtype=np.bool)
            mask[np.triu_indices_from(mask)] = True
            
            # Set up the matplotlib figure
            f, ax = plt.subplots(figsize=(12, 12))
            
            # Generate a custom diverging colormap
            cmap = sns.diverging_palette(220, 10, as_cmap=True)
            
            # Draw the heatmap with the mask and correct aspect ratio
            snsfig = sns.heatmap(corr, mask=mask, cmap=cmap, vmax=.3, center=0,
                                 square=True, linewidths=.5, cbar_kws={"shrink": .5})
            fig = snsfig.get_figure()
            fig.set_size_inches(20, 20)
            

        # draw or save plot
        if save:
            fig.savefig(dyn_savingpath, dpi=100)
            fig.clf()
        else:
            fig.show()

        if plottype == "pairplots" or plottype == "heatmap":
            break
        
    logger.info("{}[{}Plot{}]{}  %s.".format(
        cs.RES,
        cs.PRGnBA,
        cs.RES,
        cs.PRGnBA,
    )%(plottype.upper()))
        
        
