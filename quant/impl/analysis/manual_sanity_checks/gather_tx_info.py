#!/usr/bin/env python3
# >>>>>>>> Note: Please start from the directory "crypto_velocity/quant/impl/"
import pickle
import operator
import blocksci
import os
import pandas as pd
import time
import itertools
import numpy as np
from datetime import datetime
from math import ceil
from math import floor
from numpy import concatenate
from numpy import nditer
from numpy import nan
from itertools import compress

def loadSession_heuristics():
    """
        compare https://citp.github.io/BlockSci/reference/heuristics/change.html
        *first* "ChangeHeuristic" objects need to be created
        *second* the objects can be combined
        combined objects form new "ChangeHeuristic" objects that can be called
        see *first*:
    """
    
    heur = []
    heur.append(blocksci.heuristics.change.address_reuse())
    heur.append(blocksci.heuristics.change.address_type())
    heur.append(blocksci.heuristics.change.client_change_address_behavior())
    heur.append(blocksci.heuristics.change.legacy())
    heur.append(blocksci.heuristics.change.locktime())
    heur.append(blocksci.heuristics.change.optimal_change())
    heur.append(blocksci.heuristics.change.peeling_chain())
    heur.append(blocksci.heuristics.change.power_of_ten_value())
    
    # give the objects in the list names
    heur_names = [
        "address_reuse",
        "address_type",
        "client_change_address_behavior",
        "legacy",
        "locktime",
        "optimal_change",
        "peeling_chain",
        "power_of_ten_value"
    ]
    heur = dict(zip(heur_names, heur))
    
    # see *second*
    heur_select = []
    heur_select.append(heur["legacy"])
    heur_select.append(heur["address_type"].__or__(heur["legacy"]))
    
    heur_ret = dict(zip(
        ["legacy", "legacy_improved"],
        heur_select
    ))
    
    return heur_ret

def loadSession_main(inputDataDir,
                     dateformat):
    chain = blocksci.Blockchain(inputDataDir)
    chainRange = chain.range(start = "01/01/2009", end="01/15/2011")
    converter = blocksci.CurrencyConverter()
    btimes = [block.time for block in chainRange]
    txes_range = concatenate(
        [block.txes for block in chainRange], axis=0)
    ret_lst = [chain, chainRange, converter, txes_range]
    ret_lst_names = ["chain", "chainRange", "converter", "txes_range"]
    ret_lst = dict(zip(ret_lst_names, ret_lst))
    return(ret_lst)


def loadSession_clustering(cluster_path,
                           chain):
    cmgr = blocksci.cluster.ClusterManager(cluster_path, chain)
    if not os.path.exists(cluster_path):
        cmgr = "I'm not there for safty!"#cmgr.create_clustering(cluster_path, chain)
    else:
        print("Clustering already exists")
        giveAddressCluster = cmgr.cluster_with_address
        ret_lst = [cmgr, giveAddressCluster]
        ret_lst_names = ["cmgr", "giveAddressCluster"]
        ret_lst = dict(zip(ret_lst_names, ret_lst))
    return(ret_lst)

def loadSession_dirs(export_connection):
    # create directories
    if not os.path.exists(export_connection):
        os.makedirs(export_connection)

# def loadSession_heuristics():
#     """
#     compare https://citp.github.io/BlockSci/reference/heuristics/change.html
#     *first* "ChangeHeuristic" objects need to be created
#     *second* the objects can be combined
#     combined objects form new "ChangeHeuristic" objects that can be called
#     see *first*:
#     """
#     heur = []
#     heur.append(bs.heuristics.change.address_reuse())
#     heur.append(bs.heuristics.change.address_type())
#     heur.append(bs.heuristics.change.client_change_address_behavior())
#     heur.append(bs.heuristics.change.legacy())
#     heur.append(bs.heuristics.change.locktime())
#     heur.append(bs.heuristics.change.optimal_change())
#     heur.append(bs.heuristics.change.peeling_chain())
#     heur.append(bs.heuristics.change.power_of_ten_value())
    
#     # give the objects in the list names
#     heur_names = [
#         "address_reuse",
#         "address_type",
#         "client_change_address_behavior",
#         "legacy",
#         "locktime",
#         "optimal_change",
#         "peeling_chain",
#         "power_of_ten_value"
#     ]
#     heur = dict(zip(heur_names, heur))
    
#     # see *second*
#     heur_select = []
#     heur_select.append(heur["legacy"])
#     heur_select.append(heur["address_type"].__or__(heur["legacy"].__or__(heur["peeling_chain"])))
#     # heur_select.append(heur["address_reuse"].__or__(
#     #     heur["address_type"].__or__(
#     #         heur["client_change_address_behavior"].__or__(
#     #             heur["legacy"].__or__(
#     #                 heur["locktime"].__or__(
#     #                     heur["optimal_change"].__or__(
#     #                         heur["peeling_chain"].__or__(
#     #                             heur["power_of_ten_value"]))))))))
    
#     heur_ret = dict(zip(
#         ["legacy", "legacy_improved"],
#     heur_select
#     ))
    
#     return heur_ret

# >>>>>>>> START: User Input - please adapt
cluster_path = "/home/usr_btc/cluster"
inputDataDir = "/home/usr_btc/txdata"
dateformat   = "%Y-%m-%d %H:%M:%S"
check_path   = os.getcwd()+"/quant/impl/analysis/manual_sanity_checks"

hashfile_path = check_path+"/blockchair_tx_dump_hashes_nocoinbase.csv"
heur_input = "legacy"  # see modules for definition of heuristics
# <<<<<<<< END


# >>>>>>>> START: Load main objects for using BlockSci
#loadSession_dirs(export_connection=export_connection)
BS_main = loadSession_main(inputDataDir = inputDataDir,
                           dateformat   = dateformat)

BS_clustering = loadSession_clustering(cluster_path = "/home/pernicgu/cashed",
                                       chain        = BS_main["chain"])

BS_heuristics = loadSession_heuristics()

# Read hash csv file to a dataframe with custom delimiter
hashes =  pd.read_csv(hashfile_path, sep=','  , engine='python')
hashes = hashes["hash"].tolist()
hashes 
# <<<<<<<< END

inputs_txes_hashes    = []
inputs_txes_iscoinb   = []
inputs_txes_clusters  = [] 
inputs_txes_values    = []
outputs_txes_hashes   = []
outputs_txes_clusters = []
outputs_txes_values   = []
changeout_txes_value  = []
changeout_txes_pkey   = []
for hash_i in hashes:
    try:
        tx_obj = BS_main["chain"].tx_with_hash(str(hash_i))
        inputs_tx_hashes    = tx_obj.inputs.spent_tx.hash
        inputs_tx_iscoinb   = tx_obj.inputs.spent_tx.is_coinbase
        inputs_tx_values    = list(tx_obj.inputs.value)
        inputs_tx_clusters  = [BS_clustering["cmgr"].cluster_with_address(adrs).index
                               for adrs in tx_obj.inputs.address.all]
        changeout_tx        = BS_heuristics["legacy"].change(tx_obj)
        if len(changeout_tx) > 0:
            changeout_tx_value = [list(changeout_tx)[0].value]
        else:
            changeout_tx_value = "-"
        if len(changeout_tx) > 0:
            changeout_tx_pkey = [list(changeout_tx)[0].address.pubkey]
        else:
            changeout_tx_pkey = "-"        
        outputs_tx_hashes   = tx_obj.outputs.spending_tx.hash.all
        outputs_tx_values   = list(tx_obj.outputs.value)
        outputs_tx_clusters = [BS_clustering["cmgr"].cluster_with_address(adrs).index
                               for adrs in tx_obj.outputs.address.all]
        # print(tx_obj)
        # print(inputs_tx_hashes)
        # print(inputs_tx_clusters)
        # print(outputs_tx_hashes)
        # print(outputs_tx_clusters)
    except RuntimeError:
        inputs_tx_hashes    = "hash not found"
        inputs_tx_iscoinb   = "hash not found"
        inputs_tx_clusters  = "hash not found" 
        inputs_tx_values    = "hash not found" 
        outputs_tx_hashes   = "hash not found"
        outputs_tx_clusters = "hash not found"
        outputs_tx_values   = "hash not found"
        changeout_tx_val    = "hash not found"
        changeout_tx_pkey   = "hash not found"
        
    inputs_txes_hashes.append(inputs_tx_hashes if len(inputs_tx_hashes) > 0 else np.nan)
    inputs_txes_iscoinb.append(inputs_tx_iscoinb if len(inputs_tx_iscoinb) > 0 else np.nan)
    inputs_txes_clusters.append(inputs_tx_clusters if len(inputs_tx_clusters) > 0 else np.nan) 
    inputs_txes_values.append(inputs_tx_values if len(inputs_tx_values) > 0 else np.nan) 
    outputs_txes_hashes.append(outputs_tx_hashes if len(outputs_tx_hashes) > 0 else np.nan)
    outputs_txes_clusters.append(outputs_tx_clusters if len(outputs_tx_clusters) > 0 else np.nan)
    outputs_txes_values.append(outputs_tx_values if len(outputs_tx_values) > 0 else np.nan)
    changeout_txes_value.append(changeout_tx_value if len(changeout_tx_value) > 0 else np.nan)
    changeout_txes_pkey.append(changeout_tx_pkey if len(changeout_tx_pkey) > 0 else np.nan)

outerdict = {}
for i in range(len(hashes)):
    innerdict = {}
    innerdict["inputs_txes_hashes"]   = inputs_txes_hashes[i]
    innerdict["inputs_txes_iscoinb"] = inputs_txes_iscoinb[i]
    innerdict["inputs_txes_clusters"] = inputs_txes_clusters[i]
    innerdict["inputs_txes_values"]   = inputs_txes_values[i]
    innerdict["outputs_txes_hashes"]  = outputs_txes_hashes[i]
    innerdict["outputs_txes_clusters"]= outputs_txes_clusters[i]
    innerdict["outputs_txes_values"]  = outputs_txes_values[i]
    innerdict["changeout_txes_pkey"]  = changeout_txes_pkey[i]
    innerdict["changeout_txes_value"] = changeout_txes_value[i]
    outerdict[hashes[i]] = innerdict 

outerdict

filehandler = open(check_path+"/tx_info_dict.pkl","wb")
pickle.dump(outerdict, filehandler)







