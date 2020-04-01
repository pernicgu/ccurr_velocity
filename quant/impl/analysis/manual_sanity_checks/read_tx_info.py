import argparse

# parse command line arguments
def parse_args (
):
    parser = argparse.ArgumentParser(
        description=
        'This script gives infos on a certain tx with hash'
    )
    #if nargs=1 is enabled, arguments are passed as list instead of string
    parser.add_argument(
        '-i'
        , '--hash_of_tx'
        , default="CHANGEME"
        , help="Hash of tx to search for."
    )
    parser.add_argument(
        '-o'
        , '--output_type'
        , default="CHANGEME"
        , help="inputs_txes_hashes, inputs_txes_clusters,"+
        "outputs_txes_hashes, outputs_txes_clusters "
    )
    args = parser.parse_args()
    return(args)

args = parse_args()

import pickle
import os

check_path   = os.getcwd()#+"/quant/impl/analysis/manual_sanity_checks"
filehandler = open(check_path+"/tx_info_dict.pkl","rb")
txdict = pickle.load(filehandler)

print(txdict[args.hash_of_tx][args.output_type])
