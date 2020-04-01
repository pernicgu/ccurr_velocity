import argparse
import os

# parse command line arguments
def parse_args (
):
    parser = argparse.ArgumentParser(
        description=
        'This script gives back first sanity checks and plots'
    )
    #if nargs=1 is enabled, arguments are passed as list instead of string
    parser.add_argument(
        '-d'
        , '--dir_bc_data_archive'
        , default="../../data/bcdata_raw/archive_csv"
        , help="Directory to archive"
    )
    parser.add_argument(
        '-o'
        , '--output_path'
        , default="../../data/bcdata_raw/velocity_daily.csv"
        , help="Path for saving the newest file."
    )
    args = parser.parse_args()
    return(args)



def get_newest_file(args):

    files = os.listdir(args.dir_bc_data_archive)
    files = [str(args.dir_bc_data_archive)+"/"+i for i in files]
    files = sorted(files, key=os.path.getctime)
    subs  = "daily" 
    res = [i for i in files if subs in i] 
    
    return res[-1]
