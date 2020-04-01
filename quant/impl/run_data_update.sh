#!/bin/bash

# PARSE ARGUMENT
for i in "$@"
do
case $i in
    -mkt=*|--upd_mkt_data=*)
    UPD_MKT_DATA="${i#*=}"
    shift # past argument=value
    ;;
    -bc=*|--upd_bc_data=*)
    UPD_BC_DATA="${i#*=}"
    shift # past argument=value
    ;;
    *)
          # unknown option
    ;;
esac
done


# SET DEFAULT
if [ ! -z $UPD_MKT_DATA ]; then
    UPD_MKT_DATA = 0
fi

if [ ! -z $UPD_BC_DATA ]; then
    UPD_BC_DATA = 0
fi

# CONDITIONALLY DOWNLOAD / EXTRACT NEW DATA
if [ "$UPD_MKT_DATA" ]
then
    echo "Not implemnted: Would updated market data"
    # COMMENT: Note, that in marketdata etc. all directories broke because of moving files.
    # Rscript ../marketdata_generation/data_col.R 
    # Rscript ../marketdata_generation/data_proc.R
fi

if [ "$UPD_BC_DATA" ]
then
    echo "Not implemnted: Would updated BC data"
    #python3 ../bc_data_generation/script.py -e 01/01/2010 
fi

# PREPARE DATA
python3 ./data_preprocessing/get_data_from_archive.py
python3 ./data_preprocessing/prepare_data.py -m "diff_2diff"
