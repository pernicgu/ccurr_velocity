#!/bin/bash

bash run_data_update.sh
python3 ./analysis/sanity_checks.py -i "../../data/bcmeasures_raw/bc_data.pkl" -o "../../output_storage/descriptives_blockchain_data/" -f "DIFF|DIFFLOG"
python3 ./analysis/sanity_checks.py -i "../../data/marketmeasures_raw/mkt_data.pkl" -o "../../output_storage/descriptives_market_data/" -f "DIFF|DIFFLOG"
python3 ./analysis/descriptives_joined_data.py
