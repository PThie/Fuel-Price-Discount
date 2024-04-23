    #########################################################
    # libraries

from os.path import join
import os
import pandas as pd
import pyarrow.feather as feather
from tqdm import tqdm
import glob

#########################################################
# paths

mainPath = "D:/work/french_gasprices/"
dataPath = os.path.join(mainPath, "data/")

#########################################################
# Combine all
# once for prices and once for disruptions

cases = ["prices", "disruptions"]

for case in tqdm(cases):
    if case == "prices":
        # get file list for prices
        file_list = glob.glob(join(dataPath, "processed/final/station_prices_*"))
    else:
        # get file list for disruptions
        file_list = glob.glob(join(dataPath, "processed/final/station_disruptions_*"))

    # load files and add to list
    files_loaded = []
    for file in file_list:
        data_set = pd.read_feather(file)
        files_loaded.append(data_set)

    # glue together and export
    all_files_df = pd.concat(files_loaded, axis = 0, ignore_index = True)
    if case == "prices":
        all_files_df.to_feather(join(dataPath, "processed/final/station_prices.feather"))
    else:
        all_files_df.to_feather(join(dataPath, "processed/final/station_disruptions.feather"))
