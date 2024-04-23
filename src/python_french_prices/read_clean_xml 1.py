'''
TO DO
- adjust lon and lat (divide by 100000 to get WGS84)
'''


#########################################################
# libraries

import xml.etree.ElementTree as ET
from os.path import join
import os
import pandas as pd
import numpy as np
import pyarrow.feather as feather
from tqdm import tqdm
import glob

#########################################################
# paths

mainPath = "D:/work/french_gasprices/"
dataPath = os.path.join(mainPath, "data/")

#########################################################
# Cleaning data
start_year = 2007
end_year = 2022

#years = range(start_year, end_year+1)
years = range(2007, end_year+1)

for year in tqdm(years):
    #########################################################
    # set up

    # parse
    filename = "raw_data/PrixCarburants_annuel_" + str(year) + ".xml"
    tree = ET.parse(join(dataPath, filename))

    # get root
    root = tree.getroot()

    # initialise rows
    general_info = []
    price_info = []
    disruption_info = []

    #########################################################
    # loop over all nodes in root (general info)
    for node in root:
        uni_id = node.attrib.get("id")
        type_station = node.attrib.get("pop")
        lat = node.attrib.get("latitude")
        lon = node.attrib.get("longitude")
        zip = node.attrib.get("cp")
        address = node.find("adresse").text
        city = node.find("ville").text
        general_info.append({"uni_id" : uni_id, "type_station" : type_station, "lat" : lat, "lon" : lon, "zipcode" : zip, "address" : address, "city" : city})

    # transform into data frame and export
    filename = "processed/general_info_" + str(year) + ".csv"
    general_info_df = pd.DataFrame(general_info, columns = ["uni_id", "type_station", "lat", "lon", "zipcode", "address", "city"])
    general_info_df.to_csv(os.path.join(dataPath, filename), index = False, na_rep = "NA")

    # add continous ID for merge
    general_info_df["stationID"] = list(range(0, len(general_info_df)))

    #########################################################
    # price info for each station

    # number of stations
    num_stations = len(general_info_df)

    # loop
    for id in range(0, num_stations):
        for node in root[id].iter("prix"):
            if "nom" not in node.attrib:
                station_id = id
                type = np.NaN
                type_id = np.NaN
                time = np.NaN
                price = np.NaN
            else:
                station_id = id
                type = node.attrib["nom"]
                type_id = node.attrib["id"]
                time = node.attrib["maj"]
                if year < 2022: # following 2022: the price is already in Euro (before in Cents)
                    price = int(node.attrib["valeur"]) / 1000
                else:
                    price = float(node.attrib["valeur"])
            price_info.append({"stationID" : station_id, "type" : type, "type_id" : type_id, "time" : time, "price" : price})

    # transform into data frame
    prices = pd.DataFrame(price_info, columns = ["stationID", "type", "type_id", "time", "price"])

    # split time into date and time
    if year < 2015:
        prices["date"] = prices["time"].str.split(" ", n = 1, expand = True)[0]
        prices["time"] = prices["time"].str.split(" ", n = 1, expand = True)[1]
        prices["time"] = prices["time"].str.split(".", n = 1, expand = True)[0]
    else:
        prices["date"] = prices["time"].str.split("T", n = 1, expand = True)[0]
        prices["time"] = prices["time"].str.split("T", n = 1, expand = True)[1]

    # reorder
    prices = prices.reindex(columns = ["stationID", "type", "type_id", "date", "time", "price"])

    # export
    filename = "processed/prices_" + str(year) + ".csv"
    prices.to_csv(os.path.join(dataPath, filename), index = False, na_rep = "NA")

    #########################################################
    # additional info on type of gasoline
    # sometimes stations stop supplying certain types of gaseline

    # loop
    for id in range(0, num_stations):
        for node in root[id].iter("rupture"):
            if "nom" not in node.attrib:
                station_id = id
                type = np.NaN
                type_id = np.NaN
                start_change = np.NaN
                end_change = np.NaN
            else:
                station_id = id
                type = node.attrib["nom"]
                type_id = node.attrib["id"]
                start_change = node.attrib["debut"]
                end_change = node.attrib["fin"]
            disruption_info.append({"stationID" : station_id, "type" : type, "type_id" : type_id, "start_change" : start_change, "end_change" : end_change})

    # transform into data frame and export
    filename = "processed/disruptions_" + str(year) + ".csv"
    disruptions = pd.DataFrame(disruption_info, columns = ["stationID", "type", "type_id", "start_change", "end_change"])
    disruptions.to_csv(os.path.join(dataPath, filename), index = False, na_rep = "NA")

    #########################################################
    # merge

    # merge
    station_disruptions = disruptions.merge(general_info_df, on = "stationID", how = "outer")
    station_prices = prices.merge(general_info_df, on = "stationID", how = "outer")

    # export as feather file
    filename = "processed/final/station_disruptions_" + str(year) + ".feather"
    station_disruptions.to_feather(join(dataPath, filename))
    filename = "processed/final/station_prices_" + str(year) + ".feather"
    station_prices.to_feather(join(dataPath, filename))

    #########################################################
    # code stops when last available year is reached
    if year > years[len(years) - 1]:
        break

    # print completion statement (coloring based on ASCII)
    print(f"\033[92m'" + "        Transformation completed for year " + str(year) + '\x1b[0m')
