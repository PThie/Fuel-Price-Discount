#########################################################
# libraries
from os.path import isfile, join
from os import listdir
import os
import zipfile

#########################################################
# paths

mainPath = "D:/work/french_gasprices/"
dataPath = os.path.join(mainPath, "data")

# #########################################################
# # all files
onlyfiles = [file for file in listdir(join(dataPath, "raw_data")) if isfile(join(dataPath, "raw_data", file))]

# # select only zip folders
onlyfiles_zip = [file for file in onlyfiles if "zip" in file]

#########################################################
# unzip
for files in onlyfiles_zip:
    filename = join(dataPath, "raw_data",files)
    with zipfile.ZipFile(filename, "r") as zip_ref:
        zip_ref.extractall(join(dataPath, "raw_data"))
        print(f"-----------{filename} completed----------")
