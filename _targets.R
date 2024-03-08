#----------------------------------------------
# load libraries

suppressPackageStartupMessages(
	{
        library(targets)
        library(tarchetypes)
        library(renv)
        library(fst)
        library(arrow)
        library(dplyr)
    }
)

#----------------------------------------------
# load configurations

source(
    file.path(
        here::here(),
        "src",
        "helpers",
        "config.R"
    )
)

#----------------------------------------------
# load R scripts

lapply(
    list.files(
        config_paths()[["code_path"]],
        pattern = "\\.R$",
        full.names = TRUE,
        ignore.case = TRUE
    ),
    source
)

#--------------------------------------------------
# data preparation

targets_preparation <- rlang::list2(
    #--------------------------------------------------
    # read orginal fuel price data for Germany
    tar_file(
        german_fuel_price_data,
        file.path(
            config_paths()[["data_path"]],
            "german_fuel_data",
            "fuel_prices_germany.fst"
        )
    ),
    tar_file_read(
        german_fuel_prices,
        german_fuel_price_data,
        read_german_fuel_prices(!!.x)
    ),
    #--------------------------------------------------
    # read original fuel price data for France
    tar_file(
        french_fuel_price_data,
        file.path(
            config_paths()[["data_path"]],
            "french_fuel_data",
            "station_prices.feather"
        )
    ),
    tar_file_read(
        french_fuel_prices,
        french_fuel_price_data,
        read_french_fuel_prices(!!.x)
    )
)

#--------------------------------------------------
# combine everything

rlang::list2(
    targets_preparation
)