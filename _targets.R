#----------------------------------------------
# load libraries

suppressWarnings(suppressPackageStartupMessages(
	{
        library(targets)
        library(tarchetypes)
        library(renv)
        library(fst)
        library(arrow)
        library(dplyr)
    }
))

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

sub_directories <- list.dirs(
    config_paths()[["src_path"]],
    full.names = FALSE,
    recursive = FALSE
)

for (sub_directory in sub_directories) {
    if (sub_directory != "helpers") { 
        lapply(
            list.files(
                file.path(
                    config_paths()[["src_path"]],
                    sub_directory
                ),
                pattern = "\\.R$",
                full.names = TRUE,
                ignore.case = TRUE
            ),
            source
        )
    }
}

#--------------------------------------------------
# data preparation

targets_preparation <- rlang::list2(
    #--------------------------------------------------
    # read orginal fuel price data for Germany
    tar_file(
        german_fuel_price_file,
        file.path(
            config_paths()[["data_path"]],
            "german_fuel_data",
            "fuel_prices_germany.fst"
        )
    ),
    tar_file_read(
        german_fuel_prices,
        german_fuel_price_file,
        read_german_fuel_prices(!!.x)
    ),
    #--------------------------------------------------
    # read original fuel price data for France
    tar_file(
        french_fuel_price_file,
        file.path(
            config_paths()[["data_path"]],
            "french_fuel_data",
            "station_prices.feather"
        )
    ),
    tar_file_read(
        french_fuel_prices,
        french_fuel_price_file,
        read_french_fuel_prices(!!.x)
    )
)

# TODO: Separate file loading, i.e. load data once as own target object (applies e.g. to microm data)

#--------------------------------------------------
# combine everything

rlang::list2(
    targets_preparation
)