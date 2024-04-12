# TODO: once done zip the folder "transferred" in the original routine "gasprices"
# and move to archive in this project (to preserve original coding)

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
        library(tidyr)
        library(future)
        library(future.callr)
        library(MetBrewer)
        library(ggplot2)
        library(openxlsx)
        library(fixest)
        library(stringr)
        library(rlang)
        library(sf)
        library(qs)
        library(haven)
    }
))

#--------------------------------------------------
# Pipeline settings

# target options
tar_option_set(
    resources = tar_resources(
        fst = tar_resources_fst(compress = 50)
    ),
    seed = 1,
    garbage_collection = TRUE,
    storage = "worker",
    retrieval = "worker"
)

# tar_make_future() configuration:
plan(callr)

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
# geo data

targets_geo <- rlang::list2(
    #--------------------------------------------------
    # connections between grids and municipalities
    tar_file(
        grid_municipality_file,
        file.path(
            config_paths()[["gebiete_path"]],
            "Zuordnung",
            "_Gemeinde",
            "2020_Grids_Municipality_Exact_unambiguous.csv"
        )
    ),
    tar_file_read(
        grid_municipalities,
        grid_municipality_file,
        data.table::fread(!!.x)
    )
)

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
        reading_german_fuel_prices(!!.x)
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
        reading_french_fuel_prices(!!.x)
    ),
    #--------------------------------------------------
    # combine German and French fuel price data
    tar_fst(
        fuel_prices,
        joining_germany_france(
            german_fuel_prices = german_fuel_prices,
            french_fuel_prices = french_fuel_prices
        )
    ),
    # Subsetting the fuel price data to the analysis time period
    # April to August 2022
    tar_fst(
        fuel_prices_april_august,
        subsetting_fuel_prices(
            fuel_prices = fuel_prices
        )
    ),
    #--------------------------------------------------
    # extract geo information of French stations
    tar_qs(
        french_stations,
        cleaning_french_stations(
            french_fuel_prices = french_fuel_prices
        )
    ),
    #--------------------------------------------------
    # reading and cleaning German stations
    tar_file(
        german_station_file,
        file.path(
            config_paths()[["data_path"]],
            "german_stations",
            "german_stations.fst"
        )
    ),
    tar_file_read(
        german_stations_raw,
        german_station_file,
        reading_german_stations(!!.x)
    ),
    tar_qs(
        german_stations,
        cleaning_german_stations(
            german_stations_raw = german_stations_raw
        )
    ),
    #--------------------------------------------------
    # read European fuel price data
    tar_file(
        european_fuel_price_file,
        file.path(
            config_paths()[["data_path"]],
            "country_prices",
            "country_prices.dta"
        )
    ),
    tar_file_read(
        european_fuel_prices,
        european_fuel_price_file,
        reading_european_fuel_prices(!!.x)
    ),
    #--------------------------------------------------
    # read RWI-GEO-GRID data
    tar_file(
        microm_data_file,
        file.path(
            config_paths()[["microm_path"]],
            "V12",
            "microm_panel_05-20.dta"
        )
    ),
    tar_file_read(
        microm_data_clean,
        microm_data_file,
        reading_microm_data(!!.x)
    )
)

#--------------------------------------------------
# analysis

targets_analysis <- rlang::list2(
    #--------------------------------------------------
    # Price trends
    tar_target(
        price_trends_plots,
        plotting_price_trends(
            fuel_prices = fuel_prices
        )
    ),
    #--------------------------------------------------
    # Estimating baseline effects
    tar_target(
        baseline_effects,
        estimating_baseline_effects(
            fuel_prices_april_august = fuel_prices_april_august
        )
    ),
    #--------------------------------------------------
    # Event study approach
    tar_target(
        baseline_event_study,
        estimating_baseline_event_study(
            fuel_prices_april_august = fuel_prices_april_august
        )
    ),
    #--------------------------------------------------
    # Testing for spillovers at the French border
    tar_target(
        french_reaction_spillovers,
        testing_french_reaction(
            fuel_prices_april_august = fuel_prices_april_august,
            french_stations = french_stations
        )
    ),
    #--------------------------------------------------
    # Plotting European gas prices
    # Tests whether France is different in their price development compared
    # to other European countries
    tar_target(
        european_prices_plots,
        plotting_european_prices(
            european_fuel_prices = european_fuel_prices
        )
    )
)

# TODO: Separate file loading, i.e. load data once as own target object (applies e.g. to microm data)

#--------------------------------------------------
# combine everything

rlang::list2(
    targets_geo,
    targets_preparation,
    targets_analysis
)