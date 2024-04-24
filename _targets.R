# TODO: once done zip the folder "transferred" in the original routine "gasprices"
# and move to archive in this project (to preserve original coding)

# NOTE: In case the error shows up:
# Error: OGRCreateCoordinateTransformation(): transformation not available
# Run before loading the packages
    # Sys.setenv("PROJ_NETWORK"="ON")

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
        library(data.table)
        library(stats)
        library(lubridate)
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
        # NOTE: Read data directly since it does not requiere a function
        grid_municipalities,
        grid_municipality_file,
        data.table::fread(!!.x) |>
            dplyr::select(-share) |>
            dplyr::mutate(
                # add leading zeros to AGS
                AGS = stringr::str_pad(AGS, 8, pad = "0")
            )
    ),
    #--------------------------------------------------
    # geographical information for German municipalities
    tar_file(
        german_municipality_file,
        file.path(
            config_paths()[["gebiete_path"]],
            "Gemeinde",
            "2020",
            "VG250_GEM.shp"
        )
    ),
    tar_file_read(
        german_municipalities,
        german_municipality_file,
        sf::st_read(!!.x, quiet = TRUE) |>
            dplyr::select(AGS = AGS_0, geometry) |>
            sf::st_transform(config_globals()[["utmcrs"]])
    ),
    #--------------------------------------------------
    # geographical information for German districts
    tar_file(
        german_district_file,
        file.path(
            config_paths()[["gebiete_path"]],
            "Kreis",
            "2020",
            "VG250_KRS.shp"
        )
    ),
    tar_file_read(
        german_districts,
        german_district_file,
        sf::st_read(!!.x, quiet = TRUE) |>
            dplyr::select(AGS, geometry) |>
            sf::st_transform(config_globals()[["utmcrs"]])
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
            german_stations_raw = german_stations_raw,
            german_municipalities = german_municipalities
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
        microm_data_raw,
        microm_data_file,
        reading_microm_data(!!.x)
    ),
    tar_fst(
        microm_data_cleaned,
        cleaning_microm_data(
            microm_data_raw = microm_data_raw,
            grid_municipalities = grid_municipalities
        )
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
    ),
    #--------------------------------------------------
    # Testing the role of purchasing power
    # As proxy for demand elasticity
    tar_target(
        purchasing_power_effects,
        making_purch_power(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities
        )
    ),
    #--------------------------------------------------
    # Testing the role of competition
    # Proxied through the station density per car
    tar_target(
        station_density_effects,
        making_station_density(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities,
            german_districts = german_districts
        )
    ),
    #--------------------------------------------------
    # Estimating the regional effect at district level
    tar_target(
        regional_effect_district,
        estimating_regional_effect(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            german_districts = german_districts
        )
    ),
    #--------------------------------------------------
    # Affected population, i.e. population shares receiving different
    # pass-through rates
    tar_target(
        affected_population,
        calculating_affected_population(
            regional_effect_district = regional_effect_district,
            microm_data_cleaned = microm_data_cleaned
        )
    ),
    #--------------------------------------------------
    # Fuel consumption by income groups
    tar_fst(
        fuel_consumption_income_groups,
        calculating_fuel_consumption_purch_power(
            microm_data_cleaned = microm_data_cleaned
        )
    ),
    #--------------------------------------------------
    # Plotting overall fuel consumption
    tar_target(
        fuel_consumption_plots,
        plotting_fuel_consumption()
    ),
    #--------------------------------------------------
    # Determining station density in relation to the national level
    # Relate it also to the pass-through rates
    tar_target(
        station_density_inequality,
        calculating_inequality_station_density(
            regional_effect_district = regional_effect_district,
            microm_data_cleaned = microm_data_cleaned,
            german_stations = german_stations
        )
    ),
    #--------------------------------------------------
    # Determining purchasing power in relation to the national level
    # Relate it also to the pass-through rates
    tar_target(
        purchasing_power_inequality,
        calculating_inequality_purch_power(
            regional_effect_district = regional_effect_district,
            microm_data_cleaned = microm_data_cleaned,
            german_stations = german_stations
        )
    ),
    #--------------------------------------------------
    # Purchasing power estimations based on median classification
    tar_target(
        purchasing_power_median,
        making_purch_power_median(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned
        )
    ),
    #--------------------------------------------------
    # Google trends
    tar_file_read(
        google_trends_data,
        file.path(
            config_paths()[["data_path"]],
            "google_trends"
        ),
        reading_google_trends(!!.x)
    ),
    tar_target(
        google_trends_plots,
        plotting_google_trends(
            google_trends_data = google_trends_data
        )
    ),
    #--------------------------------------------------
    # Relating google scores and pass-through rates
    tar_file_read(
        time_effects_diesel,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_Germany_diesel.xlsx"
        ),
        reading_time_effects(!!.x, fuel_type = "diesel")
    ),
    tar_file_read(
        time_effects_petrol,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_Germany_e10.xlsx"
        ),
        reading_time_effects(!!.x, fuel_type = "e10")
    ),
    tar_target(
        google_scores_passthrough,
        estimating_search_behavior_passthrough(
            time_effects_diesel = time_effects_diesel,
            time_effects_petrol = time_effects_petrol,
            google_trends_data = google_trends_data
        )
    ),
    #--------------------------------------------------
    # Testing the parallel trend assumption using the HonestDiD approach
    tar_target(
        honest_did,
        testing_robust_trends(
            fuel_prices_april_august = fuel_prices_april_august
        )
    )
)

#--------------------------------------------------
# combine everything

rlang::list2(
    targets_geo,
    targets_preparation,
    targets_analysis
)