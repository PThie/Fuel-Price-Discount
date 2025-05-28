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
            "Raster_Gemeinde",
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
            dplyr::select(
                AGS = AGS_0,
                munic = GEN,
                geometry
            ) |>
            dplyr::mutate(
                munic = stringi::stri_trans_general(
                    munic,
                    "de-ASCII; Latin-ASCII"
                )   
            ) |>
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
    ),
    #--------------------------------------------------
    # geographical information for French regions
    tar_file_read(
        french_regions,
        file.path(
            config_paths()[["data_path"]],
            "french_borders",
            "regions",
            "regions.shp"
        ),
        reading_french_regions(!!.x)
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
    # Preparing data for the analysis of the French FTD
    tar_fst(
        french_fuel_prices_prep,
        subsetting_french_fuel_prices(
            price_data = fuel_prices
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
    # connecting French stations and regions
    tar_qs(
        french_stations_regions,
        connecting_french_stations_regions(
            french_stations = french_stations,
            french_regions = french_regions
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
            price_data = fuel_prices_april_august,
            suffix_export = "complete",
            twoway_clustering = FALSE
        )
    ),
    #--------------------------------------------------
    # Event study approach
    tar_target(
        baseline_event_study,
        estimating_baseline_event_study(
            price_data = fuel_prices_april_august,
            suffix_export = "complete"
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
            price_data = fuel_prices_april_august,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities,
            suffix_export = "complete"
        )
    ),
    tar_target(
        purchasing_power_effects_plots,
        plotting_purch_power_effects(
            effects = purchasing_power_effects,
            suffix_export = "complete"
        )
    ),
    tar_target(
        purchasing_power_effects_event_study,
        estimating_purch_power_event_study(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            suffix_export = "complete"
        )
    ),
    tar_target(
        purchasing_power_effects_twoweeks,
        making_purch_power(
            price_data = fuel_prices_april_august |>
                dplyr::filter(date <= "2022-06-14"),
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities,
            suffix_export = "twoweeks"
        )
    ),
    tar_target(
        purchasing_power_effects_plots_twoweeks,
        plotting_purch_power_effects(
            effects = purchasing_power_effects_twoweeks,
            suffix_export = "twoweeks"
        )
    ),
    tar_target(
        purchasing_power_effects_event_study_twoweeks,
        estimating_purch_power_event_study(
            fuel_prices_april_august = fuel_prices_april_august |>
                dplyr::filter(date <= "2022-06-14"),
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            suffix_export = "twoweeks"
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
            german_districts = german_districts,
            suffix_export = "complete"
        )
    ),
    tar_target(
        station_density_effects_plots,
        plotting_station_density_effects(
            effects = station_density_effects,
            suffix_export = "complete"
        )
    ),
    tar_target(
        station_density_effects_event_study,
        estimating_station_density_event_study(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            suffix_export = "complete"
        )
    ),
    tar_target(
        station_density_effects_event_study_plots,
        plotting_station_density_effects_event_study(
            effects = station_density_effects_event_study,
            suffix_export = "complete"
        )
    ),
    tar_target(
        station_density_effects_twoweeks,
        making_station_density(
            fuel_prices_april_august = fuel_prices_april_august |>
                dplyr::filter(date <= "2022-06-14"),
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities,
            german_districts = german_districts,
            suffix_export = "twoweeks"
        )
    ),
    tar_target(
        station_density_effects_plots_twoweeks,
        plotting_station_density_effects(
            effects = station_density_effects_twoweeks,
            suffix_export = "twoweeks"
        )
    ),
    tar_target(
        station_density_effects_event_study_twoweeks,
        estimating_station_density_event_study(
            fuel_prices_april_august = fuel_prices_april_august |>
                dplyr::filter(date <= "2022-06-14"),
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            suffix_export = "twoweeks"
        )
    ),
    tar_target(
        station_density_effects_event_study_plots_twoweeks,
        plotting_station_density_effects_event_study(
            effects = station_density_effects_event_study_twoweeks,
            suffix_export = "twoweeks"
        )
    ),
    #--------------------------------------------------
    # Estimating the regional effect at district level
    tar_target(
        regional_effect_district,
        estimating_regional_effect(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations,
            german_districts = german_districts,
            suffix_export = "complete"
        )
    ),
    tar_target(
        regional_effect_district_twoweeks,
        estimating_regional_effect(
            fuel_prices_april_august = fuel_prices_april_august |>
                dplyr::filter(date <= "2022-06-14"),
            german_stations = german_stations,
            german_districts = german_districts,
            suffix_export = "twoweeks"
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
            "did_est_Germany_diesel_complete.xlsx"
        ),
        reading_time_effects(!!.x, fuel_type = "diesel")
    ),
    tar_file_read(
        time_effects_petrol,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_Germany_e10_complete.xlsx"
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
    tar_fst(
        weekly_prices,
        making_weekly_prices(
            price_data = fuel_prices_april_august
        )
    ),
    tar_target(
        honest_did,
        testing_robust_trends(
            fuel_prices_april_august = fuel_prices_april_august
        )
    ),
    tar_target(
        honest_did_days,
        testing_robust_trends_dayspecific(
            fuel_prices_april_august = fuel_prices_april_august
        )
    ),
    tar_target(
        honest_did_plots,
        plotting_robust_trends(honest_did = honest_did)
    ),
    tar_target(
        honest_did_days_plots,
        plotting_robust_trends_dayspecific(
            honest_did_days = honest_did_days
        )
    ),
    #--------------------------------------------------
    # Testing the effect in subset of states
    # This tests the effect in the "northern" states which
    #' are all states that do not share a border with France plus Bayern. This
    #' test intends to show robustness against violations of the SUTVA assumption
    #' by excluding states at the border and Bayern. It also tests for impacts
    #' of the drought during that period.
    tar_fst(
        north_states_prices,
        making_north_states(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations
        )
    ),
    tar_target(
        testing_north_states_baseline,
        estimating_baseline_effects(
            price_data = north_states_prices,
            suffix_export = "north",
            twoway_clustering = FALSE
        )
    ),
    tar_target(
        testing_north_states_event_study,
        estimating_baseline_event_study(
            price_data = north_states_prices,
            suffix_export = "north"
        )
    ),
    #--------------------------------------------------
    # Testing the effect for each state separately
    tar_fst(
        state_prices,
        making_state_prices(
            fuel_prices_april_august = fuel_prices_april_august,
            german_stations = german_stations
        )
    ),
    tar_target(
        testing_states_event_study,
        estimating_states_event_study(
            price_data = state_prices
        )
    ),
    # testing robust trends
    tar_target(
        testing_robust_trends_dayspecific_state,
        testing_robust_trends_dayspecific_states(
            price_data = state_prices
        )
    ),
    tar_target(
        honest_did_days_plots_states,
        plotting_robust_trends_dayspecific_states(
            honest_did_days = testing_robust_trends_dayspecific_state
        )
    ),
    tar_target(
        testing_states_station_density,
        estimating_states_station_density(
            price_data = state_prices,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities,
            german_districts = german_districts
        )
    ),
    tar_target(
        testing_states_station_density_event_study,
        estimating_states_station_density_event_study(
            price_data = state_prices,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned
        )
    ),
    #--------------------------------------------------
    # Placebo analysis
    tar_target(
        placebo_prices,
        subsetting_placebo_prices(
            price_data = fuel_prices
        )
    ),
    tar_target(
        baseline_effects_placebo,
        estimating_baseline_effects_placebo(
            price_data = placebo_prices
        )
    ),
    tar_target(
        baseline_event_study_placebo,
        estimating_baseline_event_study_placebo(
            price_data = placebo_prices
        )
    ),
    tar_target(
        event_study_placebo_plots,
        plotting_event_study_placebo(
            result_list = baseline_event_study_placebo,
            suffix_export = "placebo"
        )
    ),
    tar_target(
        event_study_placebo_combined_plots,
        plotting_event_study_combined(
            result_list = baseline_event_study,
            result_list_placebo = baseline_event_study_placebo
        )
    ),
    #--------------------------------------------------
    # Baseline estimation with twoway clustering
    tar_target(
        testing_baseline_twoway_clustering,
        estimating_baseline_effects(
            price_data = fuel_prices_april_august,
            suffix_export = "twoway",
            twoway_clustering = TRUE
        )
    ),
    #--------------------------------------------------
    # Testing morning rush hour effects
    # Reading raw German data (each individual change, not aggregated information)
    tar_file(
        german_fuel_price_file_raw,
        file.path(
            config_paths()[["data_path"]],
            "german_fuel_data",
            "fuel_prices_germany_raw.fst"
        )
    ),
    tar_file_read(
        german_fuel_prices_raw,
        german_fuel_price_file_raw,
        reading_german_fuel_prices(!!.x)
    ),
    tar_fst(
        german_fuel_prices_april_august_raw,
        subsetting_fuel_prices(
            fuel_prices = german_fuel_prices_raw
        )
    ),
    tar_fst(
        german_fuel_prices_morning,
        subsetting_morning_rush(
            fuel_prices = german_fuel_prices_april_august_raw
        )
    ),
    tar_fst(
        fuel_prices_morning,
        joining_germany_france(
            german_fuel_prices = german_fuel_prices_morning,
            french_fuel_prices = french_fuel_prices
        )
    ),
    tar_target(
        testing_morning_rush_event_study,
        estimating_baseline_event_study(
            price_data = fuel_prices_morning,
            suffix_export = "morning_rush"
        )
    ),
    #--------------------------------------------------
    # Testing temperature impact
    tar_target(
        temperature_data,
        reading_temperature_data()
    ),
    tar_fst(
        station_temperature_data,
        connecting_temperature_stations(
            german_stations = german_stations,
            french_stations = french_stations,
            french_regions = french_regions,
            temperature_data = temperature_data
        )
    ),
    tar_target(
        testing_temperature_impact,
        estimating_temperature_impact(
            fuel_prices = fuel_prices_april_august,
            station_temperature_data = station_temperature_data
        )
    ),
    #--------------------------------------------------
    # Testing the French FTD
    tar_target(
        french_ftd_trends_plots,
        plotting_french_ftd(
            fuel_prices = french_fuel_prices_prep
        )
    ),
    tar_target(
        french_ftd_baseline,
        estimating_french_ftd(
            price_data = french_fuel_prices_prep
        )
    ),
    tar_target(
        french_ftd_event_study,
        estimating_french_ftd_event_study(
            price_data = french_fuel_prices_prep
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