cleaning_french_stations <- function(french_fuel_prices = NA) {
    #' @title Cleaning French stations
    #' 
    #' @description This function extracts and cleans the geographical information
    #' of French fuel stations.
    #' 
    #' @param french_fuel_prices A data frame containing the French fuel prices
    #' and station information.
    #' 
    #' @return Dataframe with cleaned French stations.
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # prepare French stations
    # extract them from the price data

    french_stations <- french_fuel_prices |>
        dplyr::select(
            station_id = stationID,
            lat,
            lon
        ) |>
        dplyr::distinct(
            station_id,
            .keep_all = TRUE
        )

    # prepare coordinates
    # according to data provider, the CRS is WSG84 but you have to divide the
    # coordinates by 100,000 first
    french_stations <- french_stations |>
        dplyr::mutate(
            station_id = as.character(station_id),
            lat = as.numeric(lat) / 100000,
            lon = as.numeric(lon) / 100000,
            country = "France"
        ) |>
        dplyr::filter(!is.na(lat)) |>
        as.data.frame() |>
        sf::st_as_sf(
            coords = c("lon", "lat"),
            crs = config_globals()[["gpscrs"]]
        ) |>
        sf::st_transform(crs = config_globals()[["utmcrs"]])

    #--------------------------------------------------
    # return

    return(french_stations)
}