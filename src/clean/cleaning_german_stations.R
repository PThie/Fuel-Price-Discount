cleaning_german_stations <- function(
    german_stations_raw = NA,
    german_municipalities = NA
) {
    #' @title Cleaning German stations
    #' 
    #' @description This function cleans the German station data.
    #' 
    #' @param german_stations_raw Dataframe containing the raw German station data.
    #' @param german_municipalities Geographical information of German
    #' municipalities.
    #' 
    #' @return Cleaned German station data.
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # clean station data

    # select geographical information
    german_stations <- german_stations_raw |>
        dplyr::select(id, lon, lat) |>
        dplyr::distinct(id, .keep_all = TRUE) |>
        dplyr::rename(station_id = id) |>
        sf::st_as_sf(
            coords = c("lon", "lat"),
            crs = config_globals()[["gpscrs"]]
        ) |>
        sf::st_transform(config_globals()[["utmcrs"]])
    
    # add municipality information
    german_stations_geo <- sf::st_join(
        german_stations,
        german_municipalities,
        left = TRUE,
        largest = TRUE
    )
    
    #--------------------------------------------------
    # return
    
    return(german_stations)
}