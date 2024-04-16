cleaning_german_stations <- function(german_stations_raw = NA) {
    #' @title Cleaning German stations
    #' 
    #' @description This function cleans the German station data.
    #' 
    #' @param german_stations_raw Dataframe containing the raw German station data.
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
    
    #--------------------------------------------------
    # return
    
    return(german_stations)
}