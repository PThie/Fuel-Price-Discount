connecting_french_stations_regions <- function(
    french_stations = NA,
    french_regions = NA
) {
    #' @title Connecting French stations with regions
    #' 
    #' @description This function connects the French stations with the regions.
    #' 
    #' @param french_stations Spatial Dataframe with French stations
    #' @param french_regions Spatial Dataframe with French regions
    #' 
    #' @return Spatial Dataframe with French stations with regions
    #' @author Patrick Thiel

    #--------------------------------------------------
    # set geometry

    french_stations <- sf::st_set_geometry(
        french_stations,
        french_stations$geometry
    )

    french_regions <- sf::st_set_geometry(
        french_regions,
        french_regions$geometry
    )

    #--------------------------------------------------
    # transform crs

    french_regions <- sf::st_transform(
        french_regions,
        crs = sf::st_crs(french_stations)
    )
    
    #--------------------------------------------------
    # connect both dataframes

    france_stations_regional <- sf::st_join(
        french_stations,
        french_regions,
        left = TRUE,
        largest = TRUE
    )

    #--------------------------------------------------
    # return

    return(france_stations_regional)
}