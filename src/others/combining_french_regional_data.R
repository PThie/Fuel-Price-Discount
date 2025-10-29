combining_french_regional_data <- function(
    french_stations = NA,
    french_regions = NA,
    french_departments = NA,
    french_communes = NA
) {
    #' @title Combining French Regional Data
    #' 
    #' @description This function enriches French station data with regional,
    #' departmental, and communal information by finding the nearest
    #' geographical features.
    #' 
    #' @param french_stations Spatial data frame of French stations.
    #' @param french_regions Spatial data frame of French regions.
    #' @param french_departments Spatial data frame of French departments.
    #' @param french_communes Spatial data frame of French communes.
    #' 
    #' @return Data frame of French stations with added regional, departmental,
    #' and communal information.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # add regional information to french stations

    nearest_region <- sf::st_nearest_feature(
        french_stations,
        french_regions
    )

    nearest_department <- sf::st_nearest_feature(
        french_stations,
        french_departments
    )

    nearest_commune <- sf::st_nearest_feature(
        french_stations,
        french_communes
    )
    

    french_stations_regional <- french_stations |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
            region_id = french_regions$running_id_region[nearest_region],
            region_name = french_regions$region_name[nearest_region],
            department_id = french_departments$running_id_region[nearest_department],
            department_name = french_departments$region_name[nearest_department],
            commune_id = french_communes$running_id_region[nearest_commune],
            commune_name = french_communes$region_name[nearest_commune]
        )

    #--------------------------------------------------
    # return

    return(french_stations_regional)
}