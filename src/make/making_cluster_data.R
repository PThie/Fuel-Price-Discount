making_cluster_data <- function(
    price_data = NA,
    german_stations = NA,
    french_stations_regional = NA
) {
    #' @title Combining regional data for clustering
    #' 
    #' @description This function combines regional data for Germany and France
    #' for clustering purposes.
    #' 
    #' @param price_data Data frame containing price data with station IDs.
    #' @param german_stations Spatial data frame containing German station information.
    #' @param french_stations_regional Data frame containing French station regional information.
    #' 
    #' @return Data frame with combined regional data for clustering.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # combine all data

    price_data_regional_germany <- price_data |>
        dplyr::filter(country == "DE") |>
        merge(
            german_stations |>
                sf::st_drop_geometry(),
            by = "station_id",
            all.x = TRUE
        ) |>
        dplyr::mutate(
            state_id = substring(AGS_district, 1, 2)
        ) |>
        dplyr::select(-munic) |>
        dplyr::rename(
            munic_id = AGS,
            district_id = AGS_district
        )
    
    price_data_regional_france <- price_data |>
        dplyr::filter(country == "FR") |>
        merge(
            french_stations_regional |>
                dplyr::select(-country),
            by = "station_id",
            all.x = TRUE
        ) |>
        dplyr::select(-c("region_name", "department_name", "commune_name")) |>
        dplyr::rename(
            munic_id = commune_id,
            district_id = department_id,
            state_id = region_id
        )

    price_data_regional <- rbind(
        price_data_regional_germany,
        price_data_regional_france
    )

    #--------------------------------------------------
    # return

    return(price_data_regional)
}