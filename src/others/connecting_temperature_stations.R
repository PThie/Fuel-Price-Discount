connecting_temperature_stations <- function(
    german_stations = NA,
    temperature_data = NA
) {
    #' @title Connecting temperature data and stations
    #' 
    #' @description This function connects the temperature data with the stations
    #' information.
    #' 
    #' @param german_stations German stations
    #' @param temperature_data Temperature data
    #' 
    #' @return Dataframe with connected temperature data and stations
    #' @author Patrick Thiel

    #--------------------------------------------------
    # combine German stations and temperature data
    # NOTE: temperature data for France is already connected with stations

    german_stations_temp <- merge(
        german_stations |> sf::st_drop_geometry(),
        temperature_data[["germany"]],
        by = "munic",
        all.x = TRUE
    )

    # drop unnecessary columns
    german_stations_temp <- german_stations_temp |>
        dplyr::select(
            -c("AGS", "AGS_district", "munic")
        )

    #--------------------------------------------------
    # clean French temperature station data

    french_stations_temp <- temperature_data[["france"]] |>
        dplyr::select(
            -c("region_name")
        ) |>
        dplyr::rename(
            station_id = running_id_region
        )

    #--------------------------------------------------
    # combine both

    stations_temp <- rbind(
        french_stations_temp,
        german_stations_temp
    )

    #--------------------------------------------------
    # return

    return(stations_temp)
}