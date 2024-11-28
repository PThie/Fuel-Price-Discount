connecting_temperature_stations <- function(
    german_stations = NA,
    french_stations = NA,
    french_regions = NA,
    temperature_data = NA
) {
    #' @title Connecting temperature data and stations
    #' 
    #' @description This function connects the temperature data with the stations
    #' information.
    #' 
    #' @param german_stations German stations
    #' @param french_stations French stations
    #' @param french_regions French regions
    #' @param temperature_data Temperature data
    #' 
    #' @return Dataframe with connected temperature data and stations
    #' @author Patrick Thiel

    #--------------------------------------------------
    # combine German stations and temperature data

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
    # combine French stations and temperature data

    # add regional information to temperature data
    french_temp <- merge(
            temperature_data[["france"]],
            french_regions |> sf::st_drop_geometry(),
            by = "running_id_region",
            all.x = TRUE
        ) |>
        dplyr::select(-dplyr::contains("region_name"))

    # combine French stations and regions
    french_stations_region <- sf::st_join(
        french_stations,
        french_regions |>
            sf::st_transform(crs = sf::st_crs(french_stations)),
        left = TRUE,
        largest = TRUE
    ) |>
    sf::st_drop_geometry() |>
    dplyr::select(-region_name)

    # combine French stations and temperature data
    french_stations_temp <- merge(
        french_stations_region,
        french_temp,
        by = "running_id_region",
        all.x = TRUE
    ) |>
    dplyr::select(-c("running_id_region", "country"))

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