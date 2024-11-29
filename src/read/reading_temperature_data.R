reading_temperature_data <- function() {
    #' @title Reading temperature data
    #' 
    #' @description This function reads the temperature data for France and Germany.
    #' 
    #' @return List with temperature data for France and Germany
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read temperature data

    temperature_france <- qs::qread(
        file.path(
            config_paths()[["data_path"]],
            "temperature",
            "temperature_regions_FR.qs"
        )
    )

    temperature_germany <- qs::qread(
        file.path(
            config_paths()[["data_path"]],
            "temperature",
            "temperature_municipalities_DE.qs"
        )
    )

    #--------------------------------------------------
    # clean data

    temperature_france_prep <- temperature_france |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
            date = as.Date(date, "%Y-%m-%d")
        ) |>
        dplyr::rename(running_id_region = running_id)

    temperature_germany_prep <- temperature_germany |>
        sf::st_drop_geometry() |>
        dplyr::filter(!is.na(munic)) |>
        dplyr::mutate(
            date = as.Date(date, "%Y-%m-%d"),
            munic = stringi::stri_trans_general(
                munic,
                "de-ASCII; Latin-ASCII"
            )
        )

    #--------------------------------------------------
    # combine both data in one list

    data_storage <- list(
        "france" = temperature_france_prep,
        "germany" = temperature_germany_prep
    )

    #--------------------------------------------------
    # return

    return(data_storage)
}

