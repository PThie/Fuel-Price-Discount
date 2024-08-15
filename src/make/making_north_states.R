making_north_states <- function(
    fuel_prices_april_august = NA,
    german_stations = NA
) {
    #' @title Subsetting states
    #' 
    #' @description This function subsets the northern states.
    #' 
    #' @param fuel_prices_april_august Fuel price data for April to August 2022
    #' @param german_stations Station data for Germany
    #' 
    #' @return Price data for northern states
    #' @author Patrick Thiel

    #--------------------------------------------------
    # prepare station data

    german_stations <- german_stations |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
            state_id = substring(AGS_district, 1, 2)
        )

    #--------------------------------------------------
    # combine with price data

    prices_prep <- fuel_prices_april_august |>
        merge(
            german_stations,
            by = "station_id",
            all.x = TRUE
        )

    #--------------------------------------------------
    # remove Bayern (09), Baden-Württemberg (08),
    # Saarland (10), and Rheinland-Pfalz (07)

    prices_states_north <- prices_prep |>
        dplyr::filter(
            !state_id %in% c("09", "08", "10", "07")
        )

    #--------------------------------------------------
    # return

    return(prices_states_north)
}