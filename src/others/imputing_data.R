imputing_data <- function(
    price_data = NA
) {
    #' @title Imputing missing data for stations in France
    #' 
    #' @description This function fills in missing days for stations in France
    #' by carrying forward the last observation.
    #' 
    #' @param price_data Data frame with daily price data
    #' 
    #' @return Returns data frame with imputed data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # fill in missing days for stations in France

    filled_france <- price_data |>
        dplyr::filter(country == "FR") |>
        tidyr::complete(
            station_id,
            date = seq(
                min(date),
                max(date),
                by = "day"
            )
        ) |>
        dplyr::arrange(station_id, date) |>
        tidyr::fill(
            e5, e10, diesel,
            country, treat_tankrabatt_de, treat_region_de,
            months,
            .direction = "down"
        ) |>
        dplyr::ungroup() |>
        as.data.frame()

    #--------------------------------------------------
    # combine data again

    data_germany <- price_data |>
        dplyr::filter(country == "DE") |>
        as.data.frame()

    filled_data <- rbind(
        data_germany,
        filled_france
    )

    #--------------------------------------------------
    # return

    return(filled_data)
}