making_weekly_prices <- function(
    price_data = NA
) {
    #' @title Making Weekly Prices
    #' 
    #' @description This function aggregates the daily fuel prices to weekly.
    #' 
    #' @param price_data Fuel price data
    #' 
    #' @return Dataframe with weekly fuel prices
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # aggregate to weekly prices by country

    # add week number
    price_data <- price_data |>
        dplyr::mutate(
            weeks = as.numeric(format(as.Date(date, "%Y-%m-%d"), "%W")),
        )

    weekly_prices <- price_data |>
        dplyr::group_by(weeks, station_id) |>
        dplyr::summarise(
            e10 = mean(e10, na.rm = TRUE),
            diesel = mean(diesel, na.rm = TRUE),
            treat_region_de = dplyr::first(treat_region_de)
        )

    # define treatment week, i.e. the week in which the treatment starts
    start_treatment <- unique(price_data[price_data$date == "2022-06-01", c("weeks")])

    # define treatment time
    weekly_prices <- weekly_prices |>
        dplyr::mutate(
            treat_tankrabatt_de = dplyr::case_when(
                weeks < start_treatment ~ "control",
                weeks >= start_treatment ~ "treated"
            )
        )

    # add time to treatment in weeks
    weekly_prices <- weekly_prices |>
        dplyr::mutate(
            time_to_treatment = weeks - start_treatment,
            # set French stations to never-treated (in terms of time)
            time_to_treatment = dplyr::case_when(
                treat_region_de == "control" ~ 999,
                TRUE ~ time_to_treatment
            )
        )

    #--------------------------------------------------
    # return

    return(weekly_prices)
}