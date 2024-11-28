estimating_temperature_impact <- function(
    fuel_prices = NA,
    station_temperature_data = NA
) {
    #' @title Estimating the impact of temperature on fuel prices
    #' 
    #' @description This function estimates the impact of temperature on fuel prices.
    #' 
    #' @param fuel_prices Fuel price data April to August 2022
    #' @param station_temperature_data Temperature data for stations
    #' 
    #' @return List with estimation results
    #' @author Patrick Thiel

    #--------------------------------------------------
    # combine fuel prices and temperature data

    fuel_prices_temp <- merge(
        fuel_prices,
        station_temperature_data,
        by = c("station_id", "date"),
        all.x = TRUE
    )

    #--------------------------------------------------
    # apply estimation function

    results <- estimating_baseline_event_study(
        price_data = fuel_prices_temp,
        suffix_export = "temperature",
        temperature = TRUE
    )

    #--------------------------------------------------
    # return

    return(results)
}