subsetting_morning_rush <- function(fuel_prices = NA) {
    #' @title Subsetting fuel prices to morning rush hours
    #' 
    #' @description This function subsets the fuel prices to morning rush hours
    #' 6 am to 10 am.
    #' 
    #' @param fuel_prices Fuel price data.
    #' 
    #' @return Dataframe with fuel prices at morning.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # restrict to central columns

    dta_prep <- fuel_prices |>
        dplyr::select(id, date, months, time, diesel, e10, e5)

    #--------------------------------------------------
    # keep only price changes from moring rush hours
    # morning rush hours: 6 am to 10 am

    dta_prep <- dta_prep |>
        dplyr::mutate(
            hours = substring(as.character(time), 1, 2) |>
                as.numeric()
        )

    # select only changes during the rush hours
    # average to daily prices
    rush_hours <- dta_prep |>
        dplyr::filter(hours >= 6 & hours <= 10) |>
        dplyr::group_by(id, date) |>
        dplyr::summarise(
            diesel = mean(diesel, na.rm = TRUE),
            e10 = mean(e10, na.rm = TRUE),
            e5 = mean(e5, na.rm = TRUE)
        ) |>
        as.data.frame()
    
    #--------------------------------------------------
    # return

    return(rush_hours)
}