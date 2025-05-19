subsetting_fuel_prices <- function(fuel_prices = NA) {
    #' @title Subsetting fuel prices
    #' 
    #' @description This function subsets the fuel prices to the analysis time
    #' period April to August 2022.
    #' 
    #' @param fuel_prices Fuel price data.
    #' 
    #' @return Dataframe with fuel prices April to August 2022.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # restrict to April to August 2022

    avg_prices_sep <- fuel_prices |>
        dplyr::filter(date >= "2022-04-01") |>
        dplyr::filter(date <= config_globals()[["end_tr_de"]])

    #--------------------------------------------------
    # add month variable

    avg_prices_sep <- avg_prices_sep |>
        dplyr::mutate(
            months = format(date, "%Y-%m")
        )

    #--------------------------------------------------
    # return

    return(avg_prices_sep)
}