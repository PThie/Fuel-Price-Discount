subsetting_french_fuel_prices <- function(
    price_data = NA
) {
    #' @title Preparing French fuel prices
    #' 
    #' @description This function prepares the fuel price data for France
    #' for the analysis of the fuel price discount.
    #' 
    #' @param price_data Fuel price data
    #' 
    #' @return Returns a data frame with the prepared fuel price data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # drop columns related to German FTD

    price_data <- price_data |>
        dplyr::select(-dplyr::starts_with("treat"))

    #--------------------------------------------------
    # define dummies for treatment group (France) and treatment period (after
    # April 2022)

    price_data <- price_data |>
        dplyr::filter(date >= "2022-02-01" & date <= "2022-05-31") |>
        dplyr::mutate(
            treat_region_fr = dplyr::case_when(
                country == "FR" ~ 1,
                country != "FR" ~ 0
            ),
            treat_ftd_fr = dplyr::case_when(
                date >= "2022-04-01" ~ 1,
                date < "2022-04-01" ~ 0
            )
        )

    #--------------------------------------------------
    # return

    return(price_data)
}