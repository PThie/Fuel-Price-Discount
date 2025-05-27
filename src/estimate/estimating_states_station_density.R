estimating_states_station_density <- function(
    price_data = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    german_municipalities = NA,
    german_districts = NA
) {
    #' @title Estimating States Station Density
    #' 
    #' @description This function estimates the competition (station density)
    #' effects for the different states.
    #' 
    #' @param price_data Fuel price data April to August 2022.
    #' 
    #' @return NULL, direct export
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through states

    state_ids <- unique(price_data$state_id)[
        !is.na(unique(price_data$state_id))
    ]

    for (state in state_ids) {
        # subset data for state
        state_price_data <- price_data |>
            dplyr::filter(state_id == state | is.na(state_id))

        suffix_state <- unique(state_price_data$state_name_short)[
            !is.na(unique(state_price_data$state_name_short))
        ]

        result <- making_station_density(
            fuel_prices_april_august = state_price_data,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            german_municipalities = german_municipalities,
            german_districts = german_districts, 
            suffix_export = suffix_state
        )

        plotting_station_density_effects(
            effects = result,
            suffix_export = suffix_state
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}