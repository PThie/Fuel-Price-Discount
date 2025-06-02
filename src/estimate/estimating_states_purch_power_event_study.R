estimating_states_purch_power_event_study <- function(
    price_data = NA,
    german_stations = NA,
    microm_data_cleaned = NA
) {
    #' @title Estimating States Purchasing Power Effects in Event Study Design
    #' 
    #' @description This function estimates the effects of purchasing power on
    #' fuel prices pass-through in an event study design for each state.
    #' 
    #' @param price_data Data frame containing fuel prices and state information.
    #' @param german_stations Data frame containing information about German fuel stations.
    #' @param microm_data_cleaned Data frame containing cleaned microm data.
    #' 
    #' @return List of results for each state, containing estimated effects.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # loop through states

    state_ids <- unique(price_data$state_id)[
        !is.na(unique(price_data$state_id))
    ]

    results_list <- list()
    for (state in state_ids) {
        # subset data for state
        state_price_data <- price_data |>
            dplyr::filter(state_id == state | is.na(state_id))

        suffix_state <- unique(state_price_data$state_name_short)[
            !is.na(unique(state_price_data$state_name_short))
        ]

        result <- estimating_purch_power_event_study(
            fuel_prices_april_august = state_price_data,
            german_stations = german_stations,
            microm_data_cleaned = microm_data_cleaned,
            suffix_export = suffix_state
        )

        results_list[[suffix_state]] <- result

        plotting_purch_power_effects_event_study(
            effects = result,
            suffix_export = suffix_state
        )
    }

    #--------------------------------------------------
    # return

    return(results_list)
}