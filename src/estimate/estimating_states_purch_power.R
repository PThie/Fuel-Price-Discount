estimating_states_purch_power <- function(
    price_data = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    german_municipalities = NA
) {
    #' @title Estimating States Purchasing Power
    #' 
    #' @description This function estimates the competition (purch power)
    #' effects for the different states.
    #' 
    #' @param price_data Fuel price data April to August 2022.
    #' @param german_stations Data frame with German stations.
    #' @param microm_data_cleaned Cleaned microm data.
    #' @param german_municipalities Data frame with German municipalities.
    #' 
    #' @return List with results for each state.
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

        # NOTE: exclude Saarland for now (because for pp_cat = 8 there are no
        # treat_tankrabatt_de == treated for Germany, therefore process cannot
        # be estimated). We probably do no need SL anyway.
        if (suffix_state != "SL") {
            result <- making_purch_power(
                price_data = state_price_data,
                german_stations = german_stations,
                microm_data_cleaned = microm_data_cleaned,
                german_municipalities = german_municipalities,
                suffix_export = suffix_state
            )
        }

        # store result
        results_list[[suffix_state]] <- result
        
        plotting_purch_power_effects(
            effects = result,
            suffix_export = suffix_state
        )
    }

    #--------------------------------------------------
    # return

    return(results_list)
}