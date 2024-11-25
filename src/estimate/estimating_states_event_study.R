estimating_states_event_study <- function(
    price_data = NA
) {
    #' @title Estimating an event study for the different states
    #' 
    #' @description This function estimates an event study for the state-specific
    #' prices comparing the day effects to the last day before implementing the FTD.
    #' 
    #' @param price_data Fuel price data April to August 2022
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

        estimating_baseline_event_study(
            price_data = state_price_data,
            suffix_export = suffix_state
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}