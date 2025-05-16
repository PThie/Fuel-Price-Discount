estimating_baseline_event_study_placebo <- function(
    price_data = NA
) {
    #' @title Estimating an event study for the baseline
    #' 
    #' @description This function estimates an event study for the baseline
    #' comparing the day effects to the last day before implementing the FTD.
    #' 
    #' @param price_data Fuel price data April to August for different years
    #' 
    #' @return List with estimation results
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # event study analysis

    # define different gasoline types
    dep_cases <- c("diesel", "e10")

    # complete results list
    mod_list_complete <- list()

    # loop through years
    for (year in names(price_data)) {
        # subset data
        dta <- price_data[[year]]
    
        # estimation
        mod_list_event <- list()

        # loop through gasoline types
        suppressMessages(for(dep_case in dep_cases){
            mod <- est_fun(
                moddata = dta,
                depvar = dep_case,
                fixef = "time_region",
                event = TRUE,
                temperature = FALSE
            )
            mod_list_event[[dep_case]] <- mod
        })

        # store
        mod_list_complete[[year]] <- mod_list_event
    }

    #--------------------------------------------------
    # return

    return(mod_list_complete)
}
