estimating_search_behavior_passthrough <- function(
    time_effects_diesel = NA,
    time_effects_petrol = NA,
    google_trends_data = NA
) {
    #' @title Testing the effect of search behavior on price passthrough
    #' 
    #' @description This function estimates the effect of search behavior
    #' based on Google scores on price passthrough.
    #' 
    #' @param time_effects_diesel Data frame with time effects for diesel
    #' @param time_effects_petrol Data frame with time effects for petrol
    #' @param google_trends_data Data frame with Google trends data
    #' 
    #' @return NULL, estimation results
    #' @author Patrick Thiel

    #--------------------------------------------------
    # merge price effects and google data

    fuel_types <- c("petrol", "diesel")
    data_storage <- list()

    for (fuel_type in fuel_types) {
        if (fuel_type == "petrol") {
            time_effects <- time_effects_petrol
        } else {
            time_effects <- time_effects_diesel
        }

        # add google trends data
        merged_data <- dplyr::left_join(
            time_effects,
            google_data,
            by = "date"
        )

        # store data
        data_storage[[fuel_type]] <- merged_data
    }

    #--------------------------------------------------
    # estimation

    for (fuel_type in fuel_types) {
        dta <- data_storage[[fuel_type]]

        # estimation
        est_mod_price <- fixest::feols(
            coefficient ~ score + score2,
            data = dta,
            se = "hetero"
        )

        est_mod_passthrough <- fixest::feols(
            passthrough ~ score + score2,
            data = dta,
            se = "hetero"
        )

        fixest::esttex(
            est_mod_price, est_mod_passthrough,
            headers = c("Price", "Passthrough"),
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0("search_behavior_", fuel_type, ".tex")
            ),
            digits = "r3", se = "hetero", replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )
    }

    #--------------------------------------------------
    # return
    
    return(NULL)
}