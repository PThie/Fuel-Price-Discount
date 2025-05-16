estimating_baseline_effects_placebo <- function(
    price_data = NA
) {
    #' @title Estimating the impact of German Fuel Tax Discount in placebo period
    #' 
    #' @description Estimating the impact of the German fuel tax discount (FTD)
    #' on fuel prices with a diff-in-diff approach but assuming that the FTD
    #' occured one year before its actual implementation (placebo approach).
    #' 
    #' @param price_data Fuel price data for April to August for different years
    #' 
    #' @return Returns estimation results
    #' @author Patrick Thiel
    #' 
    #----------------------------------------------
    # estimation with FE

    # define choices for different gasoline types
    dep_cases <- c("diesel", "e10")

    # complete results list
    mod_list_complete <- list()

    # loop through years
    for (year in names(price_data)) {
        # subset data
        dta <- price_data[[year]]

        # define lists for storages
        mod_list_gastype <- list()

        # loop through FE definitons and gasoline types
        suppressMessages(for(dep_case in dep_cases) {
            mod <- est_fun(
                moddata = dta,
                depvar = dep_case,
                fixef = "time_region",
                event = FALSE,
                twoway_clustering = FALSE
            )
            mod_list_gastype[[dep_case]] <- mod
        })

        # export
        fixest::esttex(
            mod_list_gastype[["diesel"]],
            mod_list_gastype[["e10"]],
            headers = c("diesel", "petrol"),
            digits = "r3",
            cluster = "station_id",
            dict = config_globals()[["coefnames"]],
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0(
                    "did_est_Germany_FE_placebo_",
                    year,
                    ".tex"
                )
            )
        )

        # store
        mod_list_complete[[year]] <- mod_list_gastype
    }

    #--------------------------------------------------
    # return

    return(mod_list_complete)
}