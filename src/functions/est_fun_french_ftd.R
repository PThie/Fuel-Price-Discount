est_fun_french_ftd <- function(
    moddata = NA,
    depvar = c("diesel", "e10"),
    event = FALSE
) {
    #' @title Estimation function for French fuel prices
    #' 
    #' @description This function estimates the impact of the fuel price discount
    #' in France on fuel prices.
    #' 
    #' @param moddata Data frame with the prepared fuel price data
    #' @param depvar Character, the dependent variable to be estimated. Either
    #' "diesel" or "e10".
    #' @param event Logical, if TRUE, the function estimates an event study
    #'  
    #' @return Returns a fixest object with the estimated coefficients
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define formulas

    # formula with station and date fixed effects
    fm <- formula(
        paste(
            depvar, "~",
            paste("relevel(as.factor(treat_ftd_fr), \"control\") * relevel(as.factor(treat_region_fr), \"control\")"),
            paste("| as.factor(date) + station_id")
        )
    )

    # formula for event study
    if(event == TRUE) {
        fm <- formula(
            paste(
                depvar, "~",
                paste("relevel(as.factor(time_to_treatment), \"-1\") * relevel(as.factor(treat_region_fr), \"control\")"),
                paste("| as.factor(date) + station_id")
            )
        )
    }

    #--------------------------------------------------
    # actual estimation

    est_mod <- fixest::feols(
        fml = fm,
        data = moddata,
        cluster = c("station_id", "date")
    )

    #--------------------------------------------------
    # return

    return(est_mod)
}