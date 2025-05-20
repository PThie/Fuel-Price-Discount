estimating_french_ftd <- function(
    price_data = NA
) {
    #' @title Estimation of the French fuel price discount
    #' 
    #' @description This function estimates the impact of the fuel price discount
    #' in France on fuel prices.
    #' 
    #' @param price_data Data frame with the prepared fuel price data
    #' 
    #' @return Returns a list with the estimated coefficients
    #' @author Patrick Thiel

    #--------------------------------------------------
    # estimation with FE

    # define choise for gasoline types
    dep_cases <- c("diesel", "e10")

    # define lists for storages
    mod_list_gastype <- list()

    # loop through FE definitons and gasoline types
    suppressMessages(for(dep_case in dep_cases){
        mod <- est_fun_french_ftd(
            moddata = price_data,
            depvar = dep_case,
            twoway_clustering = TRUE,
            event = FALSE
        )
        mod_list_gastype[[dep_case]] <- mod
    })

    #----------------------------------------------
    # calculate the average prices in the FTD period

    mean_prices_ftd <- price_data |>
        # after FTD started
        dplyr::filter(treat_ftd_fr == "treated") |>
        # filter for Germany
        dplyr::filter(country == "FR") |>
        dplyr::summarise(
            mean_diesel = mean(diesel, na.rm = TRUE),
            se_diesel = sd(diesel, na.rm = TRUE) / sqrt(n()),
            mean_e10 = mean(e10, na.rm = TRUE),
            se_e10 = sd(e10, na.rm = TRUE) / sqrt(n())
        )
    
    #--------------------------------------------------
    # calculate standard errors for pass-through estimates
    # by carrying over the SE from the interaction term in relation to the size
    # of the FTD

    se_results <- list()
    for (dep_case in dep_cases) {
        if (dep_case == "diesel") {
            FTD <- -0.18
            mean_price <- mean_prices_ftd$mean_diesel
            se_price <- mean_prices_ftd$se_diesel
        } else {
            FTD <- -0.18
            mean_price <- mean_prices_ftd$mean_e10
            se_price <- mean_prices_ftd$se_e10
        }

        # extract SEs and do some renaming
        ses <- as.data.frame(mod_list_gastype[[dep_case]]$se)
        ses$var <- row.names(ses)
        row.names(ses) <- seq(1, nrow(ses), 1)
        colnames(ses) <- c("SE", "var")

        # extract interaction term
        coef <- as.data.frame(mod_list_gastype[[dep_case]]$coefficient)
        coef$var <- row.names(coef)
        row.names(coef) <- seq(1, nrow(coef), 1)
        colnames(coef) <- c("coefficient", "var")

        # only keep interaction term
        ses <- ses |>
            dplyr::filter(stringr::str_detect(var, "treated:relevel")) |>
            merge(coef, by = "var", all.x = TRUE) |>
            dplyr::mutate(
                # add model and fuel type
                fuel_type = dep_case,
                # add pass-through rate
                passthrough = (coefficient / FTD) * 100,
                # add SE for pass-through by dividing the model SE with FTD
                passthrough_SE = (SE / FTD) * -1,
                # add confidence intervals
                passthrough_ci_lower = passthrough - (1.96 * passthrough_SE),
                passthrough_ci_upper = passthrough + (1.96 * passthrough_SE),
                # add mean price and SE of mean price
                mean_price = mean_price,
                mean_price_SE = se_price,
                # add price reduction in percent
                price_reduction = (coefficient / mean_price) * 100
            ) |>
            dplyr::select(-var) |>
            dplyr::relocate(coefficient, .before = SE)
        
        se_results[[dep_case]] <- ses
    }

    # combine all
    se_results <- data.table::rbindlist(se_results)

    # export
    openxlsx::write.xlsx(
        se_results,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_France_pass_through_SE.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # export estimation results with FE

    min_date <- min(price_data$date)
    max_date <- max(price_data$date)
    
    for(dep_case in dep_cases){
        filename <- paste0(
            "did_est_France_FE_",
            dep_case,
            ".tex"
        )

        fixest::esttex(
            mod_list_gastype[[dep_case]],
            headers = c("bothFE"),
            title = paste("Restricted to", min_date, "to", max_date),
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                filename
            ),
            digits = "r3",
            cluster = c("station_id", "date"),
            dict = config_globals()[["coefnames_french"]],
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )
    }

    #--------------------------------------------------
    # return

    return(mod_list_gastype)
}