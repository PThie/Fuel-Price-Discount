estimating_baseline_effects <- function(fuel_prices_april_august = NA) {
    #' @title Estimating the impact of German Fuel Tax Discount
    #' 
    #' @description Estimating the impact of the German fuel tax discount (FTD)
    #' on fuel prices with a diff-in-diff approach.
    #' 
    #' @param fuel_prices_april_august Fuel price data for April to August 2022
    #' 
    #' @return Returns estimation results
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # general information

    obs <- fuel_prices_april_august |>
        dplyr::group_by(country) |>
        dplyr::summarise(
            overall_n = n(),
            station_n = n_distinct(station_id)
        ) |>
        as.data.frame()

    openxlsx::write.xlsx(
        obs,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "observations_est_period.xlsx"
        ),
        rowNames = FALSE
    )    

    #----------------------------------------------
    # estimation with FE

    # define choices for FE and different gasoline types
    fe_cases <- c("none", "region", "time", "time_region")
    dep_cases <- c("diesel", "e10")

    # define lists for storages
    mod_list_gastype <- list()
    mod_list_complete <- list()

    # loop through FE definitons and gasoline types
    suppressMessages(for(dep_case in dep_cases) {
        for(fe_case in fe_cases){
            mod <- est_fun(
                moddata = fuel_prices_april_august,
                depvar = dep_case,
                fixef = fe_case,
                event = FALSE
            )
            mod_list_gastype[[fe_case]] <- mod
        }
        mod_list_complete[[dep_case]] <- mod_list_gastype
    })

    #----------------------------------------------
    # calculate the average prices in the FTD period

    mean_prices_ftd <- fuel_prices_april_august |>
        # after FTD started
        filter(treat_tankrabatt_de == "treated") |>
        # filter for Germany
        filter(country == "DE") |>
        summarise(
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
        for(fe_case in c("none", "time_region")) {
            if (dep_case == "diesel") {
                FTD <- -0.1671
                mean_price <- mean_prices_ftd$mean_diesel
                se_price <- mean_prices_ftd$se_diesel
            } else {
                FTD <- -0.3516
                mean_price <- mean_prices_ftd$mean_e10
                se_price <- mean_prices_ftd$se_e10
            }

            # extract SEs and do some renaming
            ses <- as.data.frame(mod_list_complete[[dep_case]][[fe_case]]$se)
            ses$var <- row.names(ses)
            row.names(ses) <- seq(1, nrow(ses), 1)
            colnames(ses) <- c("SE", "var")

            # extract interaction term
            coef <- as.data.frame(mod_list_complete[[dep_case]][[fe_case]]$coefficient)
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
                    model = fe_case,
                    model = dplyr::case_when(
                        model == "none" ~ "OLS",
                        TRUE ~ "FE"
                    ),
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
            
            se_results[[paste(dep_case, fe_case, sep = "_")]] <- ses
        }
    }

    # combine all
    se_results <- data.table::rbindlist(se_results)

    # export
    openxlsx::write.xlsx(
        se_results,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_Germany_pass_through_SE.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # export estimation results with FE

    min_date <- min(fuel_prices_april_august$date)
    max_date <- max(fuel_prices_april_august$date)
    
    for(dep_case in dep_cases){
        filename <- paste0("did_est_Germany_FE_", dep_case, ".tex")

        fixest::esttex(
            mod_list_complete[[dep_case]][["none"]],
            mod_list_complete[[dep_case]][["region"]],
            mod_list_complete[[dep_case]][["time"]],
            mod_list_complete[[dep_case]][["time_region"]],
            headers = c("OLS", "regionFE", "timeFE", "bothFE"),
            title = paste("Restricted to", min_date, "to", max_date),
            file = file.path(config_paths()[["output_path"]], "estimation", filename),
            digits = "r3",
            cluster = "station_id",
            dict = config_globals()[["coefnames"]],
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )
    }

    #--------------------------------------------------
    # estimation like in Fuest et al. (2022; Perspektiven d. Wirtschaftspolitik)

    # restrict sample to one before and two weeks after FTD
    restricted_sample <- fuel_prices_april_august |>
        dplyr::filter(date >= "2022-05-15" & date <= "2022-06-14")

    # define lists for storages
    mod_list_gastype_restricted <- list()
    mod_list_complete_restricted <- list()

    # loop through FE definitons and gasoline types
    suppressMessages(for(dep_case in dep_cases) {
        for(fe_case in fe_cases) {
            mod <- est_fun(
                moddata = restricted_sample,
                depvar = dep_case,
                fixef = fe_case,
                event = FALSE
            )
            mod_list_gastype_restricted[[fe_case]] <- mod
        }
        mod_list_complete_restricted[[dep_case]] <- mod_list_gastype_restricted
    })

    # export
    min_date <- min(restricted_sample$date)
    max_date <- max(restricted_sample$date)
    
    for(dep_case in dep_cases){
        filename <- paste0("did_est_Germany_FE_restricted_", dep_case, ".tex")

        fixest::esttex(
            mod_list_complete_restricted[[dep_case]][["none"]],
            mod_list_complete_restricted[[dep_case]][["region"]],
            mod_list_complete_restricted[[dep_case]][["time"]],
            mod_list_complete_restricted[[dep_case]][["time_region"]],
            headers = c("OLS", "regionFE", "timeFE", "bothFE"),
            title = paste("Restricted to", min_date, "to", max_date),
            file = file.path(config_paths()[["output_path"]], "estimation", filename),
            digits = "r3", cluster = "station_id",
            dict = config_globals()[["coefnames"]],
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )
    }
    
    # calculate pass-through rate and its SE
    se_results <- list()
    for (dep_case in dep_cases) {
        for(fe_case in c("none", "time_region")) {
            if (dep_case == "diesel") {
                FTD <- -0.1671
            } else {
                FTD <- -0.3516
            }

            # extract SEs and do some renaming
            ses <- as.data.frame(mod_list_complete_restricted[[dep_case]][[fe_case]]$se)
            ses$var <- row.names(ses)
            row.names(ses) <- seq(1, nrow(ses), 1)
            colnames(ses) <- c("SE", "var")

            # extract interaction term
            coef <- as.data.frame(mod_list_complete_restricted[[dep_case]][[fe_case]]$coefficient)
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
                    model = fe_case,
                    model = case_when(
                        model == "none" ~ "OLS",
                        TRUE ~ "FE"
                    ),
                    # add pass-through rate
                    passthrough = (coefficient / FTD) * 100,
                    # add SE for pass-through by dividing the model SE with FTD
                    passthrough_SE = (SE / FTD) * -1,
                    # add confidence intervals
                    passthrough_ci_lower = passthrough - (1.96 * passthrough_SE),
                    passthrough_ci_upper = passthrough + (1.96 * passthrough_SE),
                ) |>
                dplyr::select(-var) |>
                dplyr::relocate(coefficient, .before = SE)

            se_results[[paste(dep_case, fe_case, sep = "_")]] <- ses
        }
    }

    # combine all
    se_results <- data.table::rbindlist(se_results)

    # export
    openxlsx::write.xlsx(
        se_results,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_Germany_pass_through_SE_restricted.xlsx"
        ),
        rowNames = FALSE
    )
}