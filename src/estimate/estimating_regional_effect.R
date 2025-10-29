estimating_regional_effect <- function(
    price_data = NA,
    german_stations = NA,
    suffix_export = NA
) {
    #' @title Regional effect on district-level
    #' 
    #' @description This function estimates the fuel price discount
    #' on the district-level.
    #' 
    #' @param price_data Fuel price data April to August 2022
    #' @param german_stations German station information
    #' @param suffix_export Suffix for export files
    #' 
    #' @return Returns data frame with regional effects and maps
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # merge station data with district info to price data

    avg_prices_district <- merge(
        price_data,
        german_stations |>
            sf::st_drop_geometry(),
        by = "station_id",
        all.x = TRUE
    )

    # replace district ags for France with fictional number
    avg_prices_district <- avg_prices_district |>
        dplyr::mutate(
            AGS_district = dplyr::case_when(
                country == "FR" ~ "99999",
                TRUE ~ AGS_district
            )
        )

    #----------------------------------------------
    # estimation
    
    # estimation function
    est_fun <- function(moddata, depvar = c("diesel", "e10")) {
        # define formula
        fm <- formula(
            paste(
                depvar , "~",
                paste("as.factor(AGS_district) +"),
                paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(AGS_district), \"99999\")"),
                paste("| as.factor(date) + station_id")
            )
        )

        # estimate
        est_mod <- fixest::feols(
            fml = fm,
            data = moddata,
            cluster = "station_id",
            notes = FALSE
        )

        # return output
        return(est_mod)
    }

    # define fuel types
    vars <- c("diesel", "e10")

    # loop through options
    est_results <- list()
    for(var in vars) {
        # loop through options and store output
        est_results[[var]] <- est_fun(
            moddata = avg_prices_district,
            depvar = var
        )
    }

    #----------------------------------------------
    # get coefficients

    get_coefficients <- function(var) {
        # get estimates
        est <- est_results[[var]]

        # export as tex
        fixest::esttex(
            est,
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0(
                    "did_est_regional_",
                    var,
                    "_",
                    suffix_export,
                    ".tex"
                )
            ),
            digits = "r3", cluster = "station_id",
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )

        # extract coefficients
        coeffs <- as.data.frame(cbind(coefficient = est$coefficient))
        ses <- as.data.frame(cbind(se = est$se))

        # redefine row names
        coeffs$variable <- row.names(coeffs)
        row.names(coeffs) <- seq(1, nrow(coeffs))
        ses$variable <- row.names(ses)
        row.names(ses) <- seq(1, nrow(ses))

        # make sure that only interaction is left
        coeffs <- coeffs |>
            dplyr::filter(
                stringr::str_detect(variable, "treated:relevel") == TRUE
            )

        ses <- ses |>
            dplyr::filter(
                stringr::str_detect(variable, "treated:relevel") == TRUE
            )

        coeffs_merged <- merge(
            coeffs,
            ses,
            by = "variable"
        )

        # add CI to data
        coeffs_merged <- coeffs_merged |>
            dplyr::mutate(
                lower_ci = coefficient - 1.96 * se,
                upper_ci = coefficient + 1.96 * se,
                # add fuel type
                mod = var,
                # extract AGS
                ags_district = substring(
                    variable,
                    first = nchar(variable) - 4,
                    last = nchar(variable)
                ),
                ags_district = dplyr::case_when(
                    stringr::str_detect(ags_district, "\\)") ~ stringr::str_replace(ags_district, "\\)", "0"),
                    TRUE ~ ags_district
                )
            ) |>
            # remove variable
            dplyr::select(-variable)

        return(coeffs_merged)
    }

    diesel_results <- get_coefficients(var = "diesel")
    e10_results <- get_coefficients(var = "e10")

    # row bind both results
    results <- rbind(diesel_results, e10_results)

    # calculate the passthrough rate
    results <- results |>
        dplyr::group_by(mod) |>
        dplyr::mutate(
            passthrough = dplyr::case_when(
                mod == "diesel" ~ (coefficient / -0.1671) * 100,
                TRUE ~ (coefficient / -0.3516) * 100
            )
        ) |>
        dplyr::ungroup() |>
        as.data.frame()

    # export
    openxlsx::write.xlsx(
        results,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            paste0(
                "did_est_regional_",
                suffix_export,
                ".xlsx"
            )
        ),
        rowNames = FALSE
    )

    # get ranges
    range_regional_effects <- results |>
        dplyr::group_by(mod) |>
        dplyr::summarise(
            max_pass = max(passthrough),
            min_pass = min(passthrough)
        ) |>
        as.data.frame()

    # export
    openxlsx::write.xlsx(
        range_regional_effects,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            paste0(
                "did_est_regional_ranges_",
                suffix_export,
                ".xlsx"
            )
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(results)
}