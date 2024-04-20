estimating_regional_effect <- function(
    fuel_prices_april_august = NA,
    german_stations = NA,
    german_districts
) {
    #' @title Regional effect on district-level
    #' 
    #' @description This function estimates the fuel price discount
    #' on the district-level.
    #' 
    #' @param fuel_prices_april_august Fuel price data April to August 2022
    #' @param german_stations German station information
    #' @param german_districts German district information
    #' 
    #' @return Returns data frame with regional effects and maps
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # merge station data with district info to price data

    avg_prices_district <- merge(
        fuel_prices_april_august,
        german_stations |>
            sf::st_drop_geometry(),
        by = "station_id",
        all.x = TRUE
    )

    # replace district ags for France with fictional number
    avg_prices_district <- avg_prices_district |>
        dplyr::mutate(
            AGS_district = case_when(
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
        est_mod <- feols(
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
                paste0("did_est_regional_", var, ".tex")
            ),
            digits = "r3", cluster = "station_id",
            replace = TRUE,
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
        )

        # extract coefficients
        coeffs <- as.data.frame(cbind(coefficient = est$coefficient))

        # redefine row names
        coeffs$variable <- row.names(coeffs)
        row.names(coeffs) <- seq(1, nrow(coeffs))

        # make sure that only interaction is left
        coeffs <- coeffs |>
            dplyr::filter(
                stringr::str_detect(variable, "treated:relevel") == TRUE
            )

        # calculate confidence interval
        con <- confint(est, level = 0.95)

        # add CI to data
        coeffs <- coeffs |>
            dplyr::mutate(
                lower_ci = con[, 1],
                upper_ci = con[, 2],
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

        return(coeffs)
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
            "did_est_regional.xlsx"
        ),
        rowNames = FALSE
    )

    # get ranges
    range_regional_effets <- results |>
        group_by(mod) |>
        summarise(
            max_pass = max(passthrough),
            min_pass = min(passthrough)
        ) |>
        as.data.frame()

    # export
    openxlsx::write.xlsx(
        range_regional_effets,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            "did_est_regional_ranges.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # map of effects

    # merge district info
    results_districts <- merge(
        results,
        german_districts,
        by.x = "ags_district",
        by.y = "AGS",
        all.x = TRUE
    )

    # set geometry
    results_districts <- sf::st_set_geometry(
        results_districts,
        results_districts$geometry
    )

    # loop through variables
    for(var in vars) {
        # define breaks
        if (var == "diesel") {
            br <- seq(50, 140, 10)
            lim <- c(50, 140)
        } else {
            br <- seq(50, 110, 10)
            lim <- c(50, 110)
        }
        # generate map
        map <- ggplot()+
            geom_sf(
                data = results_districts |>
                    filter(mod == var),
                aes(geometry = geometry, fill = passthrough)
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = "Pass-through rate\n(in %)",
                breaks = br,
                limits = lim
            )+
            theme_void()+
            theme(
                legend.position = "bottom",
                legend.title = element_text(size = 14, vjust = 0.8),
                legend.key.size = unit(0.8, "cm"),
                legend.text = element_text(size = 11, angle = 90, vjust = 0.5)
            )

        # export
        filename <- paste0("regional_price_effect_", var, ".png")
        suppressMessages(ggsave(
            plot = map,
            file.path(
                config_paths()[["output_path"]],
                "maps",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        ))
    }

    #--------------------------------------------------
    # return

    return(results)
}