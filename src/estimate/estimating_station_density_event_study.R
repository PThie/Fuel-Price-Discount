estimating_station_density_event_study <- function(
    fuel_prices_april_august = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    suffix_export = NA
) {
    #' @title Estimating Station Density Event Study
    #' 
    #' @description This function estimates the event study effects of the
    #' station density categories.
    #' 
    #' @param fuel_prices_april_august Fuel price data April to August 2022.
    #' @param german_stations German stations data.
    #' @param microm_data_cleaned Microm data cleaned.
    #' @param suffix_export Suffix for export files.
    #' 
    #' @return List with estimated coefficients for the event study.
    #' @author Patrick Thiel

    #----------------------------------------------
    # summarise car density on municipality level
    # summarise stations on district level
    # NOTE: not all municipalities have stations

    cardensity_municipality <- microm_data_cleaned |>
        dplyr::group_by(AGS) |>
        dplyr::summarise(
            median_car_density = median(car_density, na.rm = TRUE)
        ) |>
        # add district AGS
        dplyr::mutate(
            AGS_district = substring(AGS, 1, 5)
        ) |>
        as.data.frame()

    stations_districts <- german_stations |>
        sf::st_drop_geometry() |>
        dplyr::group_by(AGS_district) |>
        dplyr::summarise(
            count_stations = n()
        ) |>
        as.data.frame()

    # merge station count to car density
    cardensity_stations <- merge(
        cardensity_municipality,
        stations_districts,
        by.x = "AGS_district",
        by.y = "AGS_district",
        all.x = TRUE
    )

    # clean car density data
    # adjust median municipality car density by number of stations per district
    cardensity_stations_clean <- cardensity_stations |>
        dplyr::select(-AGS_district) |>
        dplyr::mutate(
            station_density_adj = count_stations / median_car_density 
        )

    # get quantiles
    quant_cd <- as.numeric(
        quantile(
            cardensity_stations_clean$station_density_adj,
            prob = seq(0, 1, 0.1),
            na.rm = TRUE
        )
    )

    # add categories
    cardensity_stations_clean <- cardensity_stations_clean |>
        dplyr::mutate(
            station_density_cat = dplyr::case_when(
                station_density_adj >= 0 & station_density_adj < quant_cd[2] ~ "1",
                station_density_adj >= quant_cd[2] & station_density_adj < quant_cd[3] ~ "2",
                station_density_adj >= quant_cd[3] & station_density_adj < quant_cd[4] ~ "3",
                station_density_adj >= quant_cd[4] & station_density_adj < quant_cd[5] ~ "4",
                station_density_adj >= quant_cd[5] & station_density_adj < quant_cd[6] ~ "5",
                station_density_adj >= quant_cd[6] & station_density_adj < quant_cd[7] ~ "6",
                station_density_adj >= quant_cd[7] & station_density_adj < quant_cd[8] ~ "7",
                station_density_adj >= quant_cd[8] & station_density_adj < quant_cd[9] ~ "8",
                station_density_adj >= quant_cd[9] & station_density_adj < quant_cd[10] ~ "9",
                station_density_adj >= quant_cd[10] & station_density_adj <= quant_cd[11] ~ "10"
            ),
            station_density_cat = factor(
                station_density_cat,
                levels = seq(1, 10, 1)
            )
        )

    #----------------------------------------------
    # assign muncipality info to stations

    german_stations_cd <- merge(
        german_stations |>
            sf::st_drop_geometry(),
        cardensity_stations_clean,
        by = "AGS",
        all.x = TRUE
    ) |>
    dplyr::select(-AGS_district)

    # merge to price data
    avg_prices_cd <- merge(
        fuel_prices_april_august,
        german_stations_cd,
        by = "station_id",
        all.x = TRUE
    )

    # adjust values for France
    avg_prices_cd <- avg_prices_cd |>
        dplyr::mutate(
            station_density_cat = dplyr::case_when(
                country == "FR" ~ "0",
                TRUE ~ station_density_cat
            )
        )
    
    #----------------------------------------------
    # event study analysis

    # add time to treatment in days
    avg_prices_event <- avg_prices_cd |>
        dplyr::mutate(
            time_to_treatment = as.numeric(difftime(
                as.Date(date, "%Y-%m-%d"),
                config_globals()[["start_tr_de"]],
                units = "days"
            )),
            # set French stations to never-treated (in terms of time)
            time_to_treatment = dplyr::case_when(
                treat_region_de == "control" ~ 999,
                TRUE ~ time_to_treatment
            )
        )

    # define estimation function
    est_fun_event <- function(moddata, depvar = c("diesel", "e10"), pp_cat) {
        var_aux <- rlang::sym("station_density_cat")
        moddata_short <- moddata |>
            dplyr::filter(!!var_aux == "0" | !!var_aux == pp_cat)

        # NOTE: only run model if category is present
        if (as.character(pp_cat) %in% unique(moddata[[var_aux]])) {
            # define formula
            fm <- formula(
                paste(
                    depvar, "~",
                    paste("relevel(as.factor(time_to_treatment), \"-1\") * relevel(as.factor(treat_region_de), \"control\")"),
                    paste("| as.factor(date) + station_id")
                )
            )
            
            # estimate
            suppressMessages(est_mod <- fixest::feols(
                fml = fm,
                data = moddata_short,
                cluster = "station_id",
                notes = FALSE
            ))
        } else {
            est_mod <- "Group not present"
        }

        return(est_mod)
    }

    # define categories
    cats <- c("1", "10")
    vars <- c("diesel", "e10")
    
    # list for storage
    mod_list_event <- list()

    # loop through options
    for(var in vars) {
        for(cat in cats) {
            # list name
            list_name <- paste(var, "cd_cat", cat, sep = "_")
            # loop through options and store output
            mod_list_event[[list_name]] <- est_fun_event(
                moddata = avg_prices_event,
                depvar = var,
                pp_cat = cat
            )
        }
    }

    # get coefficient data
    get_coefficients <- function(result, suffix_export) {
        est_data <- mod_list_event[[result]]

        if (length(est_data) > 1) {
            # export raw results
            fixest::esttex(
                est_data,
                file = file.path(
                    config_paths()[["output_path"]],
                    "estimation",
                    paste0(
                        "station_density_",
                        result,
                        "_",
                        suffix_export,
                        ".tex"
                    )
                ),
                digits = "r3", cluster = "station_id",
                dict = config_globals()[["coefnames"]],
                replace = TRUE,
                signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
            )

            # extract coefficients
            coef <- as.data.frame(est_data$coefficients)
            coef$var <- row.names(coef)
            row.names(coef) <- seq(1, nrow(coef), 1)
        
            coef_prep <- coef |>
                dplyr::rename(
                    coefficient = `est_data$coefficients`
                ) |>
                dplyr::mutate(
                    time = substr(var, start = 44, stop = 46)
                )

            # get confidence intervals
            confidence <- confint(est_data, level = 0.95) |>
                dplyr::rename(
                    lower = `2.5 %`,
                    upper = `97.5 %`
                ) |>
                as.data.frame()

            confidence_prep <- confidence |>
                dplyr::mutate(
                    var = row.names(confidence),
                    time = substr(var, start = 44, stop = 46)
                )
            row.names(confidence_prep) <- seq(1, nrow(confidence_prep), 1)

            # combine both
            final_prep <- merge(
                coef_prep |>
                    dplyr::select(time, coefficient),
                confidence_prep |>
                    dplyr::select(time, lower, upper),
                by = "time"
            )

            final_prep$time <- as.numeric(final_prep$time)

            # add reference point
            final_prep <- rbind(
                final_prep,
                as.data.frame(
                    cbind(
                        time = -1,
                        coefficient = 0,
                        lower = 0,
                        upper = 0
                    )
                )
            )

            # add months
            if (suffix_export == "twoweeks") {
                final_prep <- final_prep |>
                    dplyr::mutate(
                        months = dplyr::case_when(
                            time >= -61 & time <= -32 ~ 4,
                            time >= -31 & time <= -1 ~ 5,
                            time >= 0 & time <= 29 ~ 6
                        )
                    )
            } else {
                final_prep <- final_prep |>
                    dplyr::mutate(
                        months = dplyr::case_when(
                            time >= -61 & time <= -32 ~ 4,
                            time >= -31 & time <= -1 ~ 5,
                            time >= 0 & time <= 29 ~ 6,
                            time >= 30 & time <= 60 ~ 7,
                            time >= 61 & time <= 91 ~ 8
                        )
                    )
            }

            # export table
            filename <- paste0(
                "station_density_",
                result,
                "_",
                suffix_export,
                ".xlsx"
            )
            openxlsx::write.xlsx(
                final_prep,
                file.path(
                    config_paths()[["output_path"]],
                    "estimation",
                    filename
                ),
                rowNames = FALSE
            )
        } else {
            final_prep <- data.frame(
                time = NA,
                coefficient = NA,
                lower = NA,
                upper = NA,
                months = NA
            )
        }

        # return
        return(final_prep)
    }

    # store estimated coefficients
    coef_event <- list()
    for(nam in names(mod_list_event)) {
        coef_event[[nam]] <- get_coefficients(
            result = nam,
            suffix_export = suffix_export
        )
    }

    #--------------------------------------------------
    # return

    return(coef_event)
}