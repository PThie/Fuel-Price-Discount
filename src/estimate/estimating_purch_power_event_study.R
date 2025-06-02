estimating_purch_power_event_study <- function(
    fuel_prices_april_august = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    suffix_export = NA
) {
    #' @title Estimating Purchasing Power Event Study
    #' 
    #' @description This function estimates the event study effects of the
    #' purchasing power categories.
    #' 
    #' @param fuel_prices_april_august Fuel price data April to August 2022.
    #' @param german_stations German stations data.
    #' @param microm_data_cleaned Microm data cleaned.
    #' @param suffix_export Suffix for export files.
    #' 
    #' @return List with estimated coefficients for the event study.
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # calculate purchasing power per person
    
    purchpower_municipality <- microm_data_cleaned |>
        dplyr::group_by(AGS) |>
        dplyr::summarise(
            total_purch_power = sum(purch_power, na.rm = TRUE),
            total_people = sum(people_total, na.rm = TRUE),
            purch_power_pp = total_purch_power / total_people
        ) |>
        as.data.frame()

    #----------------------------------------------
    # add purchasing power groups

    add_cats <- function(gd, des_name = c("deciles_pp_munic")) {
        # purchasing power quantiles
        quant_pp <- as.numeric(
            quantile(
                gd[["purch_power_pp"]],
                prob = seq(0, 1, 0.1),
                na.rm = TRUE
            )
        )

        # export quantiles
        filename <- paste0(des_name, ".txt")
        write.table(
            quant_pp,
            file.path(
                config_paths()[["output_path"]],
                "descriptives",
                filename
            ),
            col.names = FALSE
        )

        # add purchasing power categories
        gd <- gd |>
            dplyr::mutate(
                purch_power_pp_cat = dplyr::case_when(
                    purch_power_pp >= quant_pp[1] & purch_power_pp < quant_pp[2] ~ "1",
                    purch_power_pp >= quant_pp[2] & purch_power_pp < quant_pp[3] ~ "2",
                    purch_power_pp >= quant_pp[3] & purch_power_pp < quant_pp[4] ~ "3",
                    purch_power_pp >= quant_pp[4] & purch_power_pp < quant_pp[5] ~ "4",
                    purch_power_pp >= quant_pp[5] & purch_power_pp < quant_pp[6] ~ "5",
                    purch_power_pp >= quant_pp[6] & purch_power_pp < quant_pp[7] ~ "6",
                    purch_power_pp >= quant_pp[7] & purch_power_pp < quant_pp[8] ~ "7",
                    purch_power_pp >= quant_pp[8] & purch_power_pp < quant_pp[9] ~ "8",
                    purch_power_pp >= quant_pp[9] & purch_power_pp < quant_pp[10] ~ "9",
                    purch_power_pp >= quant_pp[10] & purch_power_pp <= quant_pp[11] ~ "10"
                )
            )

        # set as factor
        lev <- paste0(seq(1, 10, 1))
        gd[["purch_power_pp_cat"]] <- factor(
            gd[["purch_power_pp_cat"]],
            levels = lev
        )

        # return
        return(gd)
    }

    purchpower_municipality <- add_cats(
        gd = purchpower_municipality,
        des_name = "deciles_pp_munic"
    )
    
    #----------------------------------------------
    # merge join stations and geo info
    # add also purchasing power

    merge_stat_pp <- function() {
        cln <- c(
            "station_id", "AGS", "total_purch_power_munic",
            "total_people_munic", "purch_power_pp_munic",
            "purch_power_pp_cat_munic"
        )    
        
        german_stations_pp <- merge(
            german_stations |>
                sf::st_drop_geometry(),
            purchpower_municipality,
            by = "AGS",
            all.x = TRUE
        )

        # clean
        german_stations_pp <- german_stations_pp |>
            dplyr::select(
                station_id, AGS, total_purch_power,
                total_people, purch_power_pp, purch_power_pp_cat
            )

        # rename
        colnames(german_stations_pp) <- cln

        # return
        return(german_stations_pp)
    }

    german_stations_munic <- merge_stat_pp()

    # clean
    german_stations_munic <- german_stations_munic |>
        dplyr::select(-"AGS")

    #----------------------------------------------
    # merge to station prices

    avg_prices_pp <- merge(
        fuel_prices_april_august,
        german_stations_munic,
        by = "station_id",
        all.x = TRUE
    )

    # add new level to factor category (for France)
    avg_prices_pp <- avg_prices_pp |>
        dplyr::mutate(
            purch_power_pp_cat_munic = factor(
                purch_power_pp_cat_munic,
                levels = c(levels(purch_power_pp_cat_munic), "0")
            )
        )
    
    # replace NA for France
    # set purchasing power category to "0"
    avg_prices_pp$purch_power_pp_cat_munic[avg_prices_pp$country == "FR"] <- "0"
    avg_prices_pp$purch_power_pp_cat_district[avg_prices_pp$country == "FR"] <- "0"

    #----------------------------------------------
    # event study analysis

    # add time to treatment in days
    avg_prices_event <- avg_prices_pp |>
        mutate(
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
        var_aux <- rlang::sym("purch_power_pp_cat_munic")
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
            list_name <- paste(var, "pp_cat", cat, sep = "_")
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
                        "purch_power",
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
                as.data.frame(cbind(time = -1, coefficient = 0, lower = 0, upper = 0))
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
        } else {
            # if group not present, return NA
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