making_purch_power <- function(
    price_data = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    german_municipalities = NA,
    suffix_export = NA
) {
    #' @title Analysis purchasing power
    #' 
    #' @desciption This function analysis the passthrough given different
    #' purchasing power groups (i.e. income groups).
    #' 
    #' @param price_data Fuel price data for Germany and France
    #' @param german_stations German station data
    #' @param microm_data_cleaned Cleaned microm data
    #' @param german_municipalities German spatial municipality data
    #' @param suffix_export Suffix for export file names
    #' 
    #' @return Dataframe with estimation outputs
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
    # plot purchasing power
    
    # plot function
    plot_pp <- function(gd, sf_data) {
        # purchasing power quantiles
        quant_pp <- as.numeric(
            quantile(
                gd[["purch_power_pp"]],
                prob = seq(0, 1, 0.1),
                na.rm = TRUE
            )
        )

        # merge geo info
        gd_sf <- merge(
            gd,
            sf_data,
            by = "AGS",
            all.x = TRUE
        )

        # set geometry
        gd_sf <- gd_sf |>
            sf::st_set_geometry("geometry")

        # unique settings
        break_steps <- 5000
        fln <- "municipality"

        # define labels
        label_cats <- c(
            "1" = paste0("1 - ", round(quant_pp[1], 1), "\u20ac", " \u2264 Income \u003C ", round(quant_pp[2], 1), "\u20ac"),
            "2" = paste0("2 - ", round(quant_pp[2], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[3], 1), "\u20ac"),
            "3" = paste0("3 - ", round(quant_pp[3], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[4], 1), "\u20ac"),
            "4" = paste0("4 - ", round(quant_pp[4], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[5], 1), "\u20ac"),
            "5" = paste0("5 - ", round(quant_pp[5], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[6], 1), "\u20ac"),
            "6" = paste0("6 - ", round(quant_pp[6], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[7], 1), "\u20ac"),
            "7" = paste0("7 - ", round(quant_pp[7], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[8], 1), "\u20ac"),
            "8" = paste0("8 - ", round(quant_pp[8], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[9], 1), "\u20ac"),
            "9" = paste0("9 - ", round(quant_pp[9], 1), "\u20ac \u2264 Income \u003C ", round(quant_pp[10], 1), "\u20ac"),
            "10" = paste0("10 - ", round(quant_pp[10], 1), "\u20ac \u2264 Income \u2264 ", round(quant_pp[11], 1), "\u20ac")
        )

        # map levels
        map_pp <- ggplot()+
            geom_sf(
                data = gd_sf,
                mapping = aes(geometry = geometry, fill = purch_power_pp),
                col = ggplot2::alpha("black", 0.1)
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = expression("Purchasing power\nper person (in EUR)"),
                breaks = round(
                    seq(
                        min(gd_sf[["purch_power_pp"]], na.rm = TRUE),
                        max(gd_sf[["purch_power_pp"]], na.rm = TRUE),
                        by = break_steps
                    ), 
                    digits = -3
                ),
                labels = scales::comma
            )+
            theme_void()+
            theme(
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 12)
            )

        filename <- paste0("purch_power_", fln, ".png")
        suppressMessages(ggsave(
            plot = map_pp,
            file.path(
                config_paths()[["output_path"]],
                "maps",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        ))

        # map categories
        map_pp_cat <- ggplot()+
            geom_sf(
                data = gd_sf,
                mapping = aes(geometry = geometry, fill = purch_power_pp_cat),
                col = ggplot2::alpha("black", 0.1)
            )+
            scale_fill_viridis_d(
                option = "magma",
                direction = -1,
                name = expression("Purchasing power\ncategory (in EUR)"),
                labels = label_cats
            )+
            theme_void()+
            theme(
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 12)
            )

        filename <- paste0("purch_power_cat_", fln, ".png")
        suppressMessages(ggsave(
            plot = map_pp_cat,
            file.path(
                config_paths()[["output_path"]],
                "maps",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        ))
    }

    # apply function
    plot_pp(
        gd = purchpower_municipality,
        sf_data = german_municipalities
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
        price_data,
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
    # estimation

    # define purchasing power categories
    # define dependent variables
    cats <- paste0(seq(1, 10, 1))
    vars <- c("diesel", "e10")

    # list for storage
    est_results_munic <- list()

    # estimation function
    est_fun <- function(moddata, depvar = c("diesel", "e10"), pp_cat) {
        # subset for purchasing categories
        var_aux <- rlang::sym("purch_power_pp_cat_munic")
        moddata_short <- moddata |>
            dplyr::filter(!!var_aux == "0" | !!var_aux == pp_cat)

        # NOTE: only run model if category is present
        if (as.character(pp_cat) %in% unique(moddata[[var_aux]])) {
            # define formula
            fm <- formula(
                paste(
                    depvar, "~",
                    paste("+ relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
                    paste("| months + station_id")
                )
            )

            # estimate
            suppressMessages(est_mod <- fixest::feols(
                fml = fm,
                data = moddata_short,
                cluster = "station_id",
                notes = FALSE
            ))

            # extract estimate
            interaction <- "relevel(as.factor(treat_tankrabatt_de), \"control\")treated:relevel(as.factor(treat_region_de), \"control\")treated"
            est <- as.numeric(
                est_mod$coefficients[interaction]
            )

            # calculate confidence intervall
            con <- confint(est_mod, level = 0.95)[interaction, ]

            # define lower and upper confidence interval
            lowci <- con[1, 1]
            upci <- con[1, 2]
        } else {
            est <- NA
            lowci <- NA
            upci <- NA
        }

        # define model run
        mod <- paste(depvar, pp_cat, sep = "_")

        # put components together
        results_table <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(results_table) <- c("mod_name", "estimate", "lower_ci", "upper_ci")
        results_table <- results_table |>
            dplyr::mutate(
                mod_name = mod,
                estimate = est,
                lower_ci = lowci,
                upper_ci = upci
            )

        # return output
        return(results_table)
    }

    # loop through options
    for(var in vars) {
        for(cat in cats) {
            # list name
            list_name <- paste(var, "pp_cat", cat, sep = "_")
            # loop through options and store output
            est_results_munic[[list_name]] <- est_fun(
                moddata = avg_prices_pp,
                depvar = var,
                pp_cat = cat
            )
        }
    }

    # row bind the results
    est_results_munic_df <- data.table::rbindlist(est_results_munic)
    
    # export results
    openxlsx::write.xlsx(
        est_results_munic_df,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            paste0(
                "hetero_purch_power_munic_",
                suffix_export,
                ".xlsx"
            )
        )
    )

    #----------------------------------------------
    # add categories

    # add dependent variable identifier
    # add purchasing power group
    add_columns <- function(df) {
        # levels
        lev <- paste0(seq(1, 10, 1))
        
        df_updated <- df |>
            dplyr::mutate(
                depvar_id = dplyr::case_when(
                    stringr::str_detect(mod_name, "diesel") ~ "diesel",
                    stringr::str_detect(mod_name, "e10") ~ "e10"
                ),
                pp_cat = factor(
                    stringr::str_split_fixed(mod_name, pattern = "_", n = 2)[, 2],
                    levels = lev
                )
            )
        return(df_updated)
    }

    est_results_munic_df <- add_columns(est_results_munic_df)

    #--------------------------------------------------
    # return

    return(est_results_munic_df)
}