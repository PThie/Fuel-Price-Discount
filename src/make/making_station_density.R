making_station_density <- function(
    fuel_prices_april_august = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    german_municipalities = NA,
    german_districts = NA,
    suffix_export = NA
) {
    #' @title Heterogeneity based on car density
    #' 
    #' @desciption This function determines the regional car density and then
    #' estimates the price effect of the German tax discount.
    #' 
    #' @param fuel_prices_april_august Fuel prices from April to August 2022
    #' @param german_stations German stations
    #' @param microm_data_cleaned Cleaned microm data (RWI-GEO-GRID data)
    #' @param german_municipalities German municipalities
    #' @param german_districts German districts
    #' @param suffix_export Suffix for export files
    #' 
    #' @return Dataframe with estimation outputs
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

    # export quantiles
    write.table(
        quant_cd,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "deciles_station_density.txt"
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
    # mapping car density

    # define labels for categories
    label_cats <- c(
        "1" = paste0("1 - ", "0", " \u2264 density \u003C ", round(quant_cd[2], 1)),
        "2" = paste0("2 - ", round(quant_cd[2], 1), " \u2264 density \u003C ", round(quant_cd[3], 1)),
        "3" = paste0("3 - ", round(quant_cd[3], 1), " \u2264 density \u003C ", round(quant_cd[4], 1)),
        "4" = paste0("4 - ", round(quant_cd[4], 1), " \u2264 density \u003C ", round(quant_cd[5], 1)),
        "5" = paste0("5 - ", round(quant_cd[5], 1), " \u2264 density \u003C ", round(quant_cd[6], 1)),
        "6" = paste0("6 - ", round(quant_cd[6], 1), " \u2264 density \u003C ", round(quant_cd[7], 1)),
        "7" = paste0("7 - ", round(quant_cd[7], 1), " \u2264 density \u003C ", round(quant_cd[8], 1)),
        "8" = paste0("8 - ", round(quant_cd[8], 1), " \u2264 density \u003C ", round(quant_cd[9], 1)),
        "9" = paste0("9 - ", round(quant_cd[9], 1), " \u2264 density \u003C ", round(quant_cd[10], 1)),
        "10" = paste0("10 - ", round(quant_cd[10], 1), " \u2264 density \u2264 ", round(quant_cd[11], 1))
    )

    # add geometry back
    cardensity_stations_clean_sf <- merge(
        cardensity_stations_clean,
        german_municipalities,
        by = "AGS",
        all.x = TRUE
    )

    # set geometry
    cardensity_stations_clean_sf <- sf::st_set_geometry(
        cardensity_stations_clean_sf,
        cardensity_stations_clean_sf$geometry
    )

    # map
    map_cd_cat <- ggplot()+
        geom_sf(
            data = cardensity_stations_clean_sf,
            aes(geometry = geometry, fill = station_density_cat),
            col = ggplot2::alpha("black", 0.1)
        )+
        scale_fill_viridis_d(
            option = "magma",
            direction = -1,
            name = expression("Relative station\ndensity category"),
            labels = label_cats
        )+
        theme_void()+
        theme(
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12)
        )

    suppressMessages(ggsave(
        plot = map_cd_cat,
        file.path(
            config_paths()[["output_path"]],
            "maps",
            "station_density_cat.png"
        ),
        dpi = config_globals()[["owndpi"]]
    ))

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
    # estimation
    
    # define purchasing power categories
    # define dependent variables
    cats <- paste0(seq(1, 10, 1))
    vars <- c("diesel", "e10")

    # list for storage
    est_results_munic <- list()

    # estimation function
    est_fun <- function(moddata, depvar = c("diesel", "e10"), pp_cat) {
        var_aux <- rlang::sym("station_density_cat")
        moddata_short <- moddata |>
            dplyr::filter(!!var_aux == "0" | !!var_aux == pp_cat)

        # NOTE: only run model if category is present
        if (as.character(pp_cat) %in% unique(moddata[[var_aux]])) {
            # define formula
            fm <- formula(
                paste(
                    depvar, "~",
                    paste("relevel(as.factor(treat_tankrabatt_de), \"control\") * relevel(as.factor(treat_region_de), \"control\")"),
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
            list_name <- paste(var, "cd_cat", cat, sep = "_")
            # loop through options and store output
            est_results_munic[[list_name]] <- est_fun(
                moddata = avg_prices_cd,
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
                "hetero_station_density_",
                suffix_export,
                ".xlsx"
            )
        )
    )

    #----------------------------------------------
    # add categories

    # add dependent variable identifier
    # add car density group
    est_results_munic_df <- est_results_munic_df |>
        dplyr::mutate(
            depvar_id = dplyr::case_when(
                stringr::str_detect(mod_name, "diesel") ~ "diesel",
                stringr::str_detect(mod_name, "e10") ~ "e10"
            ),
            cd_cat = factor(
                stringr::str_split_fixed(mod_name, pattern = "_", n = 2)[, 2],
                levels = seq(1, 10, 1)
            )
        )

    #--------------------------------------------------
    # return

    return(est_results_munic_df)
}