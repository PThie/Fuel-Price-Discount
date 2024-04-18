making_station_density <- function(
    fuel_prices_april_august = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    german_districts = NA
) {
    #' @title Heterogeneity based on car density
    #' 
    #' @desciption This function determines the regional car density and then
    #' estimates the price effect of the German tax discount.
    #' 
    #' @param fuel_prices_april_august Fuel prices from April to August 2022
    #' @param german_stations German stations
    #' @param german_districts German districts
    #' @param microm_data_cleaned Cleaned microm data (RWI-GEO-GRID data)
    #' 
    #' @return NULL, estimation outputs
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
            car_density_adj = count_stations / median_car_density 
        )

    # get quantiles
    quant_cd <- as.numeric(
        quantile(
            cardensity_stations_clean$car_density_adj,
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
            "deciles_car_density.txt"
        )
    )

    # add categories
    cardensity_stations_clean <- cardensity_stations_clean |>
        dplyr::mutate(
            car_density_cat = dplyr::case_when(
                car_density_adj >= 0 & car_density_adj < quant_cd[2] ~ "1",
                car_density_adj >= quant_cd[2] & car_density_adj < quant_cd[3] ~ "2",
                car_density_adj >= quant_cd[3] & car_density_adj < quant_cd[4] ~ "3",
                car_density_adj >= quant_cd[4] & car_density_adj < quant_cd[5] ~ "4",
                car_density_adj >= quant_cd[5] & car_density_adj < quant_cd[6] ~ "5",
                car_density_adj >= quant_cd[6] & car_density_adj < quant_cd[7] ~ "6",
                car_density_adj >= quant_cd[7] & car_density_adj < quant_cd[8] ~ "7",
                car_density_adj >= quant_cd[8] & car_density_adj < quant_cd[9] ~ "8",
                car_density_adj >= quant_cd[9] & car_density_adj < quant_cd[10] ~ "9",
                car_density_adj >= quant_cd[10] & car_density_adj <= quant_cd[11] ~ "10"
            ),
            car_density_cat = factor(
                car_density_cat,
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
            aes(geometry = geometry, fill = car_density_cat),
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
            car_density_cat = dplyr::case_when(
                country == "FR" ~ "0",
                TRUE ~ car_density_cat
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
        var_aux <- rlang::sym("car_density_cat")
        moddata_short <- moddata |>
            dplyr::filter(!!var_aux == "0" | !!var_aux == pp_cat)

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

        # define model run
        mod <- paste(depvar, pp_cat, sep = "_")

        # extract estimate
        interaction <- "relevel(as.factor(treat_tankrabatt_de), \"control\")treated:relevel(as.factor(treat_region_de), \"control\")treated"
        est <- as.numeric(
            est_mod$coefficients[interaction]
        )

        # calculate confidence intervall
        con <- confint(est_mod, level = 0.95)[interaction, ]

        # put components together
        results_table <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(results_table) <- c("mod_name", "estimate", "lower_ci", "upper_ci")
        results_table <- results_table |>
            dplyr::mutate(
                mod_name = mod,
                estimate = est,
                lower_ci = con[1, 1],
                upper_ci = con[1, 2]
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
                moddata = avg_prices_cd_sep,
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
            "hetero_car_density.xlsx"
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

    #----------------------------------------------
    # plot results

    hetero_plot <- ggplot(
        data = est_results_munic_df,
        mapping = aes(
            x = factor(cd_cat),
            y = estimate,
            group = depvar_id,
            shape = depvar_id
        )
    )+
    geom_pointrange(
        mapping = aes(ymin = lower_ci, ymax = upper_ci),
        linewidth = 1,
        size = 0.5
    )+
    scale_shape_manual(
        values = c(
            "diesel" = 17, 
            "e10" = 15
        ),
        name = "Fuel type", 
        labels = c(
            "diesel" = "Diesel",
            "e10" = "Petrol (E10)"
        )
    )+
    scale_y_continuous(
        breaks = seq(-0.14, -0.36, -0.02)
    )+
    geom_hline(
        yintercept = -0.17,
        linewidth = 0.8,
        linetype = "dashed"
    )+
    geom_text(
        x = 2,
        y = -0.18,
        label = "Diesel FTD",
        size = 6
    )+
    geom_hline(
        yintercept = -0.35,
        linewidth = 0.8,
        linetype = "dashed"
    )+
    geom_text(
        x = 2,
        y = -0.34,
        label = "Petrol FTD",
        size = 6
    )+
    labs(
        x = "Station density category",
        y = "Point estimates and 95% CI"
    )+
    theme_classic()+
    theme(
        legend.position = "bottom",
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)
    )

    # export
    suppressMessages(ggsave(
        plot = hetero_plot,
        file.path(
            config_paths()[["output_path"]],
            "graphs",
            "hetero_results_station_density.png"
        ),
        dpi = config_globals()[["owndpi"]]
    ))

    #----------------------------------------------
    # event study analysis

    # add time to treatment in days
    avg_prices_event <- avg_prices_cd_sep |>
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
        var_aux <- rlang::sym("car_density_cat")
        moddata_short <- moddata |>
            dplyr::filter(!!var_aux == "0" | !!var_aux == pp_cat)

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

        return(est_mod)
    }

    # define categories
    cats <- c("1", "10")
    
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
    get_coefficients <- function(result) {
        est_data <- mod_list_event[[result]]

        # export raw results
        fixest::esttex(
            est_data,
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0("car_density_", result, ".tex")
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

        # export table
        filename <- paste0("car_density_", result, ".xlsx")
        openxlsx::write.xlsx(
            final_prep,
            file.path(
                config_paths()[["output_path"]],
                "estimation",
                filename
            ),
            rowNames = FALSE
        )


        # return
        return(final_prep)
    }

    # store estimated coefficients
    coef_event <- list()
    for(nam in names(mod_list_event)) {
        coef_event[[nam]] <- get_coefficients(result = nam)
    }

    # plotting function
    plot_estimates <- function(gastype) {
        #' @param gastype Fuel type (Diesel or E10)
        #----------------------------------------------
        # select data
        coefficient_data <- coef_event[
            stringr::str_detect(names(coef_event), gastype) == TRUE
        ]

        low_density_data <- data.table::rbindlist(
            coefficient_data[
                stringr::str_detect(names(coefficient_data), "_10") == FALSE
            ]
        )

        high_density_data <- data.table::rbindlist(
            coefficient_data[
                stringr::str_detect(names(coefficient_data), "_10") == TRUE
            ]
        )

        #----------------------------------------------
        # generate base plot

        baseplot <- ggplot()+
            geom_vline(
                xintercept = as.factor(-1),
                linewidth = 0.6,
                linetype = "solid",
                col = "grey80"
            )+
            geom_hline(
                yintercept = 0,
                linewidth = 0.6,
                linetype = "solid",
                col = "grey80"
            )+
            scale_x_discrete(
                breaks = seq(-60, 90, 30)
            )+
            scale_y_continuous(
                breaks = round(seq(-0.3, 0.1, 0.1), digits = 2)
            )+
            labs(
                y = "Point estimates and 95% CI",
                x = "Time to treatment (days)"
            )+
            theme_classic()+
            theme(
                legend.position = "bottom",
                panel.border = element_rect(linewidth = 1, fill = NA),
                axis.text = element_text(size = 21),
                axis.title = element_text(size = 23),
                legend.key.size = unit(1.3, "cm"),
                legend.title = element_text(size = 22),
                legend.text = element_text(size = 22)
            )+
            # extend plotting space
            coord_cartesian(xlim = c(0, 154))

        #----------------------------------------------
        # add coefficient estimates to plot

        rangeplot <- baseplot+
            geom_pointrange(
                data = high_density_data,
                mapping = aes(
                    x = factor(time, levels = seq(-61, 91, 1)),
                    y = coefficient, ymin = lower, ymax = upper,
                    col = "high", shape = "high"
                ),
                linewidth = 1,
                size = 0.5
            )+
            geom_pointrange(
                data = low_density_data,
                mapping = aes(
                    x = factor(time, levels = seq(-61, 91, 1)),
                    y = coefficient, ymin = lower, ymax = upper,
                    col = "low", shape = "low"
                ),
                linewidth = 1,
                size = 0.5
            )+
            scale_color_manual(
                values = c(
                    "low" = config_globals()[["java_five_colors"]][1],
                    "high" = config_globals()[["java_five_colors"]][3]
                ),
                labels = c(
                    "low" = "Low",
                    "high" = "High"
                ),
                name = "Station density\nlevel"
            )+
            scale_shape_manual(
                values = c(
                    "low" = 17,
                    "high" = 15
                ),
                labels = c(
                    "low" = "Low",
                    "high" = "High"
                ),
                name = "Station density\nlevel"
            )

        #----------------------------------------------
        # add horizontal line for FTD
        if (gastype == "diesel") {
            coefplot <- rangeplot+
                geom_hline(
                    yintercept = -0.1671,
                    linewidth = 0.6,
                    linetype = "dashed"
                )+
                geom_text(
                    mapping = aes(
                        x = as.factor(-40),
                        y = -0.1571,
                        label = "Fuel tax discount"
                    ),
                    size = 7.5
                )
        } else {
            coefplot <- rangeplot+
                geom_hline(
                    yintercept = -0.3516,
                    linewidth = 0.6,
                    linetype = "dashed"
                )+
                geom_text(
                    mapping = aes(
                        x = as.factor(-40),
                        y = -0.3416,
                        label = "Fuel tax discount"
                    ),
                    size = 7.5
                )
        }

        # export
        filename <- paste0("station_density_", gastype, "_high_low.png")
        suppressMessages(ggsave(
            plot = coefplot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                filename
            ),
            dpi = config_globals()[["owndpi"]],
            width = 10,
            height = 10
        ))
    }

    # loop through options and plot
    for(gs in vars) {
        plot_estimates(
            gastype = gs
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}