making_purch_power <- function(
    fuel_prices_april_august = NA,
    german_stations = NA,
    microm_data_cleaned = NA,
    german_municipalities = NA
) {
    #' @title Analysis purchasing power
    #' 
    #' @desciption This function analysis the passthrough given different
    #' purchasing power groups (i.e. income groups).
    #' 
    #' @param fuel_prices_april_august Fuel price data for Germany and France
    #' April to August 2022
    #' @param german_stations German station data
    #' @param microm_data_cleaned Cleaned microm data
    #' @param german_municipalities German spatial municipality data
    #' 
    #' @return NULL, estimation outputs
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
            "hetero_purch_power_munic.xlsx"
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
    
    #----------------------------------------------
    # plot results

    hetero_plot <- ggplot(
        data = est_results_munic_df,
        mapping = aes(
            x = factor(pp_cat),
            y = estimate,
            group = depvar_id,
            shape = depvar_id
        )
    )+
        geom_pointrange(
            mapping = aes(ymin = lower_ci, ymax = upper_ci),
            linewidth = 1, size = 0.5
        )+
        scale_y_continuous(
            breaks = seq(-0.14, -0.36, -0.02)
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
        geom_hline(
            yintercept = -0.17,
            linewidth = 0.8,
            linetype = "dashed"
        )+
        geom_text(
            x = 9,
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
            x = 9,
            y = -0.34,
            label = "Petrol FTD",
            size = 6
        )+
        labs(
            x = "Income category",
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
    fln <- "munic"
    filename <- paste0("hetero_results_purch_power_", fln, ".png")
    suppressMessages(ggsave(
        plot = hetero_plot,
        file.path(
            config_paths()[["output_path"]],
            "graphs",
            filename
        ),
        dpi = config_globals()[["owndpi"]]
    ))

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
    get_coefficients <- function(result) {
        est_data <- mod_list_event[[result]]

        # export raw results
        fixest::esttex(
            est_data,
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0("purch_power", result, ".tex")
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
                name = "Income \nlevel"
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
                name = "Income \nlevel"
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
        filename <- paste0("purch_power_", gastype, "_high_low.png")
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