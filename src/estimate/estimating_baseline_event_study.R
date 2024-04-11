estimating_baseline_event_study <- function(fuel_prices_april_august = NA) {
    #' @title Estimating an event study for the baseline
    #' 
    #' @description This function estimates an event study for the baseline
    #' comparing the day effects to the last day before implementing the FTD.
    #' 
    #' @param fuel_prices_april_august Fuel price data April to August 2022
    #' 
    #' @return Returns estimation results
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # event study analysis

    # add time to treatment in days
    avg_prices_event <- fuel_prices_april_august |>
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

    # estimation
    mod_list_event <- list()

    # define different gasoline types
    dep_cases <- c("diesel", "e10")

    # loop through gasoline types
    suppressMessages(for(dep_case in dep_cases){
        mod <- est_fun(
            moddata = avg_prices_event,
            depvar = dep_case,
            fixef = "time_region",
            event = TRUE
        )
        mod_list_event[[dep_case]] <- mod
    })

    # loop through estimation results and generate plot
    for (result in names(mod_list_event)) {
        # subset for fuel type
        est_data <- mod_list_event[[result]]

        # export raw results
        fixest::esttex(
            est_data,
            file = file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0("did_est_Germany_", result, ".tex")
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
                cbind(time = -1, coefficient = 0, lower = 0, upper = 0)
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
        filename <- paste0("did_est_Germany_", result, ".xlsx")
        openxlsx::write.xlsx(
            final_prep,
            file.path(
                config_paths()[["output_path"]],
                "estimation",
                filename
            ),
            rowNames = FALSE
        )

        # generate plot
        time_plot <- ggplot(data = final_prep)+
            geom_vline(
                xintercept = as.factor(-1),
                linewidth = 0.6,
                linetype = "solid",
                col = "grey80"
            )+
            geom_smooth(
                method = "lm",
                formula = y ~ x,
                se = FALSE,
                mapping = aes(
                    group = months,
                    x = factor(time, levels = seq(-61, 91, 1)),
                    y = coefficient
                ),
                col = "black"
            )+
            geom_hline(
                yintercept = 0,
                linewidth = 0.6,
                linetype = "solid",
                col = "grey80"
            )+
            geom_pointrange(
                mapping = aes(
                    x = factor(time, levels = seq(-61, 91, 1)),
                    y = coefficient, ymin = lower, ymax = upper
                ),
                linewidth = 1,
                size = 0.5,
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
                panel.border = element_rect(linewidth = 1, fill = NA),
                axis.text = element_text(size = 15),
                axis.title = element_text(size = 17),
                legend.key.size = unit(1, "cm"),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16)
            )+
            # extend plotting space
            coord_cartesian(xlim = c(0, 154))

        # add horizontal line for FTD
        if (result == "diesel") {
            time_plot <- time_plot+
                geom_hline(
                    yintercept = -0.1671,
                    linewidth = 0.6,
                    linetype = "dashed"
                )+
                geom_text(
                    x = as.factor(-40),
                    y = -0.155,
                    label = "Fuel tax discount",
                    size = 6
                )
        } else {
            time_plot <- time_plot+
                geom_hline(
                    yintercept = -0.3516,
                    linewidth = 0.6,
                    linetype = "dashed"
                )+
                geom_text(
                    x = as.factor(-40),
                    y = -0.335,
                    label = "Fuel tax discount",
                    size = 6
                )
        }

        # export plot
        ggsave(
            plot = time_plot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                paste0(result, "_baseline.png")
            ),
            dpi = 800,
            width = 8,
            height = 5
        )
    }
}