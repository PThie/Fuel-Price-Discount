estimating_french_ftd_event_study <- function(
    price_data = NA
) {
    #' @title Estimating an event study for the baseline in France
    #' 
    #' @description This function estimates an event study for the baseline
    #' comparing the day effects to the last day before implementing the FTD in
    #' France.
    #' 
    #' @param price_data Data frame with the prepared fuel price data
    #' 
    #' @return List with estimation results
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # define dates as globals

    start_tr_fr <- as.Date("2022-04-01", format = "%Y-%m-%d")

    #----------------------------------------------
    # event study analysis

    # add time to treatment in days
    avg_prices_event <- price_data |>
        dplyr::mutate(
            time_to_treatment = as.numeric(difftime(
                as.Date(date, "%Y-%m-%d"), 
                start_tr_fr,
                units = "days"
            )),
            # set French stations to never-treated (in terms of time)
            time_to_treatment = dplyr::case_when(
                treat_region_fr == "control" ~ 999,
                TRUE ~ time_to_treatment
            )
        )

    # estimation
    mod_list_event <- list()

    # define different gasoline types
    dep_cases <- c("diesel", "e10")

    # loop through gasoline types
    suppressMessages(for(dep_case in dep_cases){
        mod <- est_fun_french_ftd(
            moddata = avg_prices_event,
            depvar = dep_case,
            twoway_clustering = FALSE,
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
                paste0(
                    "did_est_France_",
                    result,
                    ".tex"
                )
            ),
            digits = "r3", cluster = "station_id",
            dict = config_globals()[["coefnames_french"]],
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
                    time >= -59 & time <= -32 ~ 2,
                    time >= -31 & time <= -1 ~ 3,
                    time >= 0 & time <= 29 ~ 4,
                    time >= 30 & time <= 60 ~ 5
                )
            )

        # export table
        filename <- paste0(
            "did_est_France_",
            result,
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
                    x = factor(time, levels = seq(-59, 60, 1)),
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
                    x = factor(time, levels = seq(-59, 60, 1)),
                    y = coefficient, ymin = lower, ymax = upper
                ),
                linewidth = 1,
                size = 0.5,
            )+
            geom_hline(
                yintercept = -0.18,
                linewidth = 0.6,
                linetype = "dashed"
            )+
            geom_text(
                x = as.factor(-40),
                y = -0.165,
                label = "French FTD",
                size = 6
            )+
            scale_x_discrete(
                breaks = seq(-59, 60, 29)
            )+
            scale_y_continuous(
                breaks = round(seq(-0.3, 0.2, 0.1), digits = 2)
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
            coord_cartesian(xlim = c(0, 130))

        # export plot
        ggsave(
            plot = time_plot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                paste0(
                    result,
                    "_baseline_France",
                    ".png"
                )
            ),
            dpi = 800,
            width = 8,
            height = 5
        )
    }

    #--------------------------------------------------
    # return

    return(mod_list_event)
}