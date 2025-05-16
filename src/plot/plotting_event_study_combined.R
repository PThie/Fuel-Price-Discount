plotting_event_study_combined <- function(
    result_list = NA,
    result_list_placebo = NA
) {
    #' @title Plotting Event Study Placebo (combined output)
    #' 
    #' @description This function generates the event study plots for the placebo
    #' tests and the original estimates in one figure.
    #' 
    #' @param result_list List with estimation results (original)
    #' @param result_list_placebo List with estimation results (placebo)
    #' 
    #' @return NULL, direct export
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    for (year in names(result_list_placebo)) {
        # subset placebo for year
        result_list_year_placebo <- result_list_placebo[[year]]

        for (fuel_type in names(result_list)) {
            #--------------------------------------------------
            # function for extracting coefficients

            extracting_coefficients <- function(est_data = NA) {
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

                #--------------------------------------------------
                # return

                return(final_prep)
            }

            # extract coefficients
            extracted_coef_placebo <- extracting_coefficients(result_list_year_placebo[[fuel_type]])
            extracted_coef <- extracting_coefficients(result_list[[fuel_type]])

            extracted_coef_combined <- rbind(
                extracted_coef |>
                    dplyr::mutate(
                        type = "original"
                    ),
                extracted_coef_placebo |> 
                    dplyr::mutate(
                        type = "placebo"
                    )
            )

            #--------------------------------------------------
            # generate plot

            time_plot <- ggplot(data = extracted_coef_combined)+
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
                        group = interaction(type, months),
                        x = factor(time, levels = seq(-61, 91, 1)),
                        y = coefficient,
                        col = type
                    )
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
                        y = coefficient,
                        ymin = lower,
                        ymax = upper,
                        col = type
                    ),
                    linewidth = 1,
                    size = 0.5,
                    alpha = 0.5
                )+
                scale_x_discrete(
                    breaks = seq(-60, 90, 30)
                )+
                scale_y_continuous(
                    breaks = round(seq(-0.3, 0.1, 0.1), digits = 1),
                    limits = c(-0.4, 0.1)
                )+
                scale_color_manual(
                    values = c(
                        "original" = config_globals()[["java_five_colors"]][1],
                        "placebo" = config_globals()[["java_five_colors"]][3]
                    ),
                    labels = c(
                        "original" = "Original",
                        "placebo" = "Placebo"
                    ),
                    name = ""
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
                    legend.text = element_text(size = 16),
                    legend.position = "bottom"
                )+
                # extend plotting space
                coord_cartesian(xlim = c(0, 154))

            # add horizontal line for FTD
            if (fuel_type == "diesel") {
                time_plot <- time_plot+
                    geom_hline(
                        yintercept = -0.1671,
                        linewidth = 0.6,
                        linetype = "dashed"
                    )+
                    geom_text(
                        x = as.factor(-35),
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
                        x = as.factor(-35),
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
                    paste0(
                        fuel_type,
                        "_baseline_",
                        year,
                        "_placebo_combined.png"
                    )
                ),
                dpi = config_globals()[["owndpi"]],
                width = 8,
                height = 5
            )
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}