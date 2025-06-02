plotting_purch_power_effects_event_study <- function(
    effects = NA,
    suffix_export = NA
) {
    #' @title Plotting Purchasing Power Effects Event Study
    #' 
    #' @description This function plots the estimated effects of the
    #' purchasing power categories for the event study.
    #' 
    #' @param results Data frame with estimated effects
    #' @param suffix_export Suffix for export files
    #' 
    #' @return NULL, direct figure export
    #' @author Patrick Thiel
    #
    #--------------------------------------------------
    # plotting function

    plot_estimates <- function(results_data, gastype, suffix_export) {
        #' @param results_data Data frame with coefficient estimates
        #' @param gastype Fuel type (Diesel or E10)
        #' @param suffix_export Suffix for export files
        
        #----------------------------------------------
        # select data
        coefficient_data <- results_data[
            stringr::str_detect(names(results_data), gastype) == TRUE
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
            )

        if (suffix_export == "twoweeks") {
            rangeplot <- baseplot+
                geom_pointrange(
                    data = high_density_data,
                    mapping = aes(
                        x = factor(time, levels = seq(-61, 13, 1)),
                        y = coefficient, ymin = lower, ymax = upper,
                        col = "high", shape = "high"
                    ),
                    linewidth = 1,
                    size = 0.5
                )+
                geom_pointrange(
                    data = low_density_data,
                    mapping = aes(
                        x = factor(time, levels = seq(-61, 13, 1)),
                        y = coefficient, ymin = lower, ymax = upper,
                        col = "low", shape = "low"
                    ),
                    linewidth = 1,
                    size = 0.5
                )+
                scale_x_discrete(
                    breaks = seq(-60, 15, 15)
                )+
                # extend plotting space
                coord_cartesian(xlim = c(0, 80))
        } else {
            # NOTE: implement for the states analysis the possibility that
            # not all purchasing power categories are available
            # if not available, the data just contains NA (nrow = 1)
            if (nrow(high_density_data) > 1 & nrow(low_density_data) > 1) {
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
                    )
            } else if (nrow(high_density_data) > 1 & nrow(low_density_data) == 1) {
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
                    )
            } else if (nrow(high_density_data) == 1 & nrow(low_density_data) > 1) {
                rangeplot <- baseplot+
                    geom_pointrange(
                        data = low_density_data,
                        mapping = aes(
                            x = factor(time, levels = seq(-61, 91, 1)),
                            y = coefficient, ymin = lower, ymax = upper,
                            col = "low", shape = "low"
                        ),
                        linewidth = 1,
                        size = 0.5
                    )
            } else {
                rangeplot <- baseplot
            }

            rangeplot <- rangeplot+
                scale_x_discrete(
                    breaks = seq(-60, 90, 30)
                )+
                # extend plotting space
                coord_cartesian(xlim = c(0, 154))
        }

        #----------------------------------------------
        # add colors to plot

        colplot <- rangeplot+
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
            coefplot <- colplot+
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
            coefplot <- colplot+
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
        filename <- paste0(
            "purch_power_",
            gastype,
            "_",
            suffix_export,
            "_high_low.png"
        )
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

    #--------------------------------------------------
    # loop through options and plot

    vars <- c("diesel", "e10")
    for(gs in vars) {
        plot_estimates(
            results_data = effects,
            gastype = gs,
            suffix_export = suffix_export
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}
