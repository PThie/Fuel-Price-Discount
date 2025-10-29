plotting_robust_trends_dayspecific <- function(
    honest_did_days = NA
) {
    #' @title Plotting HonestDiD Results for specific days
    #' 
    #' @description This function plots the HonestDiD results for specific days.
    #' 
    #' @param honest_did_days HonestDiD results
    #'
    #' @return NULL, graphs 
    #' @author Patrick Thiel

    #----------------------------------------------
    # plot for one-week trend

    # define fuel types
    dep_cases <- c("diesel", "e10")

    # loop through honestDiD results
    for (dta in honest_did_days) {
        # loop through fuel types
        for(dep_case in dep_cases) {
            # subset data
            moddata <- dta |>
                dplyr::filter(fuel_type == dep_case)

            period_label <- unique(moddata$period_label)

            # generate plot
            vio_plot <- ggplot(
                data = moddata,
                aes(
                    x = mbar_labels,
                    y = ub
                ),
                col = "black"
            )+
                geom_errorbar(
                    mapping = aes(
                        ymin = lb,
                        ymax = ub
                    ),
                    width = 0.2,
                    linewidth = 1.3
                )+
                geom_hline(
                    yintercept = 0
                )+
                scale_y_continuous(
                    limits = c(-0.4, 0.05),
                    breaks = seq(-0.4, 0, 0.1)
                )+
                labs(
                    x = "M",
                    y = "95% CI"
                )+
                theme_classic()+
                theme(
                    panel.border = element_rect(linewidth = 1, fill = NA),
                    axis.text = element_text(size = 19),
                    axis.title = element_text(size = 20),
                    legend.key.size = unit(1, "cm"),
                    legend.title = element_text(size = 20),
                    legend.text = element_text(size = 20),
                    legend.position = "bottom"
                )+
                guides(col = guide_legend(nrow = 4, byrow = TRUE))
            
            # export
            filename <- paste0(
                "honestdid_plot_days_",
                dep_case,
                "_",
                period_label,
                ".png"
            )
            suppressMessages(ggsave(
                plot = vio_plot,
                file.path(
                    config_paths()[["output_path"]],
                    "graphs",
                    filename
                ),
                height = 8,
                width = 10
            ))
        }
    }

    #--------------------------------------------------
    # return
    
    return(NULL)
}