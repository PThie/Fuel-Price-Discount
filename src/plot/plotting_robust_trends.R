plotting_robust_trends <- function(
    honest_did = NA
) {
    #' @title Plotting HonestDiD Results
    #' 
    #' @description This function plots the HonestDiD results.
    #' 
    #' @param honest_did HonestDiD results
    #'
    #' @return NULL, graphs 
    #' @author Patrick Thiel

    #----------------------------------------------
    # plot for one-week trend

    # define fuel types
    dep_cases <- c("diesel", "e10")

    # define colors
    pal <- MetBrewer::met.brewer(name = "Austria", n = 7)

    # loop through honestDiD results
    for (dta in honest_did) {
        start_date <- min(dta$period_label)
        end_date <- max(dta$period_label)

        # loop through fuel types
        for(dep_case in dep_cases) {
            # subset data
            moddata <- dta |>
                dplyr::filter(fuel_type == dep_case & Mbar != 0)

            period_label <- unique(moddata$period_label)

            # generate plot
            vio_plot <- ggplot(
                data = moddata,
                aes(
                    x = mbar_labels,
                    y = ub,
                    col = period
                )
            )+
                geom_errorbar(
                    mapping = aes(
                        ymin = lb,
                        ymax = ub
                    ),
                    width = 0.15,
                    position = "dodge",
                    linewidth = 1
                )+
                geom_hline(
                    yintercept = 0
                )+
                scale_color_manual(
                    name = "Days Post-FTD",
                    labels = period_label,
                    values = pal
                )+
                scale_y_continuous(
                    limits = c(-0.65, 0.35),
                    breaks = round(seq(-0.6, 0.3, 0.1), 1)
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
                "honestdid_plot_one_week_",
                dep_case,
                "_",
                start_date,
                "_",
                end_date,
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