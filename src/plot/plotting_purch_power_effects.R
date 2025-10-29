plotting_purch_power_effects <- function(
    effects = NA,
    suffix_export = NA
) {
    #' @title Plotting Purchasing Power Effects
    #' 
    #' @description This function plots the estimated effects by income category.
    #' 
    #' @param effects Data frame with estimated effects
    #' @param suffix_export Suffix for export files
    #' 
    #' @return NULL, direct figure export
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # plot results

    hetero_plot <- ggplot(
        data = effects,
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
            breaks = seq(-0.12, -0.36, -0.02),
            limits = c(-0.37, -0.11)
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
    filename <- paste0(
        "hetero_results_purch_power_", 
        fln,
        "_",
        suffix_export,
        ".png"
    )
    suppressMessages(ggsave(
        plot = hetero_plot,
        file.path(
            config_paths()[["output_path"]],
            "graphs",
            filename
        ),
        dpi = config_globals()[["owndpi"]]
    ))

    #--------------------------------------------------
    # return

    return(NULL)
}