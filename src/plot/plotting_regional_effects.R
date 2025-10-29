plotting_regional_effects <- function(
    regional_effects = NA,
    german_districts = NA,
    suffix_export = NA
) {
    #' @title Plot regional effects on district-level map
    #' 
    #' @description This function plots the estimated fuel price
    #' discount pass-through rates on a district-level map.
    #' 
    #' @param regional_effects Regional effects data frame
    #' @param german_districts German district information
    #' @param suffix_export Suffix for export files
    #' 
    #' @return NULL, direct map export
    #' @author Patrick Thiel

    #----------------------------------------------
    # map of effects

    # merge district info
    results_districts <- merge(
        regional_effects,
        german_districts,
        by.x = "ags_district",
        by.y = "AGS",
        all.x = TRUE
    )

    # set geometry
    results_districts <- sf::st_set_geometry(
        results_districts,
        results_districts$geometry
    )

    # loop through variables
    vars <- c("diesel", "e10")
    for(var in vars) {
        # define breaks
        if (var == "diesel") {
            br <- seq(50, 140, 10)
            lim <- c(50, 140)

            if (suffix_export == "twoweeks") {
                br <- seq(80, 150, 10)
                lim <- c(80, 150)
            }
        } else {
            br <- seq(40, 100, 10)
            lim <- c(40, 100)

            if (suffix_export == "twoweeks") {
                br <- seq(60, 110, 10)
                lim <- c(60, 110)
            }
        }
        # generate map
        map <- ggplot()+
            geom_sf(
                data = results_districts |>
                    filter(mod == var),
                aes(geometry = geometry, fill = passthrough)
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = "Pass-through rate\n(in %)",
                breaks = br,
                limits = lim
            )+
            theme_void()+
            theme(
                legend.position = "bottom",
                legend.title = element_text(size = 14, vjust = 0.8),
                legend.key.size = unit(0.8, "cm"),
                legend.text = element_text(size = 11, angle = 90, vjust = 0.5)
            )

        # export
        filename <- paste0(
            "regional_price_effect_",
            var,
            "_",
            suffix_export,
            ".png"
        )

        suppressMessages(ggsave(
            plot = map,
            file.path(
                config_paths()[["output_path"]],
                "maps",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        ))
    }

    #--------------------------------------------------
    # return
    
    return(NULL)
}