calculating_inequality_station_density <- function(
    regional_effect_district = NA,
    microm_data_cleaned = NA,
    german_stations = NA
) {
    #' @title Calculate inequality in station density
    #'
    #' @description This function calculates the inequality in station density
    #' by determining the deviation from the national mean. It also relates
    #' this deviation to the pass-through rate of petrol and diesel prices.
    #' 
    #' @param regional_effect_district Regional effects on the district level.
    #' @param microm_data_cleaned Clean microm data (RWI-GEO-GRID).
    #' 
    #' @return NULL, returns graphs.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # preparation regional effects

    # keep only relevant columns
    regional_effects_prep <- regional_effect_district |>
        dplyr::select(ags_district, mod, coefficient, passthrough) |>
        tidyr::pivot_wider(
            names_from = mod,
            values_from = c(coefficient, passthrough)
        ) |>
        as.data.frame()
    
    #----------------------------------------------
    # summarise car density on municipality level
    # summarise stations on district level

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
        by = "AGS_district",
        all.x = TRUE
    )

    # clean car density data
    # adjust median municipality car density by number of stations per district
    cardensity_stations_clean <- cardensity_stations |>
        dplyr::mutate(
            car_density_adj = count_stations / median_car_density 
        ) 

    #--------------------------------------------------
    # keep only relevant columns
    # construct district measure

    # overall mean car density
    overall_mean_car_density <- mean(
        cardensity_stations_clean$car_density_adj,
        na.rm = TRUE
    )

    cardensity_district <- cardensity_stations_clean |>
        dplyr::select(AGS_munic = AGS, car_density_adj, AGS_district) |>
        # summarise car density on district level
        dplyr::group_by(AGS_district) |>
        dplyr::summarise(
            mean_car_density_adj = mean(car_density_adj, na.rm = TRUE),
            std_car_density_adj = sd(car_density_adj, na.rm = TRUE),
            mean_dev_car_density_adj = (
                (mean_car_density_adj - overall_mean_car_density) / overall_mean_car_density
            ) * 100
        )

    # merge regional effects
    cardensity_regional_effects <- merge(
        cardensity_district,
        regional_effects_prep,
        by.x = "AGS_district",
        by.y = "ags_district",
        all.x = TRUE
    )

    # generate plot
    plot_function <- function(var) {
        # correlation between car density and pass-through rate
        cor_pp_pass <- cor(
            cardensity_regional_effects[[var]],
            cardensity_regional_effects$mean_dev_car_density_adj
        )

        # file name
        if (var == "passthrough_diesel") {
            filename <- "inequality_cardensity_diesel.png"
        } else {
            filename <- "inequality_cardensity_petrol.png"
        }

        plot <- ggplot(
            data = cardensity_regional_effects,
            aes(
                x = .data[[var]],
                y = mean_dev_car_density_adj
            )
        )+
            geom_point(size = 2)+
            # highlight the largest cities
            geom_point(
                data = cardensity_regional_effects |>
                    dplyr::filter(ags_district %in% c("11000", "02000", "09162")),
                size = 2,
                color = "red"
            )+
            geom_smooth(
                method = "lm",
                formula = y ~ x,
                linewidth = 1.5
            )+
            labs(
                x = "Pass-through Rate (%)",
                y = "Deviation from Average Station Density (%)"
            )+
            # add text box for correlation
            annotate(
                "label",
                label = paste0("Correlation: ", round(cor_pp_pass, 2)),
                x = 65,
                y = -150,
                size = 7
            )+
            theme_light()+
            theme(
                axis.text = element_text(size = 20),
                axis.title = element_text(size = 22)
            )

        plot_labels <- plot+
            # add labels for the largest cities
            geom_label(
                data = cardensity_regional_effects |>
                    dplyr::filter(ags_district %in% c("11000", "02000", "09162")),
                aes(label = c(
                    "11000" = "Berlin",
                    "02000" = "Hamburg",
                    "09162" = "Munich"
                )),
                size = 6,
                vjust = -0.3,
                fill = "white"
            )

        suppressMessages(ggsave(
            plot = plot_labels,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        ))
    }

    plot_function("passthrough_diesel")
    plot_function("passthrough_e10")

    #--------------------------------------------------
    # return

    return(NULL)
}