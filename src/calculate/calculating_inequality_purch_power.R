calculating_inequality_purch_power <- function(
    regional_effect_district = NA,
    microm_data_cleaned = NA,
    german_stations = NA
) {
    #' @title Calculate inequality in purchasing power
    #' 
    #' @description This function calculates the inequality in purchasing power
    #' by determining the deviation from the national mean. It also relates
    #' this deviation to the pass-through rate of petrol and diesel prices.
    #' 
    #' @param regional_effect_district Regional effects on the district level.
    #' @param microm_data_cleaned Clean microm data (RWI-GEO-GRID).
    #' @param german_stations German stations.
    #' 
    #' @return NULL, returns graphs.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # preparation regional effects

    # keep only relevant columns
    regional_effects_prep <- regional_effect_district |>
        select(ags_district, mod, coefficient, passthrough) |>
        tidyr::pivot_wider(
            names_from = mod,
            values_from = c(coefficient, passthrough)
        ) |>
        as.data.frame()

    #--------------------------------------------------
    # preparation grid data

    # add purchasing power per person
    griddata_district <- microm_data_cleaned |>
        dplyr::mutate(
            purch_power_pp = purch_power / people_total,
            # AGS district
            ags_district = substring(AGS, 1, 5)
        )

    #--------------------------------------------------
    # calculate 

    overall_mean_pp <- mean(griddata_district$purch_power_pp, na.rm = TRUE)

    inequality <- griddata_district |>
        dplyr::group_by(ags_district) |>
        dplyr::summarise(
            mean_pp = mean(purch_power_pp, na.rm = TRUE),
            std_pp = sd(purch_power_pp, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            # deviation from total mean in percent
            dev_mean_pp = ((mean_pp - overall_mean_pp) / overall_mean_pp) * 100
        )

    # merge regional effects to griddata
    inequality_regional_effects <- merge(
        inequality,
        regional_effects_prep,
        by = "ags_district",
        all.x = TRUE
    )

    # generate plot
    plot_function <- function(var) {
        # correlation between purchasing power and pass-through rate
        cor_pp_pass <- cor(
            inequality_regional_effects[[var]],
            inequality_regional_effects$dev_mean_pp
        )

        # file name
        if (var == "passthrough_diesel") {
            filename <- "inequality_purchasing_power_diesel.png"
        } else {
            filename <- "inequality_purchasing_power_petrol.png"
        }

        plot <- ggplot(
            data = inequality_regional_effects,
            aes(
                x = .data[[var]],
                y = dev_mean_pp
            )
        )+
            geom_point(size = 2)+
            # highlight the largest cities
            geom_point(
                data = inequality_regional_effects |>
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
                y = "Deviation from Average Purchasing Power (%)"
            )+
            # add text box for correlation
            annotate(
                "label",
                label = paste0("Correlation: ", round(cor_pp_pass, 2)),
                x = 65,
                y = -20,
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
                data = inequality_regional_effects |>
                    dplyr::filter(ags_district %in% c("11000", "02000", "09162")) |>
                    dplyr::mutate(
                        city_name = dplyr::case_when(
                            ags_district == "11000" ~ "Berlin",
                            ags_district == "02000" ~ "Hamburg",
                            ags_district == "09162" ~ "Munich"
                        )
                    ),
                aes(label = city_name),
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
            dpi = config_globals()[["owndpi"]],
            width = 13,
            height = 10
        ))
    }

    plot_function("passthrough_diesel")
    plot_function("passthrough_e10")

    #--------------------------------------------------
    # return

    return(NULL)
}