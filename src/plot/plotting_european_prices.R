plotting_european_prices <- function(european_fuel_prices = NA) {
    #' @title Plotting European gas prices
    #' 
    #' @description This function plots the gasoline prices across Europe.
    #' 
    #' @param european_fuel_prices European fuel prices
    #' 
    #' @return Return graphs
    #' @author Patrick Thiel

    #----------------------------------------------
    # subset for states and important time frame

    states <- c(
        "EU", "Germany", "France", "Austria", "Poland", "Czech", "Belgium",
        "Netherlands"
    )

    subset_prices <- european_fuel_prices |>
        dplyr::filter(date >= "2022-04-01") |>
        dplyr::filter(date <= "2022-08-31") |>
        dplyr::filter(country %in% states) |>
        as.data.frame()
    
    # define gastypes
    gastypes <- c("gastax", "dieselnotax")
    
    # generate plot
    for (gastype in gastypes) {
        # define anchors for discount labels
        if (gastype == "gastax") {
            anchor <- 1.4
        } else {
            anchor <- 1.5
        }

        country_plot <- ggplot()+
            # plot discount
            geom_rect(
                aes(
                    xmin = as.Date("2022-06-01"),
                    xmax = as.Date("2022-08-31"),
                    ymin = -Inf,
                    ymax = +Inf
                ),
                fill = "grey80",
                alpha = 0.4
            )+
            # plot Germany
            geom_line(
                data = subset_prices |>
                    dplyr::filter(country == "Germany"),
                aes(
                    x = date,
                    y = .data[[gastype]],
                    group = country,
                    color = country
                ),
                linewidth = 1.5,
                linetype = "solid"
            )+
            # plot for France
            geom_line(
                data = subset_prices |>
                    dplyr::filter(country == "France"),
                aes(
                    x = date,
                    y = .data[[gastype]],
                    group = country,
                    color = country
                ),
                linewidth = 1.5,
                linetype = "dashed"
            )+
            geom_line(
                data = subset_prices |>
                    dplyr::filter(country != "Germany" & country != "France"),
                aes(
                    x = date,
                    y = .data[[gastype]],
                    group = country,
                    color = country,
                ),
                linewidth = 0.8,
                linetype = "dotted"
            )+
            # plot other countries
            geom_vline(
                aes(
                    xintercept = as.Date("2022-06-01", format = "%Y-%m-%d")
                ),
                linewidth = 0.6
            )+
            # removing legend title
            scale_color_discrete(
                name = "",
                labels = c(
                    "Austria" = "Austria",
                    "Belgium" = "Belgium",
                    "Czech" = "Czech Republic",
                    "EU" = "Euro area",
                    "France" = "France",
                    "Germany" = "Germany",
                    "Netherlands" = "Netherlands",
                    "Poland" = "Poland"
                )
            )+
            # add discount label
            geom_text(
                data = subset_prices,
                x = as.Date("2022-07-10", format = "%Y-%m-%d"),
                y = anchor,
                label = "Fuel tax discount",
                size = 6.5
            )+
            labs(
                y = "Price in EUR/liter",
                x = ""
            )+
            theme_classic()+
            theme(
                axis.text = element_text(size = 15),
                axis.title = element_text(size = 16),
                legend.key.size = unit(1.5, "cm"),
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 14),
                legend.position = "bottom",
                legend.key.height = unit(0.7, "cm")
            )+
            guides(color = guide_legend(nrow = 3, byrow = TRUE))

        # generate filename
        if (gastype == "gastax") {
            filename <- paste0("european_prices_petrol.png")
        } else {
            filename <- paste0("european_prices_diesel.png")
        }

        # export
        ggsave(
            plot = country_plot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        )
    }
}