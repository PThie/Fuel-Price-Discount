plotting_french_ftd <- function(
    fuel_prices = NA
) {
    #' @title Plotting French FTD
    #' 
    #' @description This function plots the fuel price discount for France.
    #' 
    #' @param fuel_prices Fuel price data
    #' 
    #' @return NULL, direct export to local directory
    #' @author Patrick Thiel

    #----------------------------------------------
    # define dates as globals

    start_tr_fr <- as.Date("2022-04-01", format = "%Y-%m-%d")
    end_tr_fr <- as.Date("2022-05-31", format = "%Y-%m-%d")

    #----------------------------------------------
    # setting plotting language to English

    Sys.setlocale("LC_ALL", "English")

    #----------------------------------------------
    # calculate averages prices across dates and country

    avg_fuel_prices <- fuel_prices |>
        dplyr::group_by(date, country) |>
        dplyr::summarise(
            dplyr::across(
                .cols = all_of(c("diesel", "e10", "e5")),
                ~ mean(.x, na.rm = TRUE)
            )
        ) |>
        as.data.frame()

    #----------------------------------------------
    # plot function
    
    plotting_trends <- function(
        ds,
        gastype = c("diesel", "e10")
    ) {
        # generate plot
        plot <- ggplot(data = ds)+
            geom_rect(
                aes(
                    xmin = start_tr_fr,
                    xmax = end_tr_fr,
                    ymin = -Inf,
                    ymax = +Inf
                ),
                fill = "grey80",
                alpha = 0.4
            )+
            geom_line(
                aes(
                    x = date,
                    y = .data[[gastype]],
                    group = country,
                    col = country
                ),
                linewidth = 1
            )+
            scale_color_manual(
                name = "",
                values = c(
                    "DE" = config_globals()[["java_five_colors"]][3],
                    "FR" = config_globals()[["java_five_colors"]][1]
                ),
                labels = c(
                    "DE" = "Germany",
                    "FR" = "France"
                )
            )+
            scale_x_date(
                date_breaks = "1 month",
                date_minor_breaks = "1 week",
                date_labels = "%b"
            )+
            scale_y_continuous(
                breaks = seq(
                    round(min(ds[[gastype]], na.rm = TRUE), digits = 1),
                    round(max(ds[[gastype]], na.rm = TRUE), digits = 1)+0.1,
                    0.1
                )
            )+
            geom_text(
                x = as.Date("2022-05-01", format = "%Y-%m-%d"),
                y = as.numeric(
                    quantile(
                        ds[[gastype]],
                        prob = 0.01,
                        na.rm = TRUE
                    )
                ),
                label = "French FTD",
                size = 6
            )+
            labs(
                x = "",
                y = "Price in EUR/liter"
            )+
            theme_classic()+
            theme(
                legend.position = "bottom",
                axis.title.y = element_text(size = 18, vjust = 2),
                axis.text.y = element_text(size = 16),
                axis.text.x = element_text(size = 16),
                legend.text = element_text(size = 18),
                legend.key.size = unit(0.75, "cm")
            )

        return(plot)
    }

    #--------------------------------------------------
    # create plots

    for (fuel in c("diesel", "e10")) {
        price_plot <- plotting_trends(
            ds = avg_fuel_prices |>
                dplyr::filter(
                    date >= as.Date("2022-01-01", format = "%Y-%m-%d") &
                    date <= as.Date("2022-12-31", format = "%Y-%m-%d")
                ),
            gastype = fuel
        )

        ggsave(
            plot = price_plot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                paste0("french_ftd_", fuel, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}