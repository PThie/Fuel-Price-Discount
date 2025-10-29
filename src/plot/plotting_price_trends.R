plotting_price_trends <- function(fuel_prices = NA) {
    #' @title Plotting price trends
    #' 
    #' @description This function plots the price development over time.
    #' 
    #' @param fuel_prices Data frame with fuel prices for Germany and France
    #' 
    #' @return Returns graphs
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # define dates as globals

    beginning_2022 <- as.Date("2022-01-01", format = "%Y-%m-%d")
    end_2022 <- as.Date("2022-12-31", format = "%Y-%m-%d")
    april_2022 <- as.Date("2022-04-01", format = "%Y-%m-%d")
    september_2022 <- as.Date("2022-09-01", format = "%Y-%m-%d")
    end_october_2022 <- as.Date("2022-10-31", format = "%Y-%m-%d")
    start_war <- as.Date("2022-02-24", "%Y-%m-%d")

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
    # difference in averages

    diff_data <- avg_fuel_prices |>
        dplyr::filter(
            date >= april_2022
                & date <= config_globals()[["end_tr_de"]]
        ) |>
        tidyr::pivot_wider(
            id_cols = date,
            names_from = country,
            values_from = c(diesel, e10)
        )  |>
        dplyr::mutate(
            diff_diesel = diesel_DE - diesel_FR,
            diff_e10 = e10_DE - e10_FR
        ) |>
        as.data.frame()

    # plot
    diff_plot <- ggplot(diff_data)+
        geom_rect(
            aes(
                xmin = config_globals()[["start_tr_de"]],
                xmax = config_globals()[["end_tr_de"]],
                ymin = -Inf,
                ymax = +Inf
            ),
            fill = "grey80",
            alpha = 0.4
        )+
        geom_line(
            mapping = aes(
                x = as.Date(date),
                y = diff_diesel,
                col = "diesel"
            ),
            linewidth = 1
        )+
        geom_line(
            mapping = aes(
                x = as.Date(date),
                y = diff_e10,
                col = "e10"
            ),
            linewidth = 1
        )+
        scale_color_manual(
            values = c(
                "diesel" = config_globals()[["java_five_colors"]][1],
                "e10" = config_globals()[["java_five_colors"]][3]
            ),
            labels = c(
                "diesel" = "Diesel",
                "e10" = "Petrol (E10)"
            ),
            name = ""
        )+
        scale_x_date(
            date_breaks = "1 month",
            date_minor_breaks = "1 week",
            date_labels = "%b"
        )+
        scale_y_continuous(
            breaks = seq(
                round(min(diff_data[["diff_e10"]], na.rm = TRUE), digits = 2),
                max(diff_data[["diff_e10"]], na.rm = TRUE),
                0.1
            )
        )+
        geom_text(
            x = as.Date("2022-07-30", format = "%Y-%m-%d"),
            y = as.numeric(
                quantile(
                    diff_data[["diff_e10"]],
                    prob = 0.05,
                    na.rm = TRUE
                )
            ),
            label = "Fuel tax discount",
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

    suppressMessages(ggsave(
        plot = diff_plot,
        file.path(
            config_paths()[["output_path"]],
            "graphs",
            "difference_in_prices.png"
        ),
        dpi = config_globals()[["owndpi"]]
    ))

    #--------------------------------------------------
    # plot the entire time frame since 2021

    # subset data to 2021 and later
    avg_prices_2021 <- avg_fuel_prices |>
        dplyr::filter(date >= as.Date("2021-09-01", format = "%Y-%m-%d")) |>
        dplyr::filter(date <= as.Date("2022-09-01", format = "%Y-%m-%d"))

    # generate basic plot
    overall_plot <- ggplot()+
        geom_rect(
            aes(
                xmin = config_globals()[["start_tr_de"]],
                xmax = config_globals()[["end_tr_de"]],
                ymin = -Inf,
                ymax = +Inf
            ),
            fill = "grey80",
            alpha = 0.4
        )+
        geom_line(
            data = avg_prices_2021 |>
                dplyr::filter(country == "DE"),
            aes(
                y = diesel,
                x = date,
                col = "diesel"
            ),
            linewidth = 1
        )+
        geom_line(
            data = avg_prices_2021 |>
                dplyr::filter(country == "DE"),
            aes(
                y = e10,
                x = date,
                col = "e10"
            ),
            linewidth = 1
        )+
        geom_vline(
            xintercept = start_war,
            linetype = "dashed",
            linewidth = 1
        )+
        geom_text(
            aes(
                x = start_war,
                label = "Ukraine invasion",
                y = 1.5
            ),
            angle = 90,
            vjust = -1,
            size = 5,
            fontface = "bold"
        )+
        geom_text(
            aes(
                x = config_globals()[["start_tr_de"]],
                label = "German FTD",
                y = 1.5
            ),
            angle = 90,
            vjust = -1,
            size = 5,
            fontface = "bold"
        )+
        scale_color_manual(
            values = c(
                "diesel" = config_globals()[["java_five_colors"]][1],
                "e10" = config_globals()[["java_five_colors"]][3]
            ),
            labels = c(
                "diesel" = "Diesel",
                "e10" = "Petrol (E10)"
            ),
            name = ""
        )+
        scale_x_date(
            date_breaks = "2 month",
            date_minor_breaks = "1 week",
            date_labels = "%Y-%b"
        )+
        labs(
            x = "",
            y = "Price in EUR/liter"
        )+
        theme_classic()+
        theme(
            legend.position = "bottom",
            axis.title.y = element_text(size = 15, vjust = 2),
            axis.text.y = element_text(size = 13),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            legend.text = element_text(size = 14),
        )

    suppressMessages(ggsave(
        plot = overall_plot,
        file.path(
            config_paths()[["output_path"]],
            "graphs",
            "overall_trends.png"
        ),
        dpi = config_globals()[["owndpi"]]
    ))

    #----------------------------------------------
    # plot the trend for different periods

    # plot function
    plotting_trends <- function(
        ds,
        gastype = c("diesel", "e10"),
        periodname
    ) {
        # generate plot
        plot <- ggplot(data = ds)+
            geom_rect(
                aes(
                    xmin = config_globals()[["start_tr_de"]],
                    xmax = config_globals()[["end_tr_de"]],
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
                # breaks = seq(
                #     round(min(ds[[gastype]], na.rm = TRUE), digits = 1),
                #     round(max(ds[[gastype]], na.rm = TRUE), digits = 1)+0.1,
                #     0.1
                # ),
                breaks = seq(1.4, 2.3, 0.1),
                limits = c(1.4, 2.4)
            )+
            geom_text(
                x = as.Date("2022-07-15", format = "%Y-%m-%d"),
                y = as.numeric(
                    quantile(
                        ds[[gastype]],
                        prob = 0.01,
                        na.rm = TRUE
                    )
                ),
                label = "Fuel tax discount",
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

    # define different data sets
    data_year_2022 <- avg_fuel_prices |>
        dplyr::filter(date >= beginning_2022 & date <= end_2022)
    
    data_april_sep  <- avg_fuel_prices |>
        dplyr::filter(
            date >= april_2022
                & date <= config_globals()[["end_tr_de"]]
        )

    data_april_oct <- avg_fuel_prices |>
        dplyr::filter(
            date >= april_2022
                & date <= end_october_2022
        )

    # pool data into list
    data_list <- list(
        data_year_2022,
        data_april_sep,
        data_april_oct
    )

    # define gas types
    gts <- c(
        "diesel",
        "e10"
    )

    # set counter and loop through data sets
    counter <- 0
    for(ds in data_list) {
        counter <- counter + 1
        # loop through gas types
        for(gt in gts) {
            if(counter == 1) {
                prdn <- "2022"
                fln <- paste(gt, "2022", "complete", sep = "_")
            } else if (counter == 2) {
                prdn <- "April to August 2022"
                fln <- paste(gt, "2022", "APR", "SEP", sep = "_")
            } else {
                prdn <- "April to October 2022"
                fln <- paste(gt, "2022", "APR", "OCT", sep = "_")
            }

            # plot graph
            price_plot <- plotting_trends(
                ds = ds,
                gastype = gt,
                periodname = prdn
            )

            # if observation period = April to August: calculate period means
            if(counter == 2) {

                # calculate period means
                mean_ds <- ds |>
                    dplyr::mutate(
                        treated = case_when(
                            date <= as.Date("2022-05-31", format = "%Y-%m-%d") ~ "control",
                            date >= config_globals()[["start_tr_de"]] & date <= config_globals()[["end_tr_de"]] ~ "treated"
                        )
                    ) |>
                    dplyr::group_by(country, treated) |>
                    dplyr::summarise(
                        mean = mean(.data[[gt]], na.rm = TRUE)
                    ) |>
                    as.data.frame()

                # export
                filename <- paste0("period_average_", gt, ".xlsx")
                openxlsx::write.xlsx(
                    mean_ds,
                    file.path(
                        config_paths()[["output_path"]],
                        "descriptives",
                        filename
                    ),
                    rowNames = FALSE
                )

                # add average lines to graph
                price_plot <- price_plot+
                    # Germany control period
                    geom_segment(
                        data = mean_ds,
                        y = mean_ds$mean[mean_ds$treated == "control" & mean_ds$country == "DE"],
                        yend = mean_ds$mean[mean_ds$treated == "control" & mean_ds$country == "DE"],
                        x = april_2022,
                        xend = as.Date("2022-05-31", format = "%Y-%m-%d"),
                        linewidth = 0.8,
                        linetype = "dashed",
                        col = config_globals()[["java_five_colors"]][3]
                    )+
                    # Germany treatment period
                    geom_segment(
                        data = mean_ds,
                        y = mean_ds$mean[mean_ds$treated == "treated" & mean_ds$country == "DE"],
                        yend = mean_ds$mean[mean_ds$treated == "treated" & mean_ds$country == "DE"],
                        x = config_globals()[["start_tr_de"]],
                        xend = config_globals()[["end_tr_de"]],
                        linewidth = 0.8,
                        linetype = "dashed",
                        col = config_globals()[["java_five_colors"]][3]
                    )+
                    # France control period
                    geom_segment(
                        data = mean_ds,
                        y = mean_ds$mean[mean_ds$treated == "control" & mean_ds$country == "FR"],
                        yend = mean_ds$mean[mean_ds$treated == "control" & mean_ds$country == "FR"],
                        x = april_2022,
                        xend = as.Date("2022-05-31", format = "%Y-%m-%d"),
                        linewidth = 0.8,
                        linetype = "dashed",
                        col = config_globals()[["java_five_colors"]][1]
                    )+
                    # France treatement period
                    geom_segment(
                        data = mean_ds,
                        y = mean_ds$mean[mean_ds$treated == "treated" & mean_ds$country == "FR"],
                        yend = mean_ds$mean[mean_ds$treated == "treated" & mean_ds$country == "FR"],
                        x = config_globals()[["start_tr_de"]],
                        xend = config_globals()[["end_tr_de"]],
                        linewidth = 0.8,
                        linetype = "dashed",
                        col = config_globals()[["java_five_colors"]][1]
                    )
            }

            # export graph
            filename <- paste0(fln, ".png")
            suppressMessages(ggsave(
                plot = price_plot,
                file.path(
                    config_paths()[["output_path"]],
                    "graphs",
                    filename
                ),
                dpi = config_globals()[["owndpi"]]
            ))
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}