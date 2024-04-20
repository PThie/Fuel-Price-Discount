plotting_fuel_consumption <- function() {
    #' @title Plot fuel consumption levels
    #' 
    #' @description This function plots the fuel consumption levels for petrol
    #' and diesel.
    #' 
    #' @note The data origin is: https://en2x.de/service/publikationen/ (
    #' Wirtschaftsverband Fuels und Energie e. V.) using the information on
    #' Mineralölzahlen 2022.
    #' 
    #' @return NULL, returns graphs.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # create data set

    create_dataset <- function(year, fuel_cons, fuel_type) {
        dta <- data.frame(
            cbind(
                fuel = rep(fuel_type, 12),
                year = rep(year, 12),
                month = seq(1, 12, 1),
                fuel_consumption = fuel_cons
            )
        )
        return(dta)
    }

    petrol_2021 <- create_dataset(
        year = 2021,
        fuel_cons = c(
            1032, 1011, 1369, 1264, 1314, 1477,
            1500, 1579, 1539, 1512, 1440, 1392
        ),
        fuel_type = "petrol"
    )

    petrol_2022 <- create_dataset(
        year = 2022,
        fuel_cons = c(
            1234, 1276, 1387, 1351, 1339, 1667,
            1531, 1677, 1280, 1411, 1415, 1348
        ),
        fuel_type = "petrol"
    )

    diesel_2021 <- create_dataset(
        year = 2021,
        fuel_cons = c(
            2199, 2188, 2938, 2878, 2795, 3086,
            3238, 3195, 3128, 3267, 3145, 2925
        ),
        fuel_type = "diesel"
    )

    diesel_2022 <- create_dataset(
        year = 2022,
        fuel_cons = c(
            2651, 2548, 3086, 2737, 2676, 3057,
            3028, 3314, 2763, 2837, 3057, 2891
        ),
        fuel_type = "diesel"
    )

    #----------------------------------------------
    # combine all data sets

    fuel_consumption <- rbind(
        petrol_2021, petrol_2022,
        diesel_2021, diesel_2022
    )

    # set as numeric
    fuel_consumption <- fuel_consumption |>
        dplyr::mutate(
            dplyr::across(
                .cols = -fuel,
                ~ as.numeric(.x)
            )
        )

    # convert fuel consumption from thousands tons to liters
    fuel_consumption <- fuel_consumption |>
        dplyr::mutate(
            # convert into tons
            fuel_consumption = fuel_consumption * 1000,
            # convert into kiloliters
            # conversion factors taken from:
            # https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/pdfs/energy-economics/statistical-review/bp-stats-review-2022-approximate-conversion-factors.pdf
            fuel_consumption = dplyr::case_when(
                fuel == "petrol" ~ fuel_consumption * 1.328,
                fuel == "diesel" ~ fuel_consumption * 1.186
            ),
            # convert to liter
            fuel_consumption = fuel_consumption * 1000
        )

    # calculate difference to previous year month
    fuel_consumption <- fuel_consumption |>
        dplyr::group_by(fuel) |>
        dplyr::mutate(
            diff_to_prev_year = dplyr::case_when(
                year == 2022 ~ fuel_consumption - dplyr::lag(fuel_consumption, 12),
                TRUE ~ NA_real_
            )
        )

    # calculate average before and after FTD
    avg_fuel_consumption <- fuel_consumption |>
        dplyr::filter(year == 2022) |>
        dplyr::group_by(fuel) |>
        dplyr::summarise(
            avg_before_FTD = mean(fuel_consumption[month < 6]),
            avg_during_FTD = mean(fuel_consumption[month >= 6 & month <= 8]),
            avg_after_FTD = mean(fuel_consumption[month > 8])
        )

    # export
    openxlsx::write.xlsx(
        avg_fuel_consumption,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "avg_fuel_consumption.xlsx"
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # calculate percentage change in fuel consumption

    fuel_changes_perc <- avg_fuel_consumption |>
        tidyr::pivot_longer(
            cols = c("avg_before_FTD", "avg_during_FTD", "avg_after_FTD"),
            names_to = "period",
            values_to = "fuel_consumption"
        ) |>
        dplyr::filter(period != "avg_after_FTD") |>
        tidyr::pivot_wider(
            names_from = "fuel",
            values_from = "fuel_consumption"
        ) |>
        dplyr::summarise(
            dplyr::across(
                .cols = c("diesel", "petrol"),
                ~ ((.x - dplyr::lag(.x)) / dplyr::lag(.x)) * 100
            )
        ) |>
        dplyr::filter(!is.na(diesel))

    # export
    openxlsx::write.xlsx(
        fuel_changes_perc,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "avg_fuel_consumption_perc.xlsx"
        ),
        rowNames = FALSE
    )
    
    #----------------------------------------------
    # plot levels

    # generate base plot
    base_plot_levels <- ggplot()+
        geom_rect(
            aes(
                xmin = 6,
                xmax = 8,
                ymin = -Inf,
                ymax = +Inf
            ),
            fill = "grey80",
            alpha = 0.4
        )+
        geom_text(
            aes(
                x = 7,
                y = 1300
            ),
            label = "FTD",
            size = 6
        )+
        geom_line(
            data = fuel_consumption |>
                dplyr::filter(year == 2022),
            aes(
                x = month,
                # divide by 1,000,000 to get million liters as unit
                y = fuel_consumption / 1000000,
                color = fuel
            ),
            linewidth = 1
        )+
        geom_point(
            data = fuel_consumption |>
                dplyr::filter(year == 2022),
            aes(
                x = month,
                # divide by 1,000,000 to get million liters as unit
                y = fuel_consumption / 1000000,
                color = fuel
            ),
            size = 2
        )+
        scale_x_continuous(
            breaks = seq(1, 12, 1),
            labels = c(
                "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
            )
        )+
        scale_y_continuous(
            breaks = seq(0, 3500, 500),
            labels = scales::comma
        )+
        scale_color_manual(
            values = c(
                "diesel" = config_globals()[["java_five_colors"]][1],
                "petrol" = config_globals()[["java_five_colors"]][3]
            ),
            labels = c(
                "diesel" = "Diesel",
                "petrol" = "Petrol"
            ),
            name = ""
        )+
        labs(
            x = "",
            y = "Fuel consumption (in million liters)"
        )+
        theme_classic()+
        theme(
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            legend.position = "bottom",
        )

    # add average lines
    levels_plot <- base_plot_levels+
        # petrol
        geom_segment(
            data = avg_fuel_consumption,
            y = avg_fuel_consumption$avg_before_FTD[avg_fuel_consumption$fuel == "petrol"] / 1000000,
            yend = avg_fuel_consumption$avg_before_FTD[avg_fuel_consumption$fuel == "petrol"] / 1000000,
            x = 1,
            xend = 6,
            linewidth = 0.8,
            linetype = "dashed",
            col = pal[3]
        )+
        geom_segment(
            data = avg_fuel_consumption,
            y = avg_fuel_consumption$avg_during_FTD[avg_fuel_consumption$fuel == "petrol"] / 1000000,
            yend = avg_fuel_consumption$avg_during_FTD[avg_fuel_consumption$fuel == "petrol"] / 1000000,
            x = 6,
            xend = 8,
            linewidth = 0.8,
            linetype = "dashed",
            col = pal[3]
        )+
        geom_segment(
            data = avg_fuel_consumption,
            y = avg_fuel_consumption$avg_after_FTD[avg_fuel_consumption$fuel == "petrol"] / 1000000,
            yend = avg_fuel_consumption$avg_after_FTD[avg_fuel_consumption$fuel == "petrol"] / 1000000,
            x = 8,
            xend = 12,
            linewidth = 0.8,
            linetype = "dashed",
            col = pal[3]
        )+
        # diesel
        geom_segment(
            data = avg_fuel_consumption,
            y = avg_fuel_consumption$avg_before_FTD[avg_fuel_consumption$fuel == "diesel"] / 1000000,
            yend = avg_fuel_consumption$avg_before_FTD[avg_fuel_consumption$fuel == "diesel"] / 1000000,
            x = 1,
            xend = 6,
            linewidth = 0.8,
            linetype = "dashed",
            col = pal[1]
        )+
        geom_segment(
            data = avg_fuel_consumption,
            y = avg_fuel_consumption$avg_during_FTD[avg_fuel_consumption$fuel == "diesel"] / 1000000,
            yend = avg_fuel_consumption$avg_during_FTD[avg_fuel_consumption$fuel == "diesel"] / 1000000,
            x = 6,
            xend = 8,
            linewidth = 0.8,
            linetype = "dashed",
            col = pal[1]
        )+
        geom_segment(
            data = avg_fuel_consumption,
            y = avg_fuel_consumption$avg_after_FTD[avg_fuel_consumption$fuel == "diesel"] / 1000000,
            yend = avg_fuel_consumption$avg_after_FTD[avg_fuel_consumption$fuel == "diesel"] / 1000000,
            x = 8,
            xend = 12,
            linewidth = 0.8,
            linetype = "dashed",
            col = pal[1]
        )+
        theme(
            legend.text = element_text(size = 17),
            axis.text = element_text(size = 17),
            axis.title = element_text(size = 17)
        )

    suppressMessages(ggsave(
        plot = levels_plot,
        file.path(
            config_paths()[["output_path"]],
            "graphs",
            "fuel_consumption_levels.png"
        ),
        dpi = config_globals()[["dpi"]]
    ))

    #--------------------------------------------------
    # return

    return(NULL)
}