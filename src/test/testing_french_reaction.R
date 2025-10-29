testing_french_reaction <- function(
    price_data = NA,
    french_stations = NA
) {
    #' @title Testing French reaction
    #' 
    #' @desciption This function checks the price development at French stations
    #' depending on how far they are from the German border.
    #' 
    #' @param price_data Fuel price data for April to August 2022
    #' @param french_stations French station information
    #' 
    #' @return Returns graphs
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # country shape files

    country_names <- grep(
        list.files(
            file.path(
                config_paths()[["data_path"]],
                "country_shapes"
            )
        ),
        invert = TRUE,
        pattern = "\\.",
        value = TRUE
    )

    country_names <- sub("*_shapefile", "", country_names)
    country_names <- country_names[country_names %in% c("Germany", "France")]

    country_shapes = list()
    # loop through countries
    for(country_name in country_names) {
        # define path
        shp_file <- list.files(
            file.path(
                config_paths()[["data_path"]],
                "country_shapes",
                paste0(country_name, "_shapefile")
            ),
            pattern = ".shp$",
            full.names = TRUE
        )

        # read data
        dt <- sf::st_read(
            shp_file,
            quiet = TRUE
        ) |>
        dplyr::select(geometry) |>
        dplyr::mutate(name = country_name)

        # transform data
        dt <- sf::st_transform(dt, crs = config_globals()[["utmcrs"]])

        country_shapes[[country_name]] <- dt
    }

    #----------------------------------------------
    # buffer around countries and find intersection

    buffers <- c(10000, 25000, 50000)
    buffered_countries <- list()
    intersected_countries <- list()

    for(buffer in buffers) {
        # buffer around countries
        for(country in names(country_shapes)) {
            # subset data
            dta <- country_shapes[[country]]

            # buffer data
            dta_buffered <- sf::st_buffer(
                dta,
                dist = buffer
            )

            # store
            file <- paste0(country, "_", buffer)
            buffered_countries[[file]] <- dta_buffered
        }

        # select both countries for specific buffer
        dta_list <- buffered_countries[
            stringr::str_detect(
                names(buffered_countries),
                as.character(buffer)
            ) == TRUE
        ]

        dta1 <- data.table::rbindlist(dta_list[1])
        dta1 <- sf::st_set_geometry(dta1, dta1$geometry)

        dta2 <- data.table::rbindlist(dta_list[2])      
        dta2 <- sf::st_set_geometry(dta2, dta2$geometry)

        # find intersection of both countries for specific buffer
        intsect <- sf::st_intersection(
            dta1,
            dta2
        ) |>
        dplyr::mutate(
            buffer_dist = buffer
        ) |>
        dplyr::rename(
            country_x = name,
            country_y = name.1
        )

        # store
        file <- paste0("int_", buffer)
        intersected_countries[[file]] <- intsect
    }

    #----------------------------------------------
    # intersect stations with buffered border

    for(buffer in buffers) {
        # subset data
        dta_list <- intersected_countries[
            stringr::str_detect(
                names(intersected_countries),
                as.character(buffer)
            ) == TRUE
        ]

        # tranform into data frame
        dta <- data.table::rbindlist(dta_list)
        dta <- sf::st_set_geometry(dta, dta$geometry)

        # intersect stations with buffered border
        int_values <- sf::st_intersects(
            french_stations,
            dta
        )
        int_values <- lengths(int_values)
        int_values[int_values > 1] <- 1
        
        # add to station data
        var <- rlang::sym(paste0("within_", buffer))
        french_stations <- french_stations |>
            dplyr::mutate(
                !!var := int_values
            )
    }

    # make stations exclusively belonging to one buffer
    french_stations <- french_stations |>
        dplyr::mutate(
            within_25000 = dplyr::case_when(
                within_10000 == 1 ~ 0,
                TRUE ~ within_25000
            ),
            within_50000 = dplyr::case_when(
                within_10000 == 1 ~ 0,
                within_25000 == 1 ~ 0,
                TRUE ~ within_50000
            )
        )

    #----------------------------------------------
    # map stations

    # get country shapes
    france <- data.table::rbindlist(country_shapes["France"])
    france <- sf::st_set_geometry(france, france$geometry)

    germany <- data.table::rbindlist(country_shapes["Germany"])
    germany <- sf::st_set_geometry(germany, germany$geometry)

    # for plotting only keep buffered area within France
    france_buffered <- list()
    for(nam in names(intersected_countries)) {
        dta <- intersected_countries[[nam]]

        intsect <- sf::st_intersection(
            dta,
            france
        )
        france_buffered[[nam]] <- intsect
    }

    setgeo <- function(buffer) {
        dta <- france_buffered[buffer]
        dta <- data.table::rbindlist(dta)
        dta <- sf::st_set_geometry(dta, dta$geometry)
        return(dta)
    }

    buff1 <- setgeo(buffer = "int_10000")
    buff2 <- setgeo(buffer = "int_25000")
    buff3 <- setgeo(buffer = "int_50000")
    
    # create map
    map_border <- ggplot()+
        geom_sf(
            france,
            mapping = aes(geometry = geometry),
            fill = NA,
            col = "black",
            linewidth = 0.8
        )+
        geom_sf(
            germany,
            mapping = aes(geometry = geometry),
            fill = NA,
            col = "black",
            linewidth = 0.8
        )+
        geom_sf(
            buff1,
            mapping = aes(geometry = geometry),
            fill = ggplot2::alpha("black", 0.1)
        )+
        geom_sf(
            buff2,
            mapping = aes(geometry = geometry),
            fill = ggplot2::alpha("black", 0.2)
        )+
        geom_sf(
            buff3,
            mapping = aes(geometry = geometry),
            fill = ggplot2::alpha("black", 0.3)
        )+
        geom_sf(
            french_stations |> dplyr::filter(within_10000 == 1),
            mapping = aes(geometry = geometry),
            shape = 16,
            col = "darkorange"
        )+
        geom_sf(
            french_stations |> dplyr::filter(within_25000 == 1),
            mapping = aes(geometry = geometry),
            shape = 17,
            col = "darkblue"
        )+
        geom_sf(
            french_stations |> dplyr::filter(within_50000 == 1),
            mapping = aes(geometry = geometry),
            shape = 18,
            col = "darkgreen"
        )+
        geom_text(
            mapping = aes(
                x = 200000,
                y = 5400000,
                label = "France"
            ),
            size = 5
        )+
        geom_text(
            mapping = aes(
                x = 500000,
                y = 5450000,
                label = "Germany"
            ),
            size = 5
        )+
        coord_sf(xlim = c(100000, 700000), ylim = c(5100000, 5600000))+
        theme_void()

    ggsave(
        plot = map_border,
        file.path(
            config_paths()[["output_path"]],
            "maps",
            "french_border_stations.png"
        ),
        dpi = config_globals()[["owndpi"]]
    )

    #----------------------------------------------
    # merge station data to price data

    french_prices <- merge(
        price_data |>
            dplyr::filter(country == "FR"),
        french_stations |>
            sf::st_drop_geometry(),
        by = "station_id"
    )

    # calculate overall daily average (national)
    avg_overall <- french_prices |>
        dplyr::group_by(date) |>
        dplyr::summarise(
            diesel = mean(diesel, na.rm = TRUE),
            e10 = mean(e10, na.rm = TRUE)
        ) |>
        dplyr::mutate(
            date = as.Date(date, "%Y-%m-%d")
        ) |>
        tidyr::pivot_longer(
            !date,
            names_to = "fuel_type",
            values_to = "overall"
        ) |>
        as.data.frame()

    # calculate daily averages for each buffer
    avgprices <- function(groupvar) {
        avg <- french_prices |>
            dplyr::filter(!!rlang::sym(groupvar) == 1) |>
            dplyr::group_by(date) |>
            dplyr::summarise(
                diesel = mean(diesel, na.rm = TRUE),
                e10 = mean(e10, na.rm = TRUE)
            ) |>
            dplyr::mutate(
                date = as.Date(date, "%Y-%m-%d")
            ) |>
            tidyr::pivot_longer(
                !date,
                names_to = "fuel_type",
                values_to = groupvar
            ) |>
            as.data.frame()
        return(avg)
    }

    avg_int10000 <- avgprices(groupvar = "within_10000")
    avg_int25000 <- avgprices(groupvar = "within_25000")
    avg_int50000 <- avgprices(groupvar = "within_50000")

    # merge all
    aux1 <- merge(
        avg_overall,
        avg_int10000,
        by = c("date", "fuel_type")
    )

    aux2 <- merge(
        avg_int25000,
        avg_int50000,
        by = c("date", "fuel_type")
    )

    daily_french_prices <- merge(
        aux1,
        aux2,
        by = c("date", "fuel_type")
    )

    # plot different fuel types
    fueltypes <- unique(daily_french_prices$fuel_type)

    for(fueltype in fueltypes) {
        # position of FTD label
        if(fueltype == "diesel") {
            yanchor = 1.72
        } else {
            yanchor = 1.72
        }
        # plot
        french_plot <- ggplot()+
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
                data = daily_french_prices |>
                    dplyr::filter(fuel_type == fueltype),
                mapping = aes(x = as.Date(date), y = overall, col = "overall"),
                linetype = "dashed",
                linewidth = 0.8
            )+
            geom_line(
                data = daily_french_prices |>
                    dplyr::filter(fuel_type == fueltype),
                mapping = aes(x = as.Date(date), y = within_10000, col = "within_10000"),
                linewidth = 0.8
            )+
            geom_line(
                data = daily_french_prices |>
                    dplyr::filter(fuel_type == fueltype),
                mapping = aes(x = as.Date(date), y = within_25000, col = "within_25000"),
                linewidth = 0.8
            )+
            geom_line(
                data = daily_french_prices |>
                    dplyr::filter(fuel_type == fueltype),
                mapping = aes(x = as.Date(date), y = within_50000, col = "within_50000"),
                linewidth = 0.8
            )+
            scale_color_manual(
                values = c(
                    "overall" = "black",
                    "within_10000" = config_globals()[["java_five_colors"]][2],
                    "within_25000" = config_globals()[["java_five_colors"]][3],
                    "within_50000" = config_globals()[["java_five_colors"]][5]
                ),
                labels = c(
                    "overall" = "All stations",
                    "within_10000" = "Stations within 10km",
                    "within_25000" = "Stations within 25km",
                    "within_50000" = "Stations within 50km"
                ),
                name = ""
            )+
            scale_y_continuous(
                breaks = seq(1.7, 2.2, 0.1),
                limits = c(1.68, 2.22)
            )+
            scale_x_date(
                date_breaks = "1 month",
                date_minor_breaks = "1 week",
                date_labels = "%b"
            )+
            geom_text(
                data = daily_french_prices,
                x = as.Date("2022-07-01", format = "%Y-%m-%d"),
                y = yanchor,
                label = "Fuel tax discount",
                size = 6.5
            )+
            labs(
                x = "",
                y = "Price in EUR/liter"
            )+
            theme_classic()+
            theme(
                panel.border = element_rect(linewidth = 1, fill = NA),
                axis.text = element_text(size = 20),
                axis.title = element_text(size = 20),
                legend.key.size = unit(1, "cm"),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 18),
                legend.position = "bottom"
            )+
            guides(col = guide_legend(nrow = 2,byrow = TRUE))

        filename <- paste0(fueltype, "_french_prices_border.png")
        ggsave(
            plot = french_plot,
            file.path(
                config_paths()[["output_path"]],
                "graphs",
                filename
            ),
            dpi = config_globals()[["owndpi"]]
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}
