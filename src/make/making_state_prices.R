making_state_prices <- function(
    price_data = NA,
    german_stations = NA
) {
    #' @title Add state identifier
    #' 
    #' @description This function add state identifier.
    #' 
    #' @param price_data Fuel price data for April to August 2022
    #' @param german_stations Station data for Germany
    #' 
    #' @return Price data with state information
    #' @author Patrick Thiel

    #--------------------------------------------------
    # prepare station data

    german_stations <- german_stations |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
            state_id = substring(AGS_district, 1, 2)
        )

    #--------------------------------------------------
    # combine with price data

    prices_prep <- price_data |>
        merge(
            german_stations,
            by = "station_id",
            all.x = TRUE
        )

    #--------------------------------------------------
    # add state names

    prices_prep <- prices_prep |>
        dplyr::mutate(
            state_name = dplyr::case_when(
                state_id == "01" ~ "Schleswig-Holstein",
                state_id == "02" ~ "Hamburg",
                state_id == "03" ~ "Niedersachsen",
                state_id == "04" ~ "Bremen",
                state_id == "05" ~ "Nordrhein-Westfalen",
                state_id == "06" ~ "Hessen",
                state_id == "07" ~ "Rheinland-Pfalz",
                state_id == "08" ~ "Baden-Wuerttemberg",
                state_id == "09" ~ "Bayern",
                state_id == "10" ~ "Saarland",
                state_id == "11" ~ "Berlin",
                state_id == "12" ~ "Brandenburg",
                state_id == "13" ~ "Mecklenburg-Vorpommern",
                state_id == "14" ~ "Sachsen",
                state_id == "15" ~ "Sachsen-Anhalt",
                state_id == "16" ~ "Thüringen"
            ),
            state_name_short = dplyr::case_when(
                state_id == "01" ~ "SH",
                state_id == "02" ~ "HH",
                state_id == "03" ~ "NI",
                state_id == "04" ~ "HB",
                state_id == "05" ~ "NW",
                state_id == "06" ~ "HE",
                state_id == "07" ~ "RP",
                state_id == "08" ~ "BW",
                state_id == "09" ~ "BY",
                state_id == "10" ~ "SL",
                state_id == "11" ~ "BE",
                state_id == "12" ~ "BB",
                state_id == "13" ~ "MV",
                state_id == "14" ~ "SN",
                state_id == "15" ~ "ST",
                state_id == "16" ~ "TH"
            )
        )

    #--------------------------------------------------
    # return

    return(prices_prep)
}