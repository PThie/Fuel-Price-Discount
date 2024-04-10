joining_germany_france <- function(
    german_fuel_prices = NA,
    french_fuel_prices = NA
){
    #' @title Combine German and French fuel price data
    #' 
    #' @description This function combines the German and French fuel price data
    #' to one dataframe for further analysis.
    #' 
    #' @param german_fuel_prices Station-level fuel price for Germany
    #' @param french_fuel_prices Station-level fuel price for France
    #' 
    #' @return Dataframe with combined price information for both countries
    #' @author Patrick Thiel

    #----------------------------------------------
    # clean German data

    # make Date variable and add country specifier
    german_fuel_prices <- german_fuel_prices |>
        dplyr::mutate(
            date = as.Date(date, format = "%Y-%m-%d"),
            country = "DE"
        )

    # rename station
    names(german_fuel_prices)[names(german_fuel_prices) == "id"] <- "station_id"

    #----------------------------------------------
    # clean French data

    # average across date
    avg_prices_fr <- french_fuel_prices |>
        dplyr::group_by(stationID, date, type) |>
        dplyr::summarise(
            price = mean(price, na.rm = TRUE)
        )

    # reshape French data
    avg_prices_fr_long <- tidyr::pivot_wider(
        avg_prices_fr,
        values_from = price,
        names_from = type
    ) |>
        as.data.frame()

    # drop unnessary columns
    avg_prices_fr_long <- avg_prices_fr_long |>
        dplyr::select(
            c("stationID", "date", "Gazole", "SP95", "E10")
        )

    # rename columns
    colnames(avg_prices_fr_long) <- c(
        "station_id", "date", "diesel", "e5", "e10"
    )

    # transform date and add country specifier
    avg_prices_fr_long <- avg_prices_fr_long |>
        dplyr::mutate(
            date = as.Date(date, format = "%Y-%m-%d"),
            country = "FR"
        )

    # reorder according to German data
    avg_prices_fr_long <- avg_prices_fr_long |>
        dplyr::select(names(german_fuel_prices))

    #----------------------------------------------
    # bring Germany and France together (by date)

    avg_prices <- rbind(
        german_fuel_prices,
        avg_prices_fr_long
    )

    #----------------------------------------------
    # add treatment variables (for time and groups)

    avg_prices <- avg_prices |>
        dplyr::mutate(
            date = as.Date(date, "%Y-%m-%d"),
            treat_tankrabatt_de = dplyr::case_when(
                date < config_globals()[["start_tr_de"]] ~ "control",
                date > config_globals()[["end_tr_de"]] ~ "control",
                date >= config_globals()[["start_tr_de"]] ~ "treated",
            ),
            treat_region_de = dplyr::case_when(
                country == "DE" ~ "treated",
                country != "DE" ~ "control"
            )
        )

    #----------------------------------------------
    # do some cleaning on prices
    # keep prices between 0.50 EUR and 3 EUR
    # NOTE: see globals

    avg_prices_cleaned <- avg_prices |>
        dplyr::mutate(
            diesel = replace(
                diesel,
                diesel < config_globals()[["price_low_threshold"]] | diesel > config_globals()[["price_high_threshold"]],
                NA
            ),
            e10 = replace(
                e10,
                e10 < config_globals()[["price_low_threshold"]] | e10 > config_globals()[["price_high_threshold"]],
                NA
            ),
            e5 = replace(
                e5,
                e5 < config_globals()[["price_low_threshold"]] | e5 > config_globals()[["price_high_threshold"]],
                NA
            )
        )

    #----------------------------------------------
    # return

    return(avg_prices_cleaned)
}
