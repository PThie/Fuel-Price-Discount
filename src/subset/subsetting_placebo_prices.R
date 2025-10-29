subsetting_placebo_prices <- function(
    price_data = NA
) {
    #' @title Subsetting fuel prices to placebo period
    #' 
    #' @description This function subsets the fuel prices to the analysis time
    #' period April to August for different years.
    #' 
    #' @param price_data Fuel price data.
    #' 
    #' @return List with fuel prices April to August.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # extract new periods of intervention

    extracting_data <- function(
        start_date = NA,
        end_date = NA,
        placebo_date = NA
    ) {
        #--------------------------------------------------
        # restrict to April to August 2022

        price_data_subset <- price_data |>
            dplyr::filter(date >= start_date) |>
            dplyr::filter(date <= end_date)

        #--------------------------------------------------
        # add month variable

        price_data_subset <- price_data_subset |>
            dplyr::mutate(
                months = format(date, "%Y-%m")
            )

        #--------------------------------------------------
        # imput days for France

        price_data_prep <- imputing_data(
            price_data = price_data_subset
        )

        #--------------------------------------------------
        # define treatment time

        price_data_prep <- price_data_prep |>
            dplyr::mutate(
                treat_tankrabatt_de = dplyr::case_when(
                    date < placebo_date ~ "control",
                    date >= placebo_date ~ "treated"
                )
            )

        #--------------------------------------------------
        # define time to treatment

        price_data_prep <- price_data_prep |>
            dplyr::mutate(
                time_to_treatment = as.numeric(difftime(
                    as.Date(date, "%Y-%m-%d"),
                    placebo_date,
                    units = "days"
                )),
                # set French stations to never-treated (in terms of time)
                time_to_treatment = dplyr::case_when(
                    treat_region_de == "control" ~ 999,
                    TRUE ~ time_to_treatment
                )
            )

        #--------------------------------------------------
        # return

        return(price_data_prep)
    }

    data_2021 <- extracting_data(
        start_date = as.Date("2021-04-01"),
        end_date = as.Date("2021-08-31"),
        placebo_date = as.Date("2021-06-01")
    )

    data_2020 <- extracting_data(
        start_date = as.Date("2020-04-01"),
        end_date = as.Date("2020-08-31"),
        placebo_date = as.Date("2020-06-01")
    )

    data_2019 <- extracting_data(
        start_date = as.Date("2019-04-01"),
        end_date = as.Date("2019-08-31"),
        placebo_date = as.Date("2019-06-01")
    )

    # combine all
    all_data_list <- list(
        "2021" = data_2021,
        "2020" = data_2020,
        "2019" = data_2019
    )

    #--------------------------------------------------
    # return

    return(all_data_list)
}