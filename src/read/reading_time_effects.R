reading_time_effects <- function(
    data_file_path = NA,
    fuel_type = NA
) {
    #' @title Reading estimated time effects
    #' 
    #' @description This function reads the estimated time effects from the
    #' output of the time effects estimation.
    #' 
    #' @param data_file_path The path to the data file.
    #' @param fuel_type The fuel type of the data (diesel or petrol).
    #' 
    #' @return A data frame with the estimated time effects.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read data

    dta <- openxlsx::read.xlsx(data_file_path)

    #--------------------------------------------------
    # cleaning

    # add date column
    dta$date <- seq(
        lubridate::ymd("2022-04-01"),
        lubridate::ymd("2022-08-31"),
        by = "1 day"
    )
    dta$date <- as.character(dta$date)

    # add passthrough values
    if (fuel_type == "e10") {
        passthrough_threshold <- -0.3516
    } else {
        passthrough_threshold <- -0.1671
    }

    dta <- dta |>
        dplyr::mutate(
            passthrough = (coefficient / passthrough_threshold) * 100
        ) |>
        dplyr::arrange(time)

    #--------------------------------------------------
    # return

    return(dta)
}