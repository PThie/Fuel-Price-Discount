reading_european_fuel_prices <- function(data_file_path = NA) {
    #' @title Reading European fuel prices
    #' 
    #' @description This function reads the European fuel prices data.
    #' 
    #' @param data_file_path Path to the data file
    #' 
    #' @return Returns the European fuel prices data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # reading data

    european_prices <- haven::read_dta(
        data_file_path
    )

    #----------------------------------------------
    # cleaning

    # adjust names by removing unnecessary underscore
    nam <- european_prices |>
        names() |>
        stringr::str_replace("_", "")
        
    colnames(european_prices) <- nam

    # drop missing day information
    # since data is only weekly
    # rename date
    european_prices_prep <- european_prices |>
        dplyr::filter(!is.na(gastax)) |>
        dplyr::rename(date = date1)

    #--------------------------------------------------
    # return

    return(european_prices)
}