reading_french_fuel_prices <- function(data_file_path = NA) {
    #' @title Reading French fuel prices
    #' 
    #' @description This function reads the original fuel price data for
    #' France.
    #' 
    #' @param data_file_path Path to original data file
    #' 
    #' @return Dataframe
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # reading data

    dta <- arrow::read_feather(data_file_path)

    # drop Address and city
    dta <- dta |>
        dplyr::select(-c(address, city))

    #--------------------------------------------------
    # return

    return(dta)
}