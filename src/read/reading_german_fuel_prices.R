reading_german_fuel_prices <- function(data_file_path = NA) {
    #' @title Reading German fuel prices
    #' 
    #' @description This function reads the original fuel price data for
    #' Germany.
    #' 
    #' @param data_file_path Path to original data file
    #' 
    #' @return Dataframe
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # reading data

    dta <- fst::read_fst(data_file_path)

    #--------------------------------------------------
    # return

    return(dta)
}