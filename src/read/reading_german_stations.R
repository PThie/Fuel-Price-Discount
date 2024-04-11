reading_german_stations <- function(data_file_path = NA) {
    #' @title Reading German stations
    #' 
    #' @description This function reads the German stations data and transfroms
    #' it into geo data.
    #' 
    #' @param data_file_path Path to the data file
    #' 
    #' @return Return geo information of German stations
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read data

    stations <- fst::read.fst(data_file_path)

    #--------------------------------------------------
    # return

    return(stations)
}