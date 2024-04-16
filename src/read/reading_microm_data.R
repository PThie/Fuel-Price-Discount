reading_microm_data <- function(
    data_file_path = NA
) {
    #' @title Reading RWI-GEO-GRID (microm) data
    #' 
    #' @description This function reads the RWI-GEO-GRID (microm) data.
    #' The data is given at a resolution of 1 sqm. kilometer.
    #' 
    #' @param data_file_path Path to the RWI-GEO-GRID (microm) datas
    #' 
    #' @return Dataframe with clean RWI-GEO-GRID (microm) data
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read data

    griddata <- haven::read_dta(
        data_file_path
    )

    #--------------------------------------------------
    # return
    
    return(griddata)
}