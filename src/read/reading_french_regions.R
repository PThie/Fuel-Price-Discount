reading_french_regions <- function(
    data_file_path = NA
) {
    #' @title Reading French regions
    #' 
    #' @description This function reads the French regions from the shapefile.
    #' 
    #' @param data_file_path Path to the shapefile
    #' 
    #' @return Dataframe with French regions
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read data

    regions <- sf::st_read(
        data_file_path,
        quiet = TRUE
    )

    #--------------------------------------------------
    # clean regions

    regions <- regions |>
        dplyr::select(running_id = ID_1, region_name = NAME_1) |>
        dplyr:::mutate(
            region_name = stringi::stri_trans_general(
                region_name,
                "de-ASCII; Latin-ASCII"
            )   
        )

    #--------------------------------------------------
    # return

    return(regions)
}

