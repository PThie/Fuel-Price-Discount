reading_french_regions <- function(
    data_file_path = NA,
    region_name_id = NA
) {
    #' @title Reading French regions
    #' 
    #' @description This function reads the French regions from the shapefile.
    #' 
    #' @param data_file_path Path to the shapefile
    #' @param region_name_id Name of the regional level
    #' 
    #' @return Dataframe with French regions
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read data

    regions <- sf::st_read(
        data_file_path,
        quiet = TRUE
    ) |>
    sf::st_transform(config_globals()[["utmcrs"]])

    #--------------------------------------------------
    # clean regions

    if (region_name_id == "region") {
        regions <- regions |>
            dplyr::select(running_id_region = ID_1, region_name = NAME_1)
    } else if (region_name_id == "department") {
        regions <- regions |>
            dplyr::select(running_id_region = code_insee, region_name = nom)
    } else {
        regions <- regions |>
            dplyr::select(running_id_region = insee, region_name = nom)
    }

    # adjust names
    regions <- regions |>
        dplyr::mutate(
            region_name = stringi::stri_trans_general(
                region_name,
                "de-ASCII; Latin-ASCII"
            )   
        )

    #--------------------------------------------------
    # return

    return(regions)
}

