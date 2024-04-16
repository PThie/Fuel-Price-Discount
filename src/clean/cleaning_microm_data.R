cleaning_microm_data <- function(
    microm_data_raw = NA,
    grid_municipalities = NA
) {
    #' @title Cleaning microm data
    #' 
    #' @description This function cleans the microm data (RWI-GEO-GRID).
    #' 
    #' @param microm_data_raw Raw microm data.
    #' @param grid_municipalities Connection between grid and municipalities.
    #' 
    #' @return Cleaned microm data.
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # clean grid data
    # bring it on municipality level

    griddata_prep <- microm_data_raw |>
        dplyr::filter(year == 2020) |>
        dplyr::select(r1_id, r1_mpi_w_dichte, r1_kkr_w_summe, r1_ewa_a_gesamt) |>
        as.data.frame()

    # join municipality info
    griddata_municipalities <- merge(
        griddata_prep,
        grid_municipalities,
        by = "r1_id",
        all.x = TRUE
    )

    # clean
    griddata_municipalities <- griddata_municipalities |>
        dplyr::rename(
            car_density = r1_mpi_w_dichte,
            purch_power = r1_kkr_w_summe,
            people_total = r1_ewa_a_gesamt
        )

    #--------------------------------------------------
    # return

    return(griddata_municipalities)
}