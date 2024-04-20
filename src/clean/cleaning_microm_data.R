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
        dplyr::select(
            r1_id, r1_mpi_w_dichte, r1_kkr_w_summe, r1_ewa_a_gesamt,
            r1_mba_a_haushalt,
            r1_eag_p_m15bis18, r1_eag_p_w15bis18,
            r1_eag_p_m18bis20, r1_eag_p_w18bis20,
            r1_eag_p_m20bis25, r1_eag_p_w20bis25, 
            r1_eag_p_m25bis30, r1_eag_p_w25bis30, 
            r1_eag_p_m30bis35, r1_eag_p_w30bis35,
            r1_eag_p_m35bis40, r1_eag_p_w35bis40,
            r1_eag_p_m40bis45, r1_eag_p_w40bis45,
            r1_eag_p_m45bis50, r1_eag_p_w45bis50,
            r1_eag_p_m50bis55, r1_eag_p_w50bis55,
            r1_eag_p_m55bis60, r1_eag_p_w55bis60, 
            r1_eag_p_m60bis65, r1_eag_p_w60bis65
        ) |>
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
            people_total = r1_ewa_a_gesamt,
            r1_mba_a_haushalt = num_households
        )

    #--------------------------------------------------
    # return

    return(griddata_municipalities)
}