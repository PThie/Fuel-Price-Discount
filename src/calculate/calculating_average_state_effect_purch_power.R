calculating_average_state_effect_purch_power <- function(
    state_effects = NA
) {
    #' @title Calculate average state effects for purchasing power
    #' 
    #' @description This function calculates the average state effects for
    #' purchasing power based on the state effects data. Needed for calculating
    #' the impact of the FTD.
    #' 
    #' @param state_effects Data frame with state effects.
    #' 
    #' @return Data frame with average state effects for purchasing power.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # prepare data

    # combine all state effects into one data frame
    all_state_effects <- data.table::rbindlist(state_effects)

    # separate below and above median income groups
    below_median_income <- all_state_effects |>
        dplyr::filter(as.numeric(pp_cat) <= 5)

    above_median_income <- all_state_effects |>
        dplyr::filter(as.numeric(pp_cat) > 5)

    #--------------------------------------------------
    # calculate average effect by fuel type

    avg_effect_below_median <- below_median_income |>
        dplyr::group_by(depvar_id) |>
        dplyr::summarise(avg_effect = mean(estimate, na.rm = TRUE)) |>
        dplyr::mutate(pp_cat = "below_median")

    avg_effect_above_median <- above_median_income |>
        dplyr::group_by(depvar_id) |>
        dplyr::summarise(avg_effect = mean(estimate, na.rm = TRUE)) |>
        dplyr::mutate(pp_cat = "above_median")

    avg_effect <- dplyr::bind_rows(
        avg_effect_below_median,
        avg_effect_above_median
    )

    #--------------------------------------------------
    # export

    openxlsx::write.xlsx(
        avg_effect,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "average_state_effects_purch_power.xlsx"
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(avg_effect)
}