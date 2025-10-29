calculating_average_event_study_effect <- function(
    dependency = NA,
    suffix_export = NA
) {
    #' @title Calculating average event study effect
    #' 
    #' @description This function calculates the average event study effect
    #' from estimation results.
    #' 
    #' @param dependency Dependency target with event study estimation results
    #' @param suffix_export Suffix for export files
    #' 
    #' @return Data frame with average effects and passthrough rates
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check dependency

    targets::tar_assert_nonempty(
        dependency,
        msg = "Dependency for calculating average event study effect is missing."
    )

    #--------------------------------------------------
    # read event study results

    vars <- c("diesel", "e10")
    effects_list <- list()
    for (var in vars) {
        filename <- paste0(
            "did_est_Germany_",
            var,
            "_",
            suffix_export,
            ".xlsx"
        )

        dta <- openxlsx::read.xlsx(
            file.path(
                config_paths()[["output_path"]],
                "estimation",
                filename
            )
        )

        effects_list[[var]] <- dta
    }

    #--------------------------------------------------
    # calculate average effects

    avg_effects <- as.data.frame(cbind(
        variable = character(),
        avg_effect = numeric(),
        passthrough = numeric()
    ))

    for (var in names(effects_list)) {
        dta <- effects_list[[var]]

        # define FTD
        if (var == "diesel") {
            FTD <- -0.1671
        } else if (var == "e10") {
            FTD <- -0.3516
        }

        # filter for days in post period
        # add passthrough calculation
        dta_prep <- dta |>
            dplyr::filter(
                time >= 0
            ) |>
            dplyr::mutate(
                passthrough = (coefficient / FTD) * 100
            )

        # export
        openxlsx::write.xlsx(
            dta_prep,
            file.path(
                config_paths()[["output_path"]],
                "estimation",
                paste0(
                    "passthrough_event_study_",
                    var,
                    "_",
                    suffix_export,
                    "_post_period.xlsx"
                )
            )
        )


        # calculate average effect
        avg_effect <- mean(dta_prep$coefficient)
        avg_passthrough <- mean(dta_prep$passthrough)

        # store
        avg_effects <- rbind(
            avg_effects,
            data.frame(
                variable = var,
                avg_effect = avg_effect,
                passthrough = avg_passthrough
            )
        )
    }

    #--------------------------------------------------
    # export average effects

    openxlsx::write.xlsx(
        avg_effects,
        file.path(
            config_paths()[["output_path"]],
            "estimation",
            paste0(
                "average_event_study_effects_",
                suffix_export,
                ".xlsx"
            )
        )
    )
    
    #--------------------------------------------------
    # return

    return(avg_effects)

}