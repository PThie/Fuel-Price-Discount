calculating_affected_population <- function(
    regional_effect_district = NA,
    microm_data_cleaned = NA
) {
    #' @title Calculating affected population
    #' 
    #' @description This function calculates the number of people receiving
    #' different pass-through rates (focus lies on working population).
    #' 
    #' @param regional_effect_district Estimated regional effects at the 
    #' district level
    #' @param microm_data_cleaned Cleaned microm data (RWI-GEO-GRID data)
    #' 
    #' @return Various summary statistics
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # combine population shares of working population

    griddata_district <- microm_data_cleaned |>
        dplyr::mutate(
            ags_district = substring(AGS, 1, 5),
            # combine people to working population share
            working_population_share = rowSums(
                microm_data_cleaned |>
                    dplyr::select(dplyr::contains("r1_eag_p_"))
            ) / 100,
            # calculate absolute working population
            working_population = people_total * working_population_share
        ) |>
        dplyr::select(
            -c(AGS, working_population_share, dplyr::contains("r1_eag_p_"))
        )

    #----------------------------------------------
    # summarise number of people on districts

    people_district <- griddata_district |>
        dplyr::group_by(ags_district) |>
        dplyr::summarise(
            people_total = sum(people_total, na.rm = TRUE),
            working_population = sum(working_population, na.rm = TRUE),
            total_purch_power = sum(purch_power, na.rm = TRUE),
            purch_power_pp = total_purch_power / people_total,
            avg_car_density = mean(car_density, na.rm = TRUE)
        )
    
    # merge to estimated effects
    regional_effects_people <- merge(
        regional_effect_district,
        people_district,
        by = "ags_district",
        all.x = TRUE
    )

    #----------------------------------------------
    # average passthrough across all regions

    avg_pass <- regional_effects_people |>
        dplyr::group_by(mod) |>
        dplyr::summarise(
            mean_pass = mean(passthrough, na.rm = TRUE)
        ) |>
        as.data.frame()
    
    # export
    openxlsx::write.xlsx(
        avg_pass,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "avg_pass_regions.xlsx"
        ),
        rowNames = FALSE
    )

    # calculate weighted mean of pass-through
    avg_pass_weighted <- regional_effects_people |>
        dplyr::group_by(mod) |>
        dplyr:::summarise(
            weighted_mean_pass = stats::weighted.mean(passthrough, w = working_population)
        ) |>
        as.data.frame()

    # export
    openxlsx::write.xlsx(
        avg_pass_weighted,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "weighted_avg_pass_regions.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # add pass-through breaks 

    br <- seq(
        plyr::round_any(min(regional_effects_people$passthrough), 10 , floor),
        plyr::round_any(max(regional_effects_people$passthrough), 10 , ceiling),
        10
    )

    regional_effects_people <- regional_effects_people |>
        dplyr::mutate(
            breaks = cut(passthrough, breaks = br)
        )
    
    #----------------------------------------------
    # count number of people per pass-through break

    total_people_passthrough <- regional_effects_people |>
        dplyr::group_by(breaks, mod) |>
        dplyr::summarise(
            working_population = sum(working_population)
        ) |>
        dplyr::arrange(mod) |>
        as.data.frame()

    # export
    openxlsx::write.xlsx(
        total_people_passthrough,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "people_passthrough_by_breaks.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # calculate average income and station density per pass-through break

    avg_income_station_density <- regional_effects_people |>
        dplyr::group_by(breaks) |>
        dplyr::summarise(
            avg_income = mean(purch_power_pp, na.rm = TRUE),
            avg_car_density = mean(avg_car_density, na.rm = TRUE)
        ) |>
        as.data.frame()

    # calculate overall averages
    overall_avg <- regional_effects_people |>
        dplyr::summarise(
            breaks = "Overall",
            avg_income = mean(purch_power_pp, na.rm = TRUE),
            avg_car_density = mean(avg_car_density, na.rm = TRUE)
        ) |>
        as.data.frame()

    # bind overall averages to avg_income_station_density
    avg_income_station_density <- rbind(avg_income_station_density, overall_avg)

    # export
    openxlsx::write.xlsx(
        avg_income_station_density,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "avg_income_station_density_by_breaks.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # calculate correlation between income and station density at district level

    corr_income_station_density <- cor(
        people_district$purch_power_pp,
        people_district$avg_car_density
    )

    # export
    openxlsx::write.xlsx(
        corr_income_station_density,
        file.path(
            config_paths()[["output_path"]],
            "descriptives",
            "correlation_income_station_density_districts.xlsx"
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(NULL)
}